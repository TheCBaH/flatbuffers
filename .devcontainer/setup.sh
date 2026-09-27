#!/bin/bash
# Provisions the current machine (e.g. a Claude Code cloud environment) the
# same way the devcontainer does, without Docker: it replays the steps
# declared in .devcontainer/ directly on the host.
#
#   1. apt packages from the Dockerfile's `apt-get install` list
#   2. every feature in devcontainer.json, by running its install.sh with the
#      options exported as environment variables, as the devcontainer CLI does
#   3. every postCreateCommand
#
# Keeping devcontainer.json the single source of truth means a new package or
# option there reaches this environment without editing this script.
#
# Must run as root. Reruns are cheap: each feature is stamped with a hash of
# its resolved options and skipped when nothing changed.
#
# Environment:
#   OCAML_VERSION        consumed by devcontainer.json via ${localEnv:...}
#   SETUP_SKIP_FEATURES  feature ids to skip (default: common-utils, which only
#                        creates the vscode user and shell niceties)
#   SETUP_SKIP_PACKAGES  apt packages to skip (default: nodejs npm when a node
#                        binary is already on PATH)
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/.." && pwd)
cache=${SETUP_CACHE_DIR:-/var/cache/devcontainer-setup}
stamps=${SETUP_STAMP_DIR:-/var/lib/devcontainer-setup}
skip_features=${SETUP_SKIP_FEATURES-common-utils}
if [ -z "${SETUP_SKIP_PACKAGES+x}" ] && command -v node >/dev/null 2>&1; then
    SETUP_SKIP_PACKAGES="nodejs npm"
fi
skip_packages=${SETUP_SKIP_PACKAGES:-}

log() { printf '\n==> %s\n' "$*"; }

if [ "$(id -u)" != 0 ]; then
    echo "setup.sh must run as root" >&2
    exit 1
fi

export DEBIAN_FRONTEND=noninteractive
if ! command -v jq >/dev/null 2>&1; then
    apt-get update -y
    apt-get install -y --no-install-recommends jq
fi

# devcontainer.json is JSONC; drop whole-line comments so jq can read it.
config=$(sed -e 's#^[[:space:]]*//.*$##' "$here/devcontainer.json")

# Expands ${localEnv:NAME} and ${localEnv:NAME:default} like the CLI does.
expand_local_env() {
    local s=$1 out="" var name def
    while [[ $s =~ ^(.*)\$\{localEnv:([A-Za-z_][A-Za-z0-9_]*)(:([^}]*))?\}(.*)$ ]]; do
        name=${BASH_REMATCH[2]}
        def=${BASH_REMATCH[4]}
        var=${!name-$def}
        out="$var${BASH_REMATCH[5]}$out"
        s=${BASH_REMATCH[1]}
    done
    printf '%s' "$s$out"
}

# Option id -> environment variable name, per the devcontainer features spec.
option_env_name() {
    printf '%s' "$1" | sed -e 's/[^A-Za-z0-9_]/_/g' -e 's/^[0-9_]*/_/;s/^_\([A-Za-z]\)/\1/' |
        tr '[:lower:]' '[:upper:]'
}

# --- 1. Dockerfile packages -------------------------------------------------
log "apt packages from Dockerfile"
mapfile -t packages < <(awk '
    /--no-install-recommends/ { collect = 1; next }
    collect && /^[[:space:]]*;/ { collect = 0 }
    collect && /^[[:space:]]+[a-z0-9][a-z0-9.+-]*[[:space:]]*(\\)?$/ {
        gsub(/[[:space:]\\]/, ""); print
    }' "$here/Dockerfile")
wanted=()
for p in "${packages[@]}"; do
    case " $skip_packages " in *" $p "*) continue ;; esac
    wanted+=("$p")
done
missing=()
for p in "${wanted[@]}"; do
    dpkg -s "$p" >/dev/null 2>&1 || missing+=("$p")
done
if [ "${#missing[@]}" -gt 0 ]; then
    apt-get update -y || true
    apt-get install -y --no-install-recommends "${missing[@]}"
else
    echo "all present: ${wanted[*]}"
fi

# --- 2. Features ------------------------------------------------------------
# Fetches a published feature into $cache/<name> and prints that directory.
# Tries the OCI artifact first; when the registry blob host is unreachable it
# falls back to the feature-template source layout on GitHub
# (ghcr.io/OWNER/REPO/ID -> github.com/OWNER/REPO, src/ID).
fetch_feature() {
    local ref=$1 path tag owner repo id dir token digest
    path=${ref#ghcr.io/}
    tag=${path##*:}
    path=${path%:*}
    owner=${path%%/*}
    repo=${path#*/}; repo=${repo%/*}
    id=${path##*/}
    dir="$cache/${path//\//_}"
    rm -rf "$dir"
    mkdir -p "$dir"
    if token=$(curl -fsS "https://ghcr.io/token?scope=repository:$path:pull" | jq -er .token) &&
        digest=$(curl -fsS -H "Authorization: Bearer $token" \
            -H "Accept: application/vnd.oci.image.manifest.v1+json" \
            "https://ghcr.io/v2/$path/manifests/$tag" | jq -er '.layers[0].digest') &&
        curl -fsSL -H "Authorization: Bearer $token" \
            "https://ghcr.io/v2/$path/blobs/$digest" | tar -x -C "$dir" 2>/dev/null; then
        echo "$dir"
        return
    fi
    echo "OCI download of $ref failed; using github.com/$owner/$repo src/$id" >&2
    rm -rf "$dir" "$dir.git"
    git clone -q --depth 1 "https://github.com/$owner/$repo.git" "$dir.git" >&2
    mv "$dir.git/src/$id" "$dir"
    rm -rf "$dir.git"
    echo "$dir"
}

mkdir -p "$cache" "$stamps"
mapfile -t features < <(jq -r '.features // {} | keys_unsorted[]' <<<"$config")
for ref in "${features[@]}"; do
    id=${ref%:*}; id=${id##*/}
    case " $skip_features " in *" $id "*)
        log "feature $ref: skipped"
        continue ;;
    esac
    case "$ref" in
        ./*|../*) dir="$here/$ref" ;;
        ghcr.io/*) dir= ;;
        *) echo "unsupported feature reference: $ref" >&2; exit 1 ;;
    esac

    # User options, with ${localEnv:...} expanded.
    declare -A opts=()
    while IFS=$'\t' read -r k v; do
        opts[$k]=$(expand_local_env "$v")
    done < <(jq -r --arg f "$ref" '.features[$f] | to_entries[] | [.key, (.value|tostring)] | @tsv' <<<"$config")

    stamp="$stamps/$(printf '%s' "$ref" | tr -c 'A-Za-z0-9._-' _)"
    want=$( { echo "$ref"; for k in "${!opts[@]}"; do echo "$k=${opts[$k]}"; done | sort; } | sha256sum | cut -d' ' -f1)
    if [ "$(cat "$stamp" 2>/dev/null)" = "$want" ]; then
        log "feature $ref: up to date"
        unset opts
        continue
    fi

    log "feature $ref"
    [ -n "$dir" ] || dir=$(fetch_feature "$ref")

    # Feature defaults, overridden by the user's options.
    env_args=(_REMOTE_USER=root _REMOTE_USER_HOME="$HOME" _CONTAINER_USER=root _CONTAINER_USER_HOME="$HOME")
    while IFS=$'\t' read -r k v; do
        [ -n "${opts[$k]+x}" ] || opts[$k]=$v
    done < <(jq -r '.options // {} | to_entries[] | [.key, (.value.default // "" | tostring)] | @tsv' "$dir/devcontainer-feature.json")
    for k in "${!opts[@]}"; do
        env_args+=("$(option_env_name "$k")=${opts[$k]}")
    done
    unset opts

    (cd "$dir" && env "${env_args[@]}" sh ./install.sh)
    echo "$want" >"$stamp"
done

# Features append their environment (e.g. OPAMROOT) to /etc/bash.bashrc and
# /etc/profile.d for later shells; pick it up for the commands below.
set +u
for f in /etc/profile.d/*.sh; do [ -r "$f" ] && . "$f"; done
set -u

# --- 3. postCreateCommand ---------------------------------------------------
log "postCreateCommand"
cd "$root"
jq -c '.postCreateCommand // empty |
    if type == "object" then to_entries[] else {key: "command", value: .} end' <<<"$config" |
while read -r entry; do
    name=$(jq -r .key <<<"$entry")
    cmd=$(jq -r '.value | if type == "array" then map(@sh) | join(" ") else . end' <<<"$entry")
    echo "--- $name: $cmd"
    sh -c "$cmd"
done

log "done"

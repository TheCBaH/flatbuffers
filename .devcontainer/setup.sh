#!/bin/bash
# Provisions the current machine (e.g. a Claude Code cloud environment) from
# .devcontainer/ without Docker, using the shared devcontainer-host tool from
# TheCBaH/devcontainer-action (host/devcontainer_host.py). It replays the
# Dockerfile, installs the features and runs postCreateCommand as root; see
# that repository's README for what it supports.
#
# Must run as root. Arguments are passed through to the tool, e.g. --dry-run.
#
# Environment:
#   OCAML_VERSION          consumed by devcontainer.json via ${localEnv:...}
#   DEVCONTAINER_HOST_REF  devcontainer-action commit, tag or branch to use
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/.." && pwd)
ref=${DEVCONTAINER_HOST_REF:-fd166d0b6d3c80c05a74c9f9e3479dd57698ecf0}

tool=$(mktemp --suffix=.py)
trap 'rm -f "$tool"' EXIT
curl -fsSL --retry 3 -o "$tool" \
    "https://raw.githubusercontent.com/TheCBaH/devcontainer-action/$ref/host/devcontainer_host.py"
python3 "$tool" --workspace-folder "$root" "$@"

# The ocaml feature exports OPAMROOT only through shell rc files, which
# non-login shells (an agent's tool shell, make recipes) never read; point
# opam's default root at the installed one.
if [ -d /opt/opam ] && [ ! -e "$HOME/.opam" ]; then
    ln -s /opt/opam "$HOME/.opam"
fi

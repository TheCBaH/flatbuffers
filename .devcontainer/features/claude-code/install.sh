#!/bin/sh
set -eu
set -x

echo "Activating feature 'Claude Code CLI'"
CLAUDE_CODE_VERSION=${VERSION:-latest}
echo "Selected Claude Code version: $CLAUDE_CODE_VERSION"

# From https://github.com/devcontainers/features/blob/main/src/git/install.sh
apt_get_update()
{
    if [ "$(find /var/lib/apt/lists/* | wc -l)" = "0" ]; then
        echo "Running apt-get update..."
        apt-get update -y
    fi
}

check_packages() {
    if ! dpkg -s "$@" > /dev/null 2>&1; then
        apt_get_update
        if ! apt-get -o Acquire::Retries=3 -y install --no-install-recommends "$@"; then
            apt-get update -y
            apt-get -o Acquire::Retries=3 -y install --no-install-recommends "$@"
        fi
    fi
}

export DEBIAN_FRONTEND=noninteractive

# Ensure node/npm is available
if ! command -v node > /dev/null 2>&1; then
    check_packages nodejs npm
fi

# Install claude-code globally outside of home directory
export npm_config_prefix=/usr/local
if [ "$CLAUDE_CODE_VERSION" = "latest" ]; then
    npm install -g @anthropic-ai/claude-code
else
    npm install -g "@anthropic-ai/claude-code@${CLAUDE_CODE_VERSION}"
fi

apt-get clean
rm -rf /var/lib/apt/lists/*

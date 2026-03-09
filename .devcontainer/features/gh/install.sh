#!/bin/sh
set -eu
set -x

echo "Activating feature 'GitHub CLI'"

export DEBIAN_FRONTEND=noninteractive

if [ "$(find /var/lib/apt/lists/* | wc -l)" = "0" ]; then
    apt-get update -y
fi

apt-get -o Acquire::Retries=3 -y install --no-install-recommends gh

apt-get clean
rm -rf /var/lib/apt/lists/*

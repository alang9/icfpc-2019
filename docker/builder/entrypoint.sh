#!/bin/bash
set -o nounset -o pipefail -o errexit

mkdir -p docker/runner/bin
stack build
cp $(stack path --local-install-root)/bin/* docker/runner/bin

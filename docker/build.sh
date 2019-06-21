#!/bin/bash
set -o errexit -o pipefail -o nounset

# Create and run a docker image to do the build for us.

docker build -t icfp2019-builder docker/builder
docker run -it \
    -m 4GB \
    --user $(id -u):$(id -g) \
    --mount type=bind,source=$(pwd),target=/build \
    --name icfp2019-builder-01 \
    icfp2019-builder
docker rm icfp2019-builder-01

# Build a runner docker image.

docker build -t icfp2019-runner docker/runner

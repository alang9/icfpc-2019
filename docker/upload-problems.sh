#!/bin/bash
set -o nounset -o errexit -o pipefail

# Find all problems and drop them in the problems bucket.
for problem in $(find data -name 'prob-*.desc'); do
    aws s3 cp "$problem" "s3://icfp2019-problems/$(basename "$problem")"
done

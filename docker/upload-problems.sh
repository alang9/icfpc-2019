#!/bin/bash
set -o nounset -o errexit -o pipefail

for problem in $(find data -name 'prob-*.desc'); do
    echo "Uploading $problem..." 1>&2
    aws s3 cp "$problem" "s3://icfp2019-problems/$(basename "$problem")"
done

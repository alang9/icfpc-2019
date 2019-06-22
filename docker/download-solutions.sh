#!/bin/bash
set -o nounset -o errexit -o pipefail

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 BUCKET_NAME" 1>&2
    exit 1
fi

BUCKET_NAME=$1
DESTINATION="$(mktemp -d)"

aws s3 sync "s3://$BUCKET_NAME" "$DESTINATION"
zip -j "$BUCKET_NAME.zip" "$DESTINATION"/*
rm -r "$DESTINATION"
echo "Created $BUCKET_NAME.zip" 1>&2

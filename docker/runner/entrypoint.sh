#!/bin/bash
set -o errexit -o pipefail -o nounset

PROBLEM=$((AWS_BATCH_JOB_ARRAY_INDEX + 1))

PROBLEM_FILE="prob-$(printf "%03d" "$PROBLEM").desc"
SOLUTION_FILE="prob-$(printf "%03d" "$PROBLEM").sol"

echo "Downloading problem from S3..." 1>&2
aws s3 cp "s3://icfp2019-problems/$PROBLEM_FILE" "./$PROBLEM_FILE"

./bin/greedy "$PROBLEM_FILE" >"$SOLUTION_FILE"

aws s3 cp "./$SOLUTION_FILE" "s3://$SOL_BUCKET_NAME/$SOLUTION_FILE"

#!/bin/bash
set -o errexit -o pipefail -o nounset

PROBLEM=$((AWS_BATCH_JOB_ARRAY_INDEX + 1))

PROBLEM_FILE="prob-$(printf "%03d" "$PROBLEM").desc"
BUY_FILE="prob-$(printf "%03d" "$PROBLEM").buy"
SOLUTION_FILE="prob-$(printf "%03d" "$PROBLEM").sol"

echo "Downloading problem from S3..." 1>&2
aws s3 cp "s3://icfp2019-problems/$PROBLEM_FILE" "./$PROBLEM_FILE"

echo "Downloading buy from S3..." 1>&2
aws s3 cp "s3://$ICFP2019_SOLUTION_BUCKET/$BUY_FILE" "./$BUY_FILE"

echo "Solving problem..." 1>&2
"./bin/$ICFP2019_SOLVER" "$PROBLEM_FILE" "$BUY_FILE" >"$SOLUTION_FILE"

echo "Storing solution in bucket..." 1>&2
aws s3 cp "./$SOLUTION_FILE" "s3://$ICFP2019_SOLUTION_BUCKET/$SOLUTION_FILE"

echo "Downloading best solution..." 1>&2
aws s3 cp "s3://$ICFP2019_BEST_SOLUTION_BUCKET/$SOLUTION_FILE" "./$SOLUTION_FILE.best"

echo "Computing score for our solution..." 1>&2
OUR_SCORE="$(./bin/sim "$PROBLEM_FILE" "$SOLUTION_FILE" "$BUY_FILE" 2>/dev/null)"

echo "Computing score for best solution..." 1>&2
BEST_SCORE="$(./bin/sim "$PROBLEM_FILE" "$SOLUTION_FILE.best" "$BUY_FILE" 2>/dev/null || echo undefined)"

echo "Our score: $OUR_SCORE, best score: $BEST_SCORE" 1>&2
if [[ "$BEST_SCORE" = "undefined" ]] || [[ "$OUR_SCORE" -lt "$BEST_SCORE" ]]; then
    echo "Replacing best solution..." 1>&2
    aws s3 cp "./$SOLUTION_FILE" "s3://$ICFP2019_BEST_SOLUTION_BUCKET/$SOLUTION_FILE"
    aws s3 cp "./$BUY_FILE" "s3://$ICFP2019_BEST_SOLUTION_BUCKET/$BUY_FILE"
fi

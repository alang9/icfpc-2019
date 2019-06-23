#!/bin/bash
set -o errexit -o pipefail -o nounset

if [[ $# -lt 2 ]]; then
    echo "Usage: $0 SOLVER JOB_NAME" 1>&2
    exit 1
fi

SOLVER=$1
JOB_NAME=$2
SOLUTION_BUCKET="icfp2019-sol-$JOB_NAME"
BEST_SOLUTION_BUCKET="icfp2019-solutions"

AWS_ACCOUNT_ID="$(aws sts get-caller-identity --output text --query 'Account')"

# Create an S3 bucket for the results.
echo "Creating bucket $SOLUTION_BUCKET..." 1>&2
aws s3api create-bucket --bucket "$SOLUTION_BUCKET"

# Update the job definition
echo "Updating job definition..." 1>&2
JOB_DEFINITION_JSON="$(mktemp)"
cat - >"$JOB_DEFINITION_JSON" <<EOF
{
  "jobDefinitionName": "$JOB_NAME",
  "type": "container",
  "containerProperties": {
    "image": "$AWS_ACCOUNT_ID.dkr.ecr.us-east-1.amazonaws.com/icfp2019-runner",
    "vcpus": 2,
    "memory": 7000,
    "environment": [
      {
        "name": "ICFP2019_SOLUTION_BUCKET",
        "value": "$SOLUTION_BUCKET"
      },
      {
        "name": "ICFP2019_BEST_SOLUTION_BUCKET",
        "value": "$BEST_SOLUTION_BUCKET"
      },
      {
        "name": "ICFP2019_SOLVER",
        "value": "$SOLVER"
      }
    ]
  }
}
EOF
aws batch register-job-definition --cli-input-json "file://$JOB_DEFINITION_JSON"

# Post the job itself
echo "Submitting actual job..." 1>&2
JOB_JSON="$(mktemp)"
cat - >"$JOB_JSON" <<EOF
{
  "jobName": "$JOB_NAME",
  "jobQueue": "icfp2019-queue",
  "arrayProperties": {
    "size": 300
  },
  "jobDefinition": "$JOB_NAME"
}
EOF
aws batch submit-job --cli-input-json "file://$JOB_JSON"

#!/bin/bash
set -o errexit -o pipefail -o nounset

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 JOB_NAME" 1>&2
    exit 1
fi

JOB_NAME=$1
SOL_BUCKET_NAME="icfp2019-sol-$JOB_NAME"

AWS_ACCOUNT_ID="$(aws sts get-caller-identity --output text --query 'Account')"

# Create an S3 bucket for the results.
echo "Creating bucket $SOL_BUCKET_NAME..." 1>&2
aws s3api create-bucket --bucket "$SOL_BUCKET_NAME"

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
        "name": "SOL_BUCKET_NAME",
        "value": "$SOL_BUCKET_NAME"
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

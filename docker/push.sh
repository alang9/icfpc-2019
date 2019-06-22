#!/bin/bash
set -o errexit -o pipefail -o nounset

AWS_ACCOUNT_ID="$(aws sts get-caller-identity --output text --query 'Account')"
$(aws ecr get-login --region us-east-1 --no-include-email)
aws ecr create-repository --repository-name icfp2019-runner || true
docker tag icfp2019-runner "$AWS_ACCOUNT_ID.dkr.ecr.us-east-1.amazonaws.com/icfp2019-runner"
docker push "$AWS_ACCOUNT_ID.dkr.ecr.us-east-1.amazonaws.com/icfp2019-runner"

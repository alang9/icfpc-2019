#!/bin/bash
set -o nounset -o pipefail -o errexit

sed -i "s/^PrivateKey = .*$/PrivateKey = $ICFP2019_PRIVATE_KEY/" lambda.conf
sed -i "s/^PublicKey = .*$/PublicKey = $ICFP2019_PUBLIC_KEY/" lambda.conf

python3 lambdad.py >/dev/null 2>&1 &
sleep 1
python3 lambda-cli.py getbalance

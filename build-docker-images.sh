#!/usr/bin/env bash

set -euo pipefail

echo "Archiving repo.."
git archive --format=tar --output repo.tar develop

echo "Building image.."
if [ "$1" = "base" ]; then
    docker build -t haskord-base:latest -f Dockerfile.base .
elif [ "$1" = "deploy" ]; then
    docker build -t haskord-deploy:latest -f Dockerfile.deploy .
else
    echo "Specify 'base' or 'deploy' as argument."
    exit 1
fi

echo "Pushing image to DockerHub.."
docker login
docker tag haskord-deploy:latest saevarb/haskord:deploy-latest
docker tag haskord-base:latest saevarb/haskord:base-latest
docker push saevarb/haskord


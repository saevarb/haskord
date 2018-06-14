#!/usr/bin/env bash

set -euo pipefail

echo "Archiving repo.."
git archive --format=zip --output repo.zip master

echo "Building image.."
docker build -t haskord:latest .

echo "Pushing image to DockerHub.."
docker login
docker tag haskord:latest saevarb/haskord:latest
docker push saevarb/haskord


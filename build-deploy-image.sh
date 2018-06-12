#!/usr/bin/env bash

set -euo pipefail

echo "Building image.."
docker build -t haskord:latest .

echo "Pushing image to DockerHub.."
docker login
docker tag haskord:latest saevarb/haskord:latest
docker push saevarb/haskord


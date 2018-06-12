#!/usr/bin/env bash

set -euo pipefail

echo "Building bot.."
(cd haskord-bot; stack build)

HASKORD_PATH=$(cd haskord-bot && stack exec which haskord)
MUEVAL_PATH=$(cd haskord-bot && stack exec which mueval)
MUEVAL_CORE_PATH=$(cd haskord-bot && stack exec which mueval-core)
CONFIG_PATH="haskord-bot/config.yaml"
DOCKER_LOGIN=saevarb
DOCKER_PASSWORD="$(pass dockerhub)"

docker build \
     -t haskord:latest \
     --build-arg=HASKORD_PATH="$(realpath --relative-to . $HASKORD_PATH)" \
     --build-arg=MUEVAL_PATH="$(realpath --relative-to . $MUEVAL_PATH)" \
     --build-arg=MUEVAL_CORE_PATH="$(realpath --relative-to . $MUEVAL_CORE_PATH)" \
     --build-arg=CONFIG_PATH="$CONFIG_PATH" \
     .

echo "Pushing image to DockerHub.."
docker login
docker tag haskord:latest saevarb/haskord:latest
docker push saevarb/haskord


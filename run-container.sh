#!/usr/bin/env bash

set -euo pipefail

backup_name="db.backup.$(date +"%F-%H%M%S")"

touch db.sqlite

if [ -f db.sqlite ]; then
    echo "Backing up database to $backup_name"
    cp db.sqlite "$backup_name"
fi

if [ "$(docker ps -aq -f=name=haskord)" ]; then
    echo "Stopping 'haskord'.."
    docker stop haskord
    echo "Removing 'haskord'.."
    docker rm haskord
fi
docker run \
       -v "$(pwd)/db.sqlite:/db.sqlite" \
       --name haskord \
       -it saevarb/haskord:latest

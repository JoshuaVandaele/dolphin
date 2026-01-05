#!/bin/bash

cleanup() {
    docker-compose down
    exit 1
}

trap cleanup SIGINT


# Extract service names from docker-compose.yml
services=($(grep '^[[:space:]]\{2\}[a-zA-Z0-9_-]\+:' "docker-compose.yml" | awk -F: '{print $1}' | tr -d ' '))

rm -fr ./logs
mkdir -p ./logs

for service in "${services[@]}"; do
    logfile="./logs/${service}.log"

    echo "Building $service..."
    start_build=$(date +%s)
    docker compose build "$service" > "$logfile" 2>&1
    end_build=$(date +%s)
    build_duration=$((end_build - start_build))
    echo "Build took ${build_duration}s" >> "$logfile"

    echo "Running $service..."
    start_run=$(date +%s)
    docker compose run --rm "$service" >> "$logfile" 2>&1
    end_run=$(date +%s)
    run_duration=$((end_run - start_run))
    echo "Run took ${run_duration}s" >> "$logfile"
done
#!/bin/bash
set -e

# Setup .env
cp docker/env.example .env
echo "COMPOSE_PROJECT_NAME=pecan" >> .env
echo "PECAN_VERSION=test" >> .env
echo "UID=$(id -u)" >> .env
echo "GID=$(id -g)" >> .env

# Setup override
cp docker-compose.dev.yml docker-compose.override.yml
sed -i'' '/R_library/d' docker-compose.override.yml

# Start services
echo "Starting services..."
docker compose up --wait -d sipnet executor monitor rabbitmq postgres

# Run workflow
echo "Running workflow..."
docker compose exec --workdir /pecan/tests executor R CMD ../web/workflow.R --settings docker-ghaction.sipnet.xml

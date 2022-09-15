#!/bin/bash

#docker build -t shiny-test --no-cache --network=host - < Dockerfile
docker build -t shiny-test -f Dockerfile ./
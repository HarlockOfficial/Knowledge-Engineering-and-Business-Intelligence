#!/bin/bash

sudo docker run \
  --name camunda-dmn-tester \
   --rm \
   -it \
   -e TESTER_CONFIG_PATHS="/dmnConfigs" \
   -v "$(pwd)/:/opt/docker/dmns" \
   -v "$(pwd)/documents:/opt/docker/dmnConfigs" \
   -p 8883:8883 \
   pame/camunda-dmn-tester
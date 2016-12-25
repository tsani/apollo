#!/bin/bash

set -e

source .secrets

mkdir -p music
(cd music ; exec ../dist/build/apollo/apollo $HOST $PORT $PASSWORD)

#!/bin/bash

set -e

mkdir -p music
(cd music ; exec ../dist/build/apollo/apollo)

#!/bin/bash

set -e

URL="http://localhost:8082/v1"

testvid="https://www.youtube.com/watch?v=RBqm1svadxg"

curl -v \
  --data "{ \"url\": \"$testvid\", \"path\": \"lol/foo\" }"\
  -H 'Content-type: application/json' \
  $URL/tracks/add/youtube-dl

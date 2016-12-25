#!/bin/bash

set -e

URL="http://localhost:8082/v1"

testvid="https://www.youtube.com/watch?v=RBqm1svadxg"

post() {
  curl -v -H 'Content-type: application/json' --data "$2" $URL$1
}

post /playlists/-/enqueue '["tsani/la tsani - nuntoi pa moi.mp3"]'

# post /tracks/add/youtube-dl "{ \"url\": \"$testvid\", \"path\": \"lol/foo\" }"

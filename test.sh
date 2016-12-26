#!/bin/bash

set -e

URL="http://localhost:8082/v1"

testvid="https://www.youtube.com/watch?v=RBqm1svadxg"

post() {
  curl -v -H 'Content-type: application/json' --data "$2" $URL$1
}

get() {
  curl -v $URL$1
}

gets() {
  curl --silent $URL$1
}

get /status

post /playlists/enqueue '["tsani/la tsani - nuntoi pa moi.mp3"]'

post /tracks/add/youtube-dl "{ \"url\": \"$testvid\", \"path\": \"lol/foo\" }"

post /transcode \
  '{ "source": "lol/foo/the smash community-RBqm1svadxg.mp3", "params": { "bitrate": { "type": "vbr", "value": 2 }, "format": "mp3" } }'

gets /transcode/963fe7ffee22e9f40bbbf5930bc7419ff5780076/mp3-vbr-2 > foo.mp3

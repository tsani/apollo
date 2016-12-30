#!/bin/bash

set -e

testvid="https://www.youtube.com/watch?v=RBqm1svadxg"

source testfuns

post /archive '[{ "type": "track", "trackPath": "lol/foo/the smash community-RBqm1svadxg.mp3" }]'
post /archive '[{ "type": "track", "trackPath": "lol/foo/the smash community-RBqm1svadxg.mp3" }]'

post /archive \
  '[{ "type": "track", "trackPath": "lol/foo/the smash community-RBqm1svadxg.mp3" }, { "type": "transcode", "trackId": "963fe7ffee22e9f40bbbf5930bc7419ff5780076", "transParams": { "format": "mp3", "bitrate": { "type": "vbr", "value": 2 } } }]'

get /status

get /playlist

put /playlist\?position\=start_-1 '["tsani/la tsani - nuntoi pa moi.mp3"]'

get /playlist

post /tracks/add/youtube-dl "{ \"url\": \"$testvid\", \"path\": \"lol/foo\" }"

post /transcode \
  '{ "source": "lol/foo/the smash community-RBqm1svadxg.mp3", "params": { "bitrate": { "type": "vbr", "value": 2 }, "format": "mp3" } }'

gets /transcode/963fe7ffee22e9f40bbbf5930bc7419ff5780076/mp3-vbr-2 > foo.mp3

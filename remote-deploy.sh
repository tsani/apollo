#!/bin/bash

ssh j "ssh apollo@labcoders-radio bash" <<'EOF'
export PATH=$HOME/.local/bin:$PATH
set -e
cd apollo
git fetch
git reset --hard origin/master
stack install
sudo systemctl restart apollo
systemctl status apollo
EOF

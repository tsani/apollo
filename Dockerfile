FROM ubuntu:18.04 AS base

RUN apt update

# runtime dependencies for the app
RUN apt install -y liblzma5 zlib1g

FROM base AS builder-system

# core dependencies for stack & the build system
RUN apt install -y netbase haskell-stack

# system dependencies for the app
RUN apt install -y liblzma-dev zlib1g-dev

RUN stack upgrade && ln -sf /root/.local/bin/stack /usr/local/bin
RUN stack update

FROM builder-system AS builder

WORKDIR /build

COPY stack.yaml ./
COPY apollo.cabal ./

RUN stack setup
RUN stack install --only-dependencies

COPY LICENSE ./
COPY ChangeLog.md ./
COPY src src/
RUN stack install

FROM base AS runtime

RUN apt install -y python3-pip
RUN pip3 install youtube-dl
RUN apt install -y ffmpeg

FROM runtime AS final
COPY --from=builder /root/.local/bin/apollo /app

WORKDIR /lcr
CMD /app

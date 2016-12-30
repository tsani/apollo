Apollo
======

Apollo is a webservice for interacting with an MPD instance and downloading
music to it.

Features:

  * Download new music to a remote MPD library using youtube-dl. Youtube-dl
    supports a huge number of streaming services, among them SoundCloud,
    Bandcamp, and YouTube.
  * Transcode music remotely to several formats using ffmpeg.
  * Export raw and transcoded music for offline listening.
  * MPD playlist manipulation.

### Setup

Build Apollo in a cabal sandbox. The bundled `runserver.sh` script can be used
to launch the server. It takes as one argument the Apollo database directory to
operate in. This directory must contain (at least) three items.

 1. `music` is the MPD music directory or a symlink thereto;
 2. `transcoded` is where Apollo stores transcoded tracks;
 3. `archives` is where Apollo stores music export archives.

To connect to MPD, Apollo takes command line arguments specifying the
connection parameters. `runserver.sh` parses the `MPD_HOST` environment
variable to construct the parameters to Apollo. If no `MPD_HOST` variable is
set, then `runserver.sh` defaults to localhost with no password on port 6600.
Recall that the `MPD_HOST` variable is of the form `PASSWORD@HOST:PORT`.

### Usage

A number of scripts are bundled with this repository for some common Apollo
workflows. The scripts generally require the `jq` and `curl` programs. The
scripts expect an environment variable `APOLLO_URL_BASE` to be set, and to be
of the form `https://apollo.example.org:8080`, i.e. consisting of the scheme
and hostname (and optionally the port number).

  * `apollo-dl` - download music to the MPD library using youtube-dl

    The script expects two positional arguments, a path of the form
    `Artist/Album` followed by a URL. The album part of the path may be
    omitted. If the `-c` switch is supplied, then the URL can be omitted; in
    this case it is taken from the clipboard (via xclip, which must be
    installed).

  * `apollo-export` - export a subset of the current playlist

    This script facilitates exporting (a portion of) the current playlist.
    First, the playlist is downloaded and opened in the editor given by the
    `EDITOR` variable. The user can filter the playlist to select only those
    tracks they wish to export. When the user quits their editor, the selected
    tracks are transcoded (by default to MP3 V2), archived, and the archive is
    downloaded to `apollo-export.zip`.

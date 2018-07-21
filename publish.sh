#!/bin/sh

set -xe

# Publish the revue with current content to production (srid.ca)

nix-build -A exe | cachix push srid

ssh srid@tinix.srid.ca 'cd ~/run/revue; and git pull; and nix-build -A exe'

# TODO: Restart the server

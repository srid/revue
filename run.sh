#!/bin/sh

set -xe

# Run the app; and restart the server when 'result' symlink gets updated.
export PORT=9005

touch .entr
ls .entr | entr -r -d sh -c 'cd result; ./backend -p $PORT'

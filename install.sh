#!/bin/sh

# Build the say-things binary
sbcl --load setup.lisp

# Copy the say-things binary and the say-things.sh wrapper to the PATH
chmod a+x say-things.sh
cp say-things say-things.sh /usr/local/bin

# Copy the plist to LaunchDaemons, load it and then start the daemon
cp say-things.plist /Library/LaunchDaemons
launchctl load /Library/LaunchDaemons/say-things.plist
launchctl start say-things

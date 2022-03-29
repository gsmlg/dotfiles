#!/bin/bash

# commands:

#FILE=gsmlg-cli_amd64.exe
#FILE=gsmlg-cli_arm64.exe
#FILE=gsmlg-cli_freebsd_amd64
#FILE=gsmlg-cli_linux_amd64
#FILE=gsmlg-cli_linux_arm64
FILE=gsmlg-cli_mac_amd64
#FILE=gsmlg-cli_mac_arm64

URL=$(curl -s https://api.github.com/repos/gsmlg-dev/gsmlg-cli/releases/latest |grep browser_download_url |grep $FILE |awk '{print $2}' | tr -d \")

FNAME=gsmlg-cli

curl -sSL $URL -o $FNAME

chmod +x $FNAME

mv $FNAME /usr/local/bin/$FNAME


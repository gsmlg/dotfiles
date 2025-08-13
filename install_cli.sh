#!/bin/bash

# commands:

#FILE=gsmlg-cli_amd64.exe
#FILE=gsmlg-cli_arm64.exe
#FILE=gsmlg-cli_freebsd_amd64
#FILE=gsmlg-cli_linux_amd64
#FILE=gsmlg-cli_linux_arm64
#FILE=gsmlg-cli_mac_amd64
#FILE=gsmlg-cli_mac_arm64

SYS="$(uname -s).$(uname -p)"

case $SYS in
  Darwin.i386)
  FILE=gsmlg-cli_mac_amd64
  ;;
  Darwin.arm)
  FILE=gsmlg-cli_mac_arm64
  ;;
  Linux.x86_64)
  FILE=gsmlg-cli_linux_amd64
  ;;
  Linux.aarch64)
  FILE=gsmlg-cli_linux_arm64
  ;;
  FreeBSD.amd64)
  FILE=gsmlg-cli_freebsd_amd64
  ;;
  *)
  echo "Unsupported $SYS"
  exit 1
  ;;
esac

URL=$(curl -s https://api.github.com/repos/gsmlg-dev/gsmlg-cli/releases/latest |grep browser_download_url |grep $FILE |awk '{print $2}' | tr -d \")

FNAME=gsmlg-cli

echo -e "Download from:\n$URL"

curl --progress-bar -sSLf $URL -o /tmp/$FNAME

chmod +x /tmp/$FNAME

mv /tmp/$FNAME /usr/local/bin/$FNAME


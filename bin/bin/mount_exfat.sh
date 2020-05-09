#!/usr/bin/env bash
# Mount an exFAT partition with the right permissions.

set -e

DEVICE=$1
OPT_MOUNT_POINT=$2

if [ -z "${OPT_MOUNT_POINT}" ]; then
  MOUNT_POINT="/media/exfat"
else
  MOUNT_POINT=${OPT_MOUNT_POINT}
fi

set -x

sudo mount -t exfat -o rw,auto,uid=${USER},gid=${USER},dmask=022,fmask=133 /dev/mmcblk0p1 ${MOUNT_POINT}

#!/bin/sh
if ! gputemp=$(nvidia-smi --format=nounits,csv,noheader --query-gpu=temperature.gpu 2> /dev/null | xargs echo); then
    gputemp=0
fi
if [ "$gputemp" -gt 0 ]; then
    echo "$gputemp°C"
else
    echo "OFF"
fi

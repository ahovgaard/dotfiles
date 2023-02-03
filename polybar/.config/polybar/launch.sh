#!/usr/bin/env bash

# Terminate already running bar instances
# killall -q polybar
# If all your bars have ipc enabled, you can also use
polybar-msg cmd quit

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Concatenate base config and host-specific config
cat ${SCRIPT_DIR}/config.base.ini \
    ${SCRIPT_DIR}/config.$(uname -n).ini > ${SCRIPT_DIR}/config.ini

echo "---" | tee /tmp/polybar.log

monitors=$(polybar --list-monitors)
primary=$(echo "${monitors}" | grep primary | cut -d":" -f1)
others=$(echo "${monitors}" | grep -v primary | cut -d":" -f1)

echo "Monitors:"
echo "${monitors}"
echo "Primary: ${primary}, others: ${others}"

echo "Launching main bar on ${primary}"
MONITOR=${primary} polybar --reload main 2>&1 | tee -a /tmp/polybar.log & disown

for m in ${others}; do
    echo "Launching secondary bar on ${m}"
    MONITOR=${m} polybar --reload secondary 2>&1 | tee -a /tmp/polybar.log & disown
done

echo "Bars launched..."

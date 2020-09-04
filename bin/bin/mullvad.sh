#!/bin/bash

set -euo pipefail

loc=$(mullvad relay list | grep -v '^\s\|^$' | rofi -i -dmenu | grep -Po '\(\K[^\)]+')
mullvad relay set location ${loc}

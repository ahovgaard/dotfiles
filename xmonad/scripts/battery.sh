acpi -b | awk '{print $3 " " $4 " " $5}'

STATE=`acpi -b | awk '{print $3}'`

if [ "$STATE" = "Discharging," ]; then
  BAT=`acpi -b | awk '{print $4}' | tr -d '[%,]'`
  if [ "$BAT" -lt 10 ]; then
    notify-send "low battery";
  fi
fi

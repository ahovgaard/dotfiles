rsync -aAXv --delete --progress \
  /home/akh/ /media/sdb/t520_backup/ \
  --exclude 'builds' \
  --exclude 'media' \
  --exclude '.cabal' \
  --exclude '.cache'

  #--exclude 'downloads' \
  #--exclude 'btsync' \

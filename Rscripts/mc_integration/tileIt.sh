#!/bin/sh
# tileIt.sh

endseq=8
for i in $(seq 1 $endseq); do fnames="$fnames plots4/normal$i.png"; done
montage -tile 4x -geometry +0+0 $fnames "$endseq"tile.png

fnames=""
endseq=9
for i in $(seq 1 $endseq); do fnames="$fnames plots4/normal$i.png"; done
montage -tile 3x -geometry +0+0 $fnames "$endseq"tile.png

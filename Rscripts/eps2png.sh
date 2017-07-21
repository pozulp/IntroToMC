#!/bin/bash
# eps2png.sh
# creates a png from an eps for
# all of the eps files in the cwd

for f in $(ls *.eps); do
    echo "Converting $f -> $f.png"
    convert -density 400% $f -resize 1024x1024 $f.png
done

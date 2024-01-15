#!/bin/bash

# Build the syntran interpreter, run syntran, plot the frames in
# Scilab, then make a video with ffmpeg
#
# Start this script from the root of the syntran repo:
#
#     ./samples/wave-equation-1d/run.sh

./build.sh

rm samples/wave-equation-1d/wave-output.txt

./build/syntran samples/wave-equation-1d/wave.syntran > samples/wave-equation-1d/wave-output.txt

pushd samples/wave-equation-1d
rm -rf frames
mkdir frames

# Honestly, just open the .sce file in Scilab and F5 if this doesn't work on
# your system
scilab="/c/Program Files/scilab-6.0.0/bin/WScilex.exe"
"$scilab" -nw -f plot-syntran-wave-eq.sce

ffmpeg -r 60 -i frames/wave_%d.png -vcodec libx264 -crf 15 -pix_fmt yuv420p wave.mp4 -y

popd


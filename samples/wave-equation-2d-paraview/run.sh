#!/bin/bash

# Build the syntran interpretter, run syntran, plot the frames in
# Scilab, then make a video with ffmpeg
#
# Start this script from the root of the syntran repo:
#
#     ./samples/wave-equation-2d-paraview/run.sh

./build.sh release

#rm samples/wave-equation-2d-paraview/wave-output.txt

./build/syntran samples/wave-equation-2d-paraview/wave.syntran

pushd samples/wave-equation-2d-paraview
rm -rf frames
mkdir frames

## Honestly, just open the .sce file in Scilab and F5 if this doesn't work on
## your system
#scilab="/c/Program Files/scilab-6.0.0/bin/WScilex.exe"
#"$scilab" -nw -f plot-syntran-wave-eq.sce
#
#./convert.sh

# TODO: add paraview batch script to save PNGs

#ffmpeg -r 60 -i frames/wave_%d.png -vcodec libx264 -crf 25 -pix_fmt yuv420p wave.mp4 -y
ffmpeg -r 24 -i frames/wave_%d.png -vcodec libx264 -crf 25 -pix_fmt yuv420p wave.mp4 -y
#ffmpeg -r 10 -i frames/wave_%d.png -vcodec libx264 -crf 25 -pix_fmt yuv420p wave.mp4 -y

popd


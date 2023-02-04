#!/bin/bash

# Build the syntran interpretter, run syntran, plot the frames in
# ParaView, then make a video with ffmpeg
#
# Start this script from the root of the syntran repo:
#
#     ./samples/wave-equation-2d-paraview/run.sh

./build.sh release

#rm samples/wave-equation-2d-paraview/wave-output.txt

pushd samples/wave-equation-2d-paraview
rm -rf frames
mkdir frames
popd

./build/syntran samples/wave-equation-2d-paraview/wave.syntran
pushd samples/wave-equation-2d-paraview

pvpython="/c/Program Files/ParaView 5.8.0-Windows-Python3.7-msvc2015-64bit/bin/pvpython.exe"

# bash: Argument list too long.  Break into chunks
"$pvpython" batch.py -f frames/wave-*[0-4].vtk
"$pvpython" batch.py -f frames/wave-*[5-9].vtk

ffmpeg -r 60 -i frames/wave-%d.png -vcodec libx264 -crf 25 -pix_fmt yuv420p wave.mp4 -y
#ffmpeg -r 24 -i frames/wave-%d.png -vcodec libx264 -crf 25 -pix_fmt yuv420p wave.mp4 -y
#ffmpeg -r 10 -i frames/wave-%d.png -vcodec libx264 -crf 25 -pix_fmt yuv420p wave.mp4 -y

popd


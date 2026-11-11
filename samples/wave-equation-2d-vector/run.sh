#!/bin/bash

# Build the syntran interpreter, run syntran, plot the frames in
# ParaView, then make a video with ffmpeg
#
# Start this script from the root of the syntran repo:
#
#     ./samples/wave-equation-2d-vector/run.sh

#./build.sh release

#rm samples/wave-equation-2d-vector/wave-output.txt

pushd samples/wave-equation-2d-vector
rm -rf frames
mkdir frames
popd

fpm run --profile release -- samples/wave-equation-2d-vector/wave.syntran

pushd samples/wave-equation-2d-vector

#pvpython="/c/Program Files/ParaView 5.8.0-Windows-Python3.7-msvc2015-64bit/bin/pvpython.exe"
#pvpython="/mnt/c/Program Files/ParaView 5.8.0-Windows-Python3.7-msvc2015-64bit/bin/pvpython.exe"
pvpython="/mnt/c/Program Files/ParaView 5.11.0/bin/pvpython.exe"

## bash: Argument list too long.  Break into chunks

#"$pvpython" batch.py -f frames/wave-*[0-4].vtk
#"$pvpython" batch.py -f frames/wave-*[5-9].vtk
"$pvpython" batch.py -f frames/wave-*[0-3].vtk
"$pvpython" batch.py -f frames/wave-*[4-7].vtk
"$pvpython" batch.py -f frames/wave-*[8-9].vtk

ffmpeg -r 60 -i frames/wave-%d.png -vcodec libx264 -crf 16 -pix_fmt yuv420p wave.mp4 -y
#ffmpeg -r 24 -i frames/wave-%d.png -vcodec libx264 -crf 16 -pix_fmt yuv420p wave.mp4 -y
#ffmpeg -r 10 -i frames/wave-%d.png -vcodec libx264 -crf 16 -pix_fmt yuv420p wave.mp4 -y

popd


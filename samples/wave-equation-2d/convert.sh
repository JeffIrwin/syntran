#!/bin/bash

#for i in {1..352}; do
#for i in {352..480}; do
#for i in {480..1591}; do

for i in {1..1591}; do

	echo "frames/wave_${i}.png ..."

	#magick -size 2440x1840 frames/wave_${i}.svg frames/wave_${i}.png
	magick -size 1920x1080 frames/wave_${i}.svg frames/wave_${i}.png

done


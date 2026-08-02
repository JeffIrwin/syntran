#!/usr/bin/env bash

# Dependencies:
# - pandoc
#   * sudo apt install pandoc
#
# For pdf output:
# - latex
#   * sudo apt install texlive-latex-recommended
# - rsvg-convert
#   * only for pdf output
#   * sudo apt install librsvg2-bin

#****************

# Generate an html viewable by a web browser
pandoc README.md -o README.html

## TODO: unicode chars break tex
#pandoc README.md -o README.pdf


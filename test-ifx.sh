#!/usr/bin/env bash

set -exu

time \
	fpm test test \
	--compiler ifx \
	--flag "-DSYNTRAN_INTEL -fpp"


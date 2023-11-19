#!/bin/bash

magenta="\e[95m"
green="\e[92;1m"
red="\e[91;1m"
reset="\e[0m"

echo
echo -e "${magenta}Installing syntran ... ${reset}"
echo

this_syntran="./build/syntran"

fail_find()
{
	set +x
	echo
	echo -e "${red}Failed to find syntran${reset}"
	echo
	echo -e "${red}Choose a folder in your PATH environment variable and copy \"$this_syntran\" there first, before using this script to update it${reset}"
	echo
	exit -1
}

# Copy the syntran interpretter built here to the user/system's bin folder.
# Really more of a updater than an installer

set -xe

# TODO: clean and build release?

usr_syntran=$(which syntran) || fail_find

cp "$this_syntran" "$usr_syntran"

set +x

echo
echo -e "${green}Succesfully installed syntran ${reset}"
echo


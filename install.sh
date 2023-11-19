#!/bin/bash

green="\e[92;1m"
magenta="\e[95m"
yellow="\e[93m"
red="\e[91;1m"
bold="\e[;1m"
reset="\e[0m"

echo
echo -e "${magenta}Installing syntran ... ${reset}"
echo

this_syntran="./build/syntran"

fail_find()
{
	set +x
	echo
	echo -e "${red}Failed to find existing syntran installation${reset}"
	echo
	echo -e "${yellow}Choose a folder in your PATH environment variable and copy \"$this_syntran\" there first, before using this script to update it${reset}"
	echo
	echo -e "${yellow}Hint:  choose a folder such as \"/usr/local/bin/\" or \"~/bin/\", if it exists, according to your preference${reset}"
	echo
	echo -e "${yellow}For example:${reset}"
	echo
	echo -e "    ${bold}sudo cp \"$this_syntran\" /usr/local/bin/${reset}"
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


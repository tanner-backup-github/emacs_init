#!/bin/bash

clear
YELLOW="\e[93m"
DEFAULT="\e[0m"

for arg in "$@"
do
	printf "${YELLOW}Showing annotations matching $arg${DEFAULT}\n"
	grep --color=always -rni "$arg" .
done

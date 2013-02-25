#!/bin/bash

cabal test

if [[ $? -eq 0 ]]; then
	echo -e '\e[0;32m'Passed'\e[0m'
else
	echo -e '\e[0;31m'Failed'\e[0m'
fi

#!/bin/bash

while true; do
    read -p "Run again?" -n1 yn
    case $yn in
        [Yy]* ) runhaskell rfk.hs;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
done

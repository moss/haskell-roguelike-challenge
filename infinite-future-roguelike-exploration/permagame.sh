#!/bin/bash

yn='y'
while true; do
    case $yn in
        [Yy]* ) runhaskell -isrc src/rfk.hs;;
        [Nnq]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
    read -p "Run again?" -n1 yn
    clear
done

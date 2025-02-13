#!/bin/bash

for file in $(pwd)/.bin/*; do
    ln -s "$file" /usr/local/bin/"$(basename "$file")"
done


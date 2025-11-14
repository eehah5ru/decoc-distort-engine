#!/bin/sh

echo "replacing map in diagrams folder with $1"

MAP_FILE=$1

cp $MAP_FILE data/diagrams/map.svg
cp $MAP_FILE data/diagrams/map_patched.svg

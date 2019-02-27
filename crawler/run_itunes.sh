#!/bin/bash

while true
do
    fname=soundcloud-`date +"%s"`.jsons.xz
    python itunes.py itunes.sqlite $fname
done


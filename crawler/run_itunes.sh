#!/bin/bash

while true
do
    fname=soundcloud-`date +"%s"`.jsons.xz
    python itunes.py itunes_queue.sqlite $fname
    (
        s3cmd put $fname s3://itunespod-crawl
        rm $fname
    )&
done


#!/bin/bash

while true
do
    fname=soundcloud-`date +"%s"`.jsons.xz
    python soundcloud.py queue.sqlite $fname
    (
        s3cmd put $fname s3://sc-crawl
        rm $fname
    )&
done


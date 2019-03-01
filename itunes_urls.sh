#!/bin/bash
for f in $@; do
	xzcat --check=none $f | ./itunes_url_ids.py
done

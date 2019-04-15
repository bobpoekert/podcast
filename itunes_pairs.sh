#!/bin/bash

(for f in $@; do xzcat --check=none $f || true; done) | ./itunes_pairs.py

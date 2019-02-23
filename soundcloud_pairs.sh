#!/bin/bash

(for f in $@; do xzcat --check=none $f || true; done) | jq -R -r 'fromjson? | "\(.user_id)\t\((.followers + .followings) [] .id)"' | hashuniq

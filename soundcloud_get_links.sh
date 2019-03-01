#!/bin/bash

for f in $@
do
	xzcat $f | jq -r -R 'fromjson? | "\(.user_id) \(.links [] .url)"'
done

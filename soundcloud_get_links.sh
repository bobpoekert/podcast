#!/bin/bash

for f in $@
do
	xzcat $f | jq -r -R 'fromjson? | "\(.user_id) \(.links [] .url)"' | sed -r 's/http.*itunes\.apple\.com.*?(id|\/)([0-9]+)([\?#][^ ]*)?/itunes:\2/'
done

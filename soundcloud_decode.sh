#!/bin/bash

(for f in $@; do xzcat --check=none $f || true; done) | jq -R 'fromjson? | (.followers + .followings) [] | [.id, .avatar_url, .city, .comments_count, .country_code, .created_at, .description, .first_name, .followers_count, .followings_count, .full_name, .groups_count, .kind, .last_modified, .last_name, .likes_count, .permalink, .permalink_url, .playlist_count, .playlist_likes_count, .track_count, .uri, .urn, .username, .verified] | @csv' | hashuniq | sed -e 's/^"//' -e 's/"$//g' -e 's/\\"/"/g' -e "s/\$/$CR/g"


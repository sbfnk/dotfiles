#!/usr/bin/env bash
yabai -m query --spaces --display | \
     jq -re 'map(select(."is-native-fullscreen" == false)) | length > 1' > /dev/null \
     && yabai -m query --spaces | \
          jq -re 'map(select(."windows" == [] and ."has-focus" == false and ."index" > 6).index) | reverse | .[] ' | \
          xargs -I % sh -c 'yabai -m space % --destroy'

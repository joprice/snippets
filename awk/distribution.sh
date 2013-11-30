#!/usr/bin/env bash

width=600
file="data"

cat "$file" | \
  cut -d ',' -f 5 | \
  grep -v "Count" | \
  sort -n | \
  awk 'BEGIN { i = 0 } { if ($1 % '$width' == 0) { i = $1 }; print i }' | \
  uniq -c | \
  awk '{
  t=""; i=0; 
  for(;i<$1 / '$width';i++) { 
    t = t "*" 
  } 
  print $2, t
}'


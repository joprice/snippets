#!/usr/bin/env sh

set -eu

runcmd() {
  cmd="$@"
  echo "$cmd"
  `$cmd`
}

trim() {
  echo "$1" | tr -d ' '
}

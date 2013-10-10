#!/usr/bin/env bash

# joins an array with commas
j() { 
  local IFS=","; 
  echo -e "$*"
}


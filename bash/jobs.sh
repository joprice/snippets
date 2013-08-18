#!/usr/bin/env sh

set -eu

numProcessors() {
  echo $(sysctl hw.ncpu | cut -d' ' -f2)
}

declare -a jobs

addJob() {
  job="$@"
  jobs[${#jobs[*]}]="$job"
}

runJobs() {
  log "running ${#jobs[@]} jobs"
  for i in ${!jobs[@]}; do
    job="${jobs[i]}"
    log "running $job"
    eval "$job" &
    pid=$!
    pids[${#pids[*]}]=$pid
  done
}

echo $(numProcessors)


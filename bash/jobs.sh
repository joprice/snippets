#!/usr/bin/env sh

set -eu
declare -a jobs

cleanup() {
  for pid in ${pids[@]}; do
    log "killing $pid"
    if ! kill $pid > /dev/null 2>&1; then
      error "Did not find pid $pid"
    fi  
  done
}

trap "cleanup" SIGINT SIGTERM

numProcessors() {
  echo $(sysctl hw.ncpu | cut -d' ' -f2)
}

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


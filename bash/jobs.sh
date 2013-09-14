#!/usr/bin/env sh

set -eu

jobs=()

declare -a pids
declare -a jobs

jobs=('sleep 1; echo 10 >>out' 'sleep 2; echo 20 >>out' 'sleep 1; echo 30 >>out' 
  'echo 40 >>out' 'echo 50 >>out' 'echo 60 >>out' 'echo 70 >>out' 'echo 80 >>out' 
  'echo 90 >>out' 'echo 100 >>out'
)

cur=0
logFile=jobs.log

retry() {
  local pid=$1
  log "retrying $pid"
  runJob pids[$pid]
}

log() {
  echo "$(date) $1" >> "$logFile"
}

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
  local job="$@"
  jobs[${#jobs[*]}]="$job"
}

runJob() {
  local job="$1"
  eval "$job" &
  pid=$!
  pids[${#pids[*]}]=$pid
}

runJobs() {
  local len=${#jobs[@]}
  log "running $len jobs"
  echo "max par $maxPar"
  for (( i=0; i < $len && i < maxPar; i++ )); do
    echo "run $i"
    nextJob
  done
  echo "done running"
}

nextJob() {
  local job="${jobs[cur]}"
  local len=${#jobs[@]}
  log "running job $((cur + 1)) of $len $job"
  runJob "$job"
  cur=$((cur + 1))
}

synchronize() {
  local len=${#jobs[@]}
  for pid in ${pids[@]}; do  
    echo "waiting on pid $pid"
    if wait $pid; then
      unset jobs["$pid"]
      log "$pid - success"
      if [ "$cur" -lt "$len" ]; then
        echo "finished pid $pid; running job $cur"
        nextJob
      else
        echo "no more jobs"
      fi
    else 
      error "$pid - failure"
      runJob $pid
      #retry -- keep pids in map of pid -> job string to retry
    fi  
  done
}

maxPar=$(numProcessors)
runJobs
synchronize



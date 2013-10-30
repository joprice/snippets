
# prints first 100 lines (1-indexed), then quits. Without the q, it would continue to walk the entire file
sed -n '1,200p;200q' file


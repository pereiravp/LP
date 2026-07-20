#!/usr/bin/env bash

args=()
while IFS= read -r -d '' dir; do
    args+=(--hpcdir="$dir")
done < <(find dist-newstyle -type d -path "*/vanilla/mix" -print0)

echo "hpc markup $1.tix ${args[*]}"
hpc markup "$1".tix "${args[@]}"

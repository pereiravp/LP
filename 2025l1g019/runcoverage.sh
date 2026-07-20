#!/usr/bin/env bash

set -Eeuo pipefail

TAREFA=

case "$1" in
  t1 | t2 | t3 | t4)
    TAREFA="$1"
    ;;
  *)
    echo "Usage: $0 (t1 | t2 | t3 | t4)"
    exit 1
    ;;
esac

cabal clean && rm -rf ${TAREFA}-feedback.tix
cabal run --enable-coverage ${TAREFA}-feedback

bash ./runhpc.sh ${TAREFA}-feedback

printf "\033[33m*\033[0m Relatório disponível em \033[36m$(pwd)/hpc_index.html\033[0m\n"

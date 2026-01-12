#!/usr/bin/env bash
set -euo pipefail

export ALN_ENV=prod
export ALN_SEED=987654
export ALN_DIFFICULTY_TARGET=0.55

mkdir -p logs

node dist/server.js --seed "$ALN_SEED" --difficulty "$ALN_DIFFICULTY_TARGET" && \
node dist/telemetry.js >> logs/telemetry.log 2>&1

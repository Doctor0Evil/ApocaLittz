#!/usr/bin/env bash
set -euo pipefail

export ALN_ENV=${ALN_ENV:-dev}
export ALN_SEED=${ALN_SEED:-12345}
mkdir -p logs

export ALN_LOG=${ALN_LOG:-logs/run.$(date +%s).log}

build()  { pnpm -s build && cargo build --quiet; }
testall(){ pnpm -s test && cargo test; }
sim()    { node dist/sim.js --seed "$ALN_SEED" --env "$ALN_ENV"; }
viz()    { node tools/viz.js --input logs/trace.json --out web/viz; }

(build && testall && sim) 2>&1 | tee "$ALN_LOG"

viz ; echo "Done"

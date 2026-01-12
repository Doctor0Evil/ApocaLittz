# ALN Encounter Framework

AI-driven encounter and narrative system with ALN DSL, TypeScript orchestration, and Rust simulation cores.

## Setup

```bash
pnpm install
cargo build
pnpm -C tests run build
Run a sample simulation

```bash
node tests/dist/sim.js --seed 4242 --ticks 200 --out logs/trace.json
node tools/dist/viz.js --input logs/trace.json --out web/viz
Open web/viz/index.html in a browser to inspect the encounter trace DAG.


All files above are drop‑in and consistent, so you can create a repo with this structure, run the build commands, and immediately simulate and visualize the Courier‑Null versus Warden Kine encounter loop.

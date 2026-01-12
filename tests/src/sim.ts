import fs from "node:fs";
import path from "node:path";
import { graph, nodes } from "../../narrative/src/graph";
import { step } from "../../narrative/src/schema";

interface EnvState {
  temp: number;
  wind: number;
  vis: number;
}

interface CombatState {
  npc: { hp: number; max: number };
  player: { hp: number; ammo: number };
}

interface TraceRow {
  tick: number;
  node: string;
  env: EnvState;
  npcHp: number;
  playerHp: number;
}

function rngFactory(seed: number) {
  let s = seed >>> 0;
  return function rng() {
    s = (1103515245 * s + 12345) % 0x80000000;
    return s / 0x80000000;
  };
}

function tickEnv(env: EnvState, rng: () => number): EnvState {
  const visDelta = (rng() - 0.5) * 0.05;
  const windDelta = (rng() - 0.5) * 0.6;
  return {
    vis: Math.max(0.2, Math.min(1.0, env.vis + visDelta)),
    wind: Math.max(0, env.wind + windDelta),
    temp: env.temp
  };
}

export function runSimulation(seed: number, ticks: number): TraceRow[] {
  const rng = rngFactory(seed);
  let env: EnvState = { temp: -3, wind: 7, vis: 0.6 };
  const combat: CombatState = {
    npc: { hp: 100, max: 100 },
    player: { hp: 100, ammo: 1 }
  };

  let nodeId = graph.start;
  const trace: TraceRow[] = [];

  for (let t = 0; t < ticks; t++) {
    env = tickEnv(env, rng);
    nodeId = step(graph, nodeId, { env, player: combat.player, npc: combat.npc }, rng) ?? nodeId;

    if (nodeId === "combat") {
      const hit = rng() < 0.6;
      if (hit) {
        combat.player.hp -= 10;
      }
    }

    trace.push({
      tick: t,
      node: nodeId,
      env,
      npcHp: combat.npc.hp,
      playerHp: combat.player.hp
    });

    if (combat.player.hp <= 0) break;
  }

  return trace;
}

if (require.main === module) {
  const seedIdx = process.argv.indexOf("--seed");
  const ticksIdx = process.argv.indexOf("--ticks");
  const outIdx = process.argv.indexOf("--out");

  const seed = seedIdx !== -1 ? Number(process.argv[seedIdx + 1]) : 4242;
  const ticks = ticksIdx !== -1 ? Number(process.argv[ticksIdx + 1]) : 200;
  const out = outIdx !== -1 ? process.argv[outIdx + 1] : "logs/trace.json";

  const rows = runSimulation(seed, ticks);
  const outDir = path.dirname(out);
  fs.mkdirSync(outDir, { recursive: true });
  fs.writeFileSync(out, JSON.stringify(rows, null, 2), "utf8");
}

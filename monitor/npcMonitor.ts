export type Tactic = "rush" | "feint" | "pressure" | "break_off" | "threaten" | "parley";

export interface NpcSnapshot {
  id: string;
  hp: number;
  morale: number;
  fatigue: number;
  tactic: Tactic;
  position: [number, number];
  envSeed: number;
  tick: number;
}

export function analyze(snapshot: NpcSnapshot) {
  const risk = (1 - snapshot.hp / 100) + snapshot.fatigue - snapshot.morale;
  const escalate = snapshot.tactic === "pressure" && risk < 0.4;
  return { risk, escalate, advise: escalate ? "commit" : "probe" };
}

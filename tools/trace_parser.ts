import fs from "node:fs";

export interface TraceRow {
  tick: number;
  node: string;
  reward?: number;
}

export function summarize(path: string) {
  const buf = fs.readFileSync(path, "utf8");
  const rows: TraceRow[] = JSON.parse(buf);
  const counts = new Map<string, number>();
  for (const r of rows) {
    counts.set(r.node, (counts.get(r.node) ?? 0) + 1);
  }
  return Array.from(counts.entries()).sort((a, b) => b[1] - a[1]);
}

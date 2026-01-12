import fs from "node:fs";
import path from "node:path";

export interface TraceSpan {
  tick: number;
  env: any;
  status: any;
  gate?: string;
  branch?: string;
  tactic?: string;
  reward?: number;
}

export function render(trace: TraceSpan[], outDir: string) {
  fs.mkdirSync(outDir, { recursive: true });
  const nodes = new Map<string, number>();
  const edges = new Map<string, number>();

  let lastNode: string | null = null;

  for (const span of trace) {
    const node = span.gate ?? span.branch ?? span.tactic ?? `tick_${span.tick}`;
    nodes.set(node, (nodes.get(node) ?? 0) + 1);

    if (lastNode) {
      const key = `${lastNode}->${node}`;
      edges.set(key, (edges.get(key) ?? 0) + 1);
    }
    lastNode = node;
  }

  const html = `<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8" />
<title>ALN Encounter Trace</title>
<style>
body { font-family: system-ui, sans-serif; background:#050810; color:#e8ecff; }
svg { width:100%; height:100vh; }
.node { fill:#1f2937; stroke:#60a5fa; stroke-width:1; }
.edge { stroke:#4b5563; stroke-width:1; }
label { font-size:10px; fill:#e5e7eb; }
</style>
</head>
<body>
<h1>ALN Encounter Trace</h1>
<svg viewBox="0 0 1200 800">
${Array.from(nodes.entries())
  .map(([name, count], idx) => {
    const x = 80 + (idx % 10) * 110;
    const y = 80 + Math.floor(idx / 10) * 110;
    return `<g transform="translate(${x},${y})">
  <rect class="node" x="-40" y="-20" width="80" height="40" rx="6"/>
  <text class="label" x="0" y="0" text-anchor="middle">${name} (${count})</text>
</g>`;
  })
  .join("\n")}
</svg>
</body>
</html>`;

  fs.writeFileSync(path.join(outDir, "index.html"), html, "utf8");
}

if (require.main === module) {
  const input = process.argv[2] || "--input";
  const idx = process.argv.indexOf("--input");
  const outIdx = process.argv.indexOf("--out");
  if (idx === -1 || outIdx === -1) {
    console.error("Usage: node viz.js --input trace.json --out web/viz");
    process.exit(1);
  }
  const tracePath = process.argv[idx + 1];
  const outDir = process.argv[outIdx + 1];
  const buf = fs.readFileSync(tracePath, "utf8");
  const trace: TraceSpan[] = JSON.parse(buf);
  render(trace, outDir);
}

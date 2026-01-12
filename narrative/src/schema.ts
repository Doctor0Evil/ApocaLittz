export interface Gate {
  id: string;
  expr: (s: any) => boolean;
}

export interface Branch {
  id: string;
  weight: number;
  next: string;
}

export interface Node {
  id: string;
  gates: Gate[];
  branches: Branch[];
  onEnter?: (s: any) => void;
  onExit?: (s: any) => void;
}

export interface Graph {
  nodes: Node[];
  start: string;
}

export function pickBranch(branches: Branch[], rng: () => number): Branch | null {
  if (!branches.length) return null;
  const total = branches.reduce((acc, b) => acc + b.weight, 0);
  let r = rng() * total;
  for (const b of branches) {
    if (r < b.weight) return b;
    r -= b.weight;
  }
  return branches[branches.length - 1] ?? null;
}

export function step(graph: Graph, nodeId: string, state: any, rng: () => number): string | null {
  const node = graph.nodes.find(n => n.id === nodeId);
  if (!node) return null;
  if (node.onEnter) node.onEnter(state);

  for (const g of node.gates) {
    if (!g.expr(state)) {
      if (node.onExit) node.onExit(state);
      return null;
    }
  }

  const branch = pickBranch(node.branches, rng);
  if (node.onExit) node.onExit(state);
  return branch?.next ?? null;
}

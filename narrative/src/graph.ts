import type { Graph, Node } from "./schema";

export const nodes: Node[] = [
  {
    id: "opening",
    gates: [],
    branches: [
      { id: "threat", weight: 0.5, next: "standoff" },
      { id: "parley", weight: 0.5, next: "talk" }
    ]
  },
  {
    id: "standoff",
    gates: [
      { id: "ammo_check", expr: s => s.player.ammo > 0 }
    ],
    branches: [
      { id: "fire", weight: 0.6, next: "combat" },
      { id: "hesitate", weight: 0.4, next: "talk" }
    ]
  },
  {
    id: "talk",
    gates: [],
    branches: [
      { id: "negotiate", weight: 0.7, next: "deal" },
      { id: "fail", weight: 0.3, next: "combat" }
    ]
  },
  {
    id: "deal",
    gates: [],
    branches: []
  },
  {
    id: "combat",
    gates: [],
    branches: []
  }
];

export const graph: Graph = {
  nodes,
  start: "opening"
};

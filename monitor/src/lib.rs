use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NpcSnapshot {
    pub hp: f32,
    pub morale: f32,
    pub fatigue: f32,
    pub tactic: String,
}

pub fn assess(s: &NpcSnapshot) -> (f32, &'static str) {
    let risk = (1.0 - s.hp / 100.0) + s.fatigue - s.morale;
    let advise = if s.tactic == "pressure" && risk < 0.4 {
        "commit"
    } else {
        "probe"
    };
    (risk.max(0.0), advise)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn low_risk_pressure_commits() {
        let s = NpcSnapshot {
            hp: 90.0,
            morale: 0.8,
            fatigue: 0.1,
            tactic: "pressure".into(),
        };
        let (risk, advise) = assess(&s);
        assert!(risk >= 0.0);
        assert_eq!(advise, "commit");
    }
}

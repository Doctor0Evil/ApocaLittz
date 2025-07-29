extends Node

# Faction-specific economy data, including resources, units, buildings, facilities, heroes, commanders, superweapons, and tech
var factions = {
	"Survivors": {
		"resources": {"ore": 10000, "energy": 5000, "credits": 10000, "food": 5000, "iron": 3000, "power": 5000, "relics": 0},
		"units": {"scavenger": 1, "builder": 0, "survivor": 0, "grunt": 0},
		"buildings": ["scrapyard"],
		"facilities": [],
		"heroes": [],
		"commanders": [],
		"superweapons": [],
		"tech": []
	},
	"Monsters": {
		"resources": {"bio-mass": 10000, "food": 5000, "power": 5000, "data_cores": 0},
		"units": {"screecher": 1, "sporeling": 0, "brute": 0, "stalker": 0},
		"buildings": ["hive_core"],
		"facilities": [],
		"heroes": [],
		"commanders": [],
		"superweapons": [],
		"tech": []
	},
	"Government": {
		"resources": {"ore": 10000, "energy": 5000, "credits": 10000, "food": 5000, "iron": 3000, "power": 5000, "relics": 0},
		"units": {"rifleman": 1, "engineer": 0, "sniper": 0, "tank": 0},
		"buildings": ["command_center"],
		"facilities": [],
		"heroes": [],
		"commanders": [],
		"superweapons": [],
		"tech": []
	},
	"Raiders": {
		"resources": {"ore": 10000, "energy": 5000, "credits": 10000, "food": 5000, "iron": 3000, "power": 5000, "relics": 0},
		"units": {"marauder": 1, "saboteur": 0, "gunner": 0, "buggy": 0},
		"buildings": ["raider_camp"],
		"facilities": [],
		"heroes": [],
		"commanders": [],
		"superweapons": [],
		"tech": []
	}
}

# Costs for deploying units, buildings, facilities, heroes, commanders, superweapons, and tech
var costs = {
	# Units (Infantry requires food and credits; some require iron; low ore costs)
	"scavenger": {"credits": 500, "food": 200, "power": 500},
	"builder": {"credits": 500, "food": 200, "iron": 100, "power": 500},
	"survivor": {"credits": 750, "food": 300, "iron": 150, "power": 750},
	"grunt": {"credits": 750, "food": 300, "iron": 150, "power": 750},
	"screecher": {"bio-mass": 500, "food": 200, "power": 500},
	"sporeling": {"bio-mass": 500, "food": 200, "power": 500},
	"brute": {"bio-mass": 1000, "food": 400, "power": 1000},
	"stalker": {"bio-mass": 750, "food": 300, "power": 750},
	"rifleman": {"credits": 500, "food": 200, "power": 500},
	"engineer": {"credits": 500, "food": 200, "iron": 100, "power": 500},
	"sniper": {"credits": 750, "food": 300, "iron": 150, "power": 750},
	"tank": {"credits": 15000, "iron": 3000, "ore": 5000, "power": 1500},
	"marauder": {"credits": 500, "food": 200, "power": 500},
	"saboteur": {"credits": 500, "food": 200, "iron": 100, "power": 500},
	"gunner": {"credits": 750, "food": 300, "iron": 150, "power": 750},
	"buggy": {"credits": 10000, "iron": 2000, "ore": 3000, "power": 1000},
	# Heroes (1 per faction)
	"jericho_kane": {"credits": 5000, "food": 1000, "ore": 2000, "power": 2000}, # Survivors: Boosts scavenging efficiency
	"vyrak": {"bio-mass": 5000, "food": 1000, "power": 2000}, # Monsters: Spawns extra sporelings
	"ada_voss": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Government: Enhances turret accuracy
	"blaze_quinn": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Raiders: Increases vehicle speed
	# Commanders (10 per faction, player-characters with traits and perks)
	# Survivors
	"field_medic": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Units heal over time; Perk: Heal all units instantly
	"tactician": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Faster unit deployment; Perk: Double unit production for 30s
	"scout_master": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Reveals map fog; Perk: Detect enemy bases
	"quartermaster": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: +10% resource gain; Perk: Double scavenging yield
	"fortifier": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Buildings +20% HP; Perk: Instant build repair
	"engineer_prime": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Faster build times; Perk: Free building placement
	"survivalist": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Units survive longer; Perk: Revive fallen units
	"recycler": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: +15% ore from recycling; Perk: Convert iron to ore
	"warlord": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Units +10% damage; Perk: Area damage burst
	"pathfinder": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Faster unit movement; Perk: Teleport units to node
	# Monsters
	"swarm_caller": {"bio-mass": 5000, "food": 1000, "power": 2000}, # Trait: Extra unit spawns; Perk: Summon swarm
	"plague_weaver": {"bio-mass": 5000, "food": 1000, "power": 2000}, # Trait: Enemy units take DoT; Perk: Plague cloud attack
	"hive_mind": {"bio-mass": 5000, "food": 1000, "power": 2000}, # Trait: Units share health; Perk: Heal all units
	"spore_lord": {"bio-mass": 5000, "food": 1000, "power": 2000}, # Trait: +10% bio-mass gain; Perk: Double sporeling production
	"burrower": {"bio-mass": 5000, "food": 1000, "power": 2000}, # Trait: Units can burrow; Perk: Ambush from underground
	"toxin_master": {"bio-mass": 5000, "food": 1000, "power": 2000}, # Trait: Units poison enemies; Perk: Toxic area denial
	"alpha_beast": {"bio-mass": 5000, "food": 1000, "power": 2000}, # Trait: Units +15% HP; Perk: Summon elite brute
	"mind_flayer": {"bio-mass": 5000, "food": 1000, "power": 2000}, # Trait: Enemy units slowed; Perk: Mind control enemy unit
	"regenerator": {"bio-mass": 5000, "food": 1000, "power": 2000}, # Trait: Buildings regenerate; Perk: Instant building heal
	"stalker_prime": {"bio-mass": 5000, "food": 1000, "power": 2000}, # Trait: Units +10% speed; Perk: Invisibility for units
	# Government
	"field_commander": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Units +10% accuracy; Perk: Precision strike
	"drone_operator": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Drones auto-repair; Perk: Deploy drone swarm
	"logistician": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Faster resource transport; Perk: Instant resource delivery
	"strategist": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Faster tech research; Perk: Free tech upgrade
	"commando": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Units +10% crit chance; Perk: Stealth attack
	"artillery_expert": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Turrets +15% range; Perk: Barrage strike
	"intel_officer": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Reveal enemy movements; Perk: Disable enemy building
	"engineer_corps": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Faster repairs; Perk: Instant fortification
	"tank_commander": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Vehicles +10% armor; Perk: Tank shield
	"radar_specialist": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: +20% radar range; Perk: Jam enemy radar
	# Raiders
	"demo_expert": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Explosives +15% damage; Perk: Massive bomb
	"scout_leader": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Units +10% speed; Perk: Scout ambush
	"smuggler": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: +10% trade profits; Perk: Smuggle rare resources
	"raider_king": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Units +10% attack speed; Perk: Raid enemy stockpile
	"saboteur_elite": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Sabotage lasts longer; Perk: Disable enemy defenses
	"mechanic": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Vehicles repair faster; Perk: Instant vehicle repair
	"outlaw": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Units evade attacks; Perk: Cloak units
	"pyro": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Units cause fire damage; Perk: Firestorm attack
	"scrapper": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: +15% scrap from enemies; Perk: Convert enemy units to resources
	"nomad": {"credits": 5000, "food": 1000, "iron": 500, "ore": 2000, "power": 2000}, # Trait: Faster base relocation; Perk: Instant base move
	# Facilities (15 per faction)
	# Survivors
	"recycling_plant": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 2000},
	"solar_array": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # +20% power generation
	"water_purifier": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1200}, # +15% food production
	"armory": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # Units +10% damage
	"medbay": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Units heal faster
	"radar_station": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # Reveals map areas
	"bunker_network": {"credits": 5000, "iron": 1500, "ore": 15000, "power": 2500}, # +20% building HP
	"supply_depot": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1200}, # +10% resource storage
	"training_ground": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Faster unit training
	"forge": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # +15% iron production
	"comm_tower": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # Faster AI-chat responses
	"scrap_processor": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # +10% ore from scavenging
	"defense_grid": {"credits": 5000, "iron": 1500, "ore": 15000, "power": 2500}, # Auto-turret deployment
	"research_lab": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # Faster tech research
	"survival_cache": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1200}, # Stores rare relics
	# Monsters
	"bio_reactor": {"bio-mass": 4000, "food": 1000, "power": 2000}, # +20% bio-mass production
	"fungal_grove": {"bio-mass": 3500, "food": 800, "power": 1500}, # +15% food production
	"spore_launcher": {"bio-mass": 4500, "food": 1200, "power": 2000}, # Units gain ranged attack
	"regrowth_chamber": {"bio-mass": 4000, "food": 1000, "power": 1800}, # Units regenerate
	"sense_organ": {"bio-mass": 3500, "food": 800, "power": 1500}, # Detects enemy movements
	"chitin_fortress": {"bio-mass": 5000, "food": 1500, "power": 2500}, # +20% building HP
	"nutrient_pool": {"bio-mass": 3000, "food": 700, "power": 1200}, # +10% resource storage
	"spawning_pit": {"bio-mass": 4000, "food": 1000, "power": 1800}, # Faster unit spawning
	"toxin_vat": {"bio-mass": 4500, "food": 1200, "power": 2000}, # Units poison enemies
	"neuro_link": {"bio-mass": 3500, "food": 800, "power": 1500}, # Faster AI-chat responses
	"biomass_converter": {"bio-mass": 4000, "food": 1000, "power": 1800}, # +10% bio-mass from enemies
	"spike_trap": {"bio-mass": 5000, "food": 1500, "power": 2500}, # Auto-damage to enemies
	"evolution_chamber": {"bio-mass": 4500, "food": 1200, "power": 2000}, # Faster tech research
	"core_nexus": {"bio-mass": 3000, "food": 700, "power": 1200}, # Stores data cores
	"swarm_hive": {"bio-mass": 4000, "food": 1000, "power": 1800}, # +10% unit speed
	# Government
	"power_plant": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 2000}, # +20% power generation
	"hydro_farm": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # +15% food production
	"missile_silo": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # Units gain missile attack
	"field_hospital": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Units heal faster
	"surveillance_hub": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # Reveals enemy positions
	"fortified_base": {"credits": 5000, "iron": 1500, "ore": 15000, "power": 2500}, # +20% building HP
	"supply_cache": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1200}, # +10% resource storage
	"boot_camp": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Faster unit training
	"foundry": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # +15% iron production
	"comm_array": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # Faster AI-chat responses
	"salvage_yard": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # +10% ore from enemies
	"auto_turret": {"credits": 5000, "iron": 1500, "ore": 15000, "power": 2500}, # Auto-defense system
	"tech_center": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # Faster tech research
	"relic_vault": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1200}, # Stores relics
	"drone_bay": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # +10% unit speed
	# Raiders
	"generator": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 2000}, # +20% power generation
	"food_silo": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # +15% food production
	"explosives_cache": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # Units gain explosive attack
	"infirmary": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Units heal faster
	"scout_post": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # Reveals enemy positions
	"stronghold": {"credits": 5000, "iron": 1500, "ore": 15000, "power": 2500}, # +20% building HP
	"stockpile": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1200}, # +10% resource storage
	"raider_den": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Faster unit training
	"smelter": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # +15% iron production
	"relay_tower": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # Faster AI-chat responses
	"scrap_heap": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # +10% ore from enemies
	"trap_network": {"credits": 5000, "iron": 1500, "ore": 15000, "power": 2500}, # Auto-damage to enemies
	"tech_workshop": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # Faster tech research
	"loot_vault": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1200}, # Stores relics
	"speed_garage": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # +10% unit speed
	# Superweapons (2 per faction)
	"scrapstorm_barrage": {"credits": 10000, "iron": 20000, "ore": 20000, "food": 100000, "power": 5000}, # Survivors: Massive area damage
	"ion_cannon": {"credits": 10000, "iron": 20000, "ore": 20000, "food": 100000, "power": 5000}, # Survivors: Precision strike
	"bio_plague_eruption": {"bio-mass": 20000, "food": 40000, "iron": 20000, "power": 5000}, # Monsters: Poison cloud
	"spore_nova": {"bio-mass": 20000, "food": 40000, "iron": 20000, "power": 5000}, # Monsters: Spawns enemy-disrupting spores
	"orbital_strike": {"credits": 10000, "iron": 20000, "ore": 20000, "food": 100000, "power": 5000}, # Government: Satellite-based attack
	"EMP_pulse": {"credits": 10000, "iron": 20000, "ore": 20000, "food": 100000, "power": 5000}, # Government: Disables enemy electronics
	"junkyard_apocalypse": {"credits": 10000, "iron": 20000, "ore": 20000, "food": 100000, "power": 5000}, # Raiders: Massive explosive chaos
	"raider_swarm": {"credits": 10000, "iron": 20000, "ore": 20000, "food": 100000, "power": 5000}, # Raiders: Deploys fast attack units
	# Tech (20 per faction)
	# Survivors
	"infantry_training": {"credits": 2000, "food": 500, "ore": 5000, "power": 1000}, # +10% infantry damage
	"vehicle_armor": {"credits": 3000, "iron": 750, "ore": 7500, "power": 1500}, # +15% vehicle HP
	"superweapon_efficiency": {"credits": 5000, "energy": 1000, "ore": 10000, "power": 2000}, # -10% superweapon cost
	"scavenging_tech": {"credits": 2500, "food": 600, "ore": 6000, "power": 1200}, # +15% scavenging yield
	"power_optimizer": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1500}, # +10% power efficiency
	"fortification_tech": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # +20% building durability
	"recycling_efficiency": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # +15% ore from recycling
	"combat_training": {"credits": 3000, "food": 700, "ore": 7000, "power": 1500}, # +10% unit attack speed
	"resource_logistics": {"credits": 2500, "iron": 600, "ore": 6000, "power": 1200}, # +10% resource transport speed
	"survival_gear": {"credits": 4000, "food": 1000, "ore": 10000, "power": 1800}, # Units survive longer
	"energy_shields": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # Units gain shields
	"stealth_tech": {"credits": 5000, "iron": 1500, "ore": 15000, "power": 2500}, # Units gain partial invisibility
	"rapid_deployment": {"credits": 3000, "food": 700, "ore": 7000, "power": 1500}, # Faster unit deployment
	"ore_refinery": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # +10% ore refining
	"comm_upgrades": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1200}, # Faster AI-chat responses
	"armor_piercing": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Units ignore 10% enemy armor
	"scout_drones": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # Reveals map areas
	"repair_bots": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Buildings auto-repair
	"food_synthesis": {"credits": 3000, "food": 600, "ore": 6000, "power": 1200}, # +10% food production
	"tactical_hud": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # +10% unit accuracy
	# Monsters (similar structure, themed differently)
	"spore_enhancement": {"bio-mass": 2000, "food": 500, "power": 1000}, # +10% unit damage
	"chitin_armor": {"bio-mass": 3000, "food": 750, "power": 1500}, # +15% unit HP
	"bio_weapon_efficiency": {"bio-mass": 5000, "food": 1000, "power": 2000}, # -10% superweapon cost
	"spore_harvesting": {"bio-mass": 2500, "food": 600, "power": 1200}, # +15% bio-mass yield
	"bio_power": {"bio-mass": 3000, "food": 700, "power": 1500}, # +10% power efficiency
	"hive_defenses": {"bio-mass": 4000, "food": 1000, "power": 1800}, # +20% building durability
	"toxin_refinery": {"bio-mass": 3500, "food": 800, "power": 1500}, # +15% bio-mass from enemies
	"swarm_training": {"bio-mass": 3000, "food": 700, "power": 1500}, # +10% unit attack speed
	"nutrient_flow": {"bio-mass": 2500, "food": 600, "power": 1200}, # +10% resource transport speed
	"regeneration": {"bio-mass": 4000, "food": 1000, "power": 1800}, # Units regenerate
	"bio_shields": {"bio-mass": 4500, "food": 1200, "power": 2000}, # Units gain shields
	"camouflage": {"bio-mass": 5000, "food": 1500, "power": 2500}, # Units gain partial invisibility
	"rapid_spawning": {"bio-mass": 3000, "food": 700, "power": 1500}, # Faster unit spawning
	"bio_refinery": {"bio-mass": 3500, "food": 800, "power": 1500}, # +10% bio-mass refining
	"neuro_comm": {"bio-mass": 3000, "food": 700, "power": 1200}, # Faster AI-chat responses
	"venomous_claws": {"bio-mass": 4000, "food": 1000, "power": 1800}, # Units ignore 10% enemy armor
	"sense_drones": {"bio-mass": 3500, "food": 800, "power": 1500}, # Reveals map areas
	"auto_regrowth": {"bio-mass": 4000, "food": 1000, "power": 1800}, # Buildings auto-repair
	"nutrient_synthesis": {"bio-mass": 3000, "food": 600, "power": 1200}, # +10% food production
	"swarm_sense": {"bio-mass": 4500, "food": 1200, "power": 2000}, # +10% unit accuracy
	# Government (similar structure)
	"rifle_training": {"credits": 2000, "food": 500, "ore": 5000, "power": 1000}, # +10% infantry damage
	"tank_armor": {"credits": 3000, "iron": 750, "ore": 7500, "power": 1500}, # +15% vehicle HP
	"orbital_efficiency": {"credits": 5000, "energy": 1000, "ore": 10000, "power": 2000}, # -10% superweapon cost
	"salvage_tech": {"credits": 2500, "food": 600, "ore": 6000, "power": 1200}, # +15% scavenging yield
	"power_grid": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1500}, # +10% power efficiency
	"bunker_tech": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # +20% building durability
	"recycling_tech": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # +15% ore from recycling
	"combat_drills": {"credits": 3000, "food": 700, "ore": 7000, "power": 1500}, # +10% unit attack speed
	"supply_chain": {"credits": 2500, "iron": 600, "ore": 6000, "power": 1200}, # +10% resource transport speed
	"survival_training": {"credits": 4000, "food": 1000, "ore": 10000, "power": 1800}, # Units survive longer
	"energy_barriers": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # Units gain shields
	"stealth_systems": {"credits": 5000, "iron": 1500, "ore": 15000, "power": 2500}, # Units gain partial invisibility
	"rapid_response": {"credits": 3000, "food": 700, "ore": 7000, "power": 1500}, # Faster unit deployment
	"ore_processor": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # +10% ore refining
	"comm_network": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1200}, # Faster AI-chat responses
	"ap_rounds": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Units ignore 10% enemy armor
	"drone_scouts": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # Reveals map areas
	"auto_repair": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Buildings auto-repair
	"food_production": {"credits": 3000, "food": 600, "ore": 6000, "power": 1200}, # +10% food production
	"targeting_systems": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # +10% unit accuracy
	# Raiders
	"raider_training": {"credits": 2000, "food": 500, "ore": 5000, "power": 1000}, # +10% infantry damage
	"vehicle_plating": {"credits": 3000, "iron": 750, "ore": 7500, "power": 1500}, # +15% vehicle HP
	"apocalypse_efficiency": {"credits": 5000, "energy": 1000, "ore": 10000, "power": 2000}, # -10% superweapon cost
	"loot_tech": {"credits": 2500, "food": 600, "ore": 6000, "power": 1200}, # +15% scavenging yield
	"power_rig": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1500}, # +10% power efficiency
	"stronghold_tech": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # +20% building durability
	"scrap_tech": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # +15% ore from recycling
	"raid_training": {"credits": 3000, "food": 700, "ore": 7000, "power": 1500}, # +10% unit attack speed
	"smuggling_routes": {"credits": 2500, "iron": 600, "ore": 6000, "power": 1200}, # +10% resource transport speed
	"survival_kit": {"credits": 4000, "food": 1000, "ore": 10000, "power": 1800}, # Units survive longer
	"jury_rigged_shields": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # Units gain shields
	"cloak_tech": {"credits": 5000, "iron": 1500, "ore": 15000, "power": 2500}, # Units gain partial invisibility
	"quick_strike": {"credits": 3000, "food": 700, "ore": 7000, "power": 1500}, # Faster unit deployment
	"ore_smelter": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # +10% ore refining
	"raider_comm": {"credits": 3000, "iron": 700, "ore": 7000, "power": 1200}, # Faster AI-chat responses
	"piercing_ammo": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Units ignore 10% enemy armor
	"scout_rigs": {"credits": 3500, "iron": 800, "ore": 8000, "power": 1500}, # Reveals map areas
	"auto_salvage": {"credits": 4000, "iron": 1000, "ore": 10000, "power": 1800}, # Buildings auto-repair
	"food_rations": {"credits": 3000, "food": 600, "ore": 6000, "power": 1200}, # +10% food production
	"targeting_rigs": {"credits": 4500, "iron": 1200, "ore": 12000, "power": 2000}, # +10% unit accuracy
	# Commander Upgrades (rare resources)
	"commander_upgrade_1": {"relics": 1000, "power": 1000}, # +5% to all traits
	"commander_upgrade_2": {"relics": 2000, "power": 2000}, # +10% to all traits
	"commander_perk_boost": {"relics": 3000, "power": 3000}, # +20% perk effectiveness
	"monster_upgrade_1": {"data_cores": 1000, "power": 1000}, # +5% to all traits
	"monster_upgrade_2": {"data_cores": 2000, "power": 2000}, # +10% to all traits
	"monster_perk_boost": {"data_cores": 3000, "power": 3000} # +20% perk effectiveness
}

# Easter eggs for rare resources (10 per faction)
var easter_eggs = {
	"Survivors": [
		{"id": "hidden_cache", "reward": {"relics": 500}, "task": "Visit map coordinates (100,100) with a scavenger"},
		{"id": "old_vault", "reward": {"relics": 500}, "task": "Build 3 scrapyards in one game"},
		{"id": "lost_relic", "reward": {"relics": 1000}, "task": "Defeat 50 enemies with survivors"},
		{"id": "secret_code", "reward": {"relics": 500}, "task": "Enter '/relic' in AI-chat"},
		{"id": "ancient_ruins", "reward": {"relics": 1000}, "task": "Capture 5 nodes in World-Game"},
		{"id": "scavenger_lore", "reward": {"relics": 500}, "task": "Complete AI-chat quest chain"},
		{"id": "survivor_pact", "reward": {"relics": 1000}, "task": "Build 10 units in one match"},
		{"id": "ghost_signal", "reward": {"relics": 500}, "task": "Activate radar_station at night"},
		{"id": "buried_tech", "reward": {"relics": 1000}, "task": "Research 5 techs in one game"},
		{"id": "lone_wanderer", "reward": {"relics": 500}, "task": "Move a single unit to map edge"}
	],
	"Monsters": [
		{"id": "spore_cluster", "reward": {"data_cores": 500}, "task": "Spawn 100 sporelings"},
		{"id": "toxic_pool", "reward": {"data_cores": 500}, "task": "Build 3 hive_cores"},
		{"id": "alpha_nest", "reward": {"data_cores": 1000}, "task": "Defeat 50 enemies with brutes"},
		{"id": "dark_signal", "reward": {"data_cores": 500}, "task": "Enter '/core' in AI-chat"},
		{"id": "node_hive", "reward": {"data_cores": 1000}, "task": "Capture 5 nodes in World-Game"},
		{"id": "swarm_lore", "reward": {"data_cores": 500}, "task": "Complete AI-chat quest chain"},
		{"id": "spore_pact", "reward": {"data_cores": 1000}, "task": "Spawn 10 units in one match"},
		{"id": "night_stalk", "reward": {"data_cores": 500}, "task": "Activate sense_organ at night"},
		{"id": "bio_cache", "reward": {"data_cores": 1000}, "task": "Research 5 techs in one game"},
		{"id": "lone_predator", "reward": {"data_cores": 500}, "task": "Move a single stalker to map edge"}
	],
	"Government": [
		{"id": "secret_bunker", "reward": {"relics": 500}, "task": "Visit map coordinates (200,200) with a rifleman"},
		{"id": "old_arsenal", "reward": {"relics": 500}, "task": "Build 3 command_centers"},
		{"id": "elite_cache", "reward": {"relics": 1000}, "task": "Defeat 50 enemies with snipers"},
		{"id": "code_drop", "reward": {"relics": 500}, "task": "Enter '/relic' in AI-chat"},
		{"id": "node_control", "reward": {"relics": 1000}, "task": "Capture 5 nodes in World-Game"},
		{"id": "intel_lore", "reward": {"relics": 500}, "task": "Complete AI-chat quest chain"},
		{"id": "army_pact", "reward": {"relics": 1000}, "task": "Build 10 units in one match"},
		{"id": "radar_pulse", "reward": {"relics": 500}, "task": "Activate surveillance_hub at night"},
		{"id": "tech_vault", "reward": {"relics": 1000}, "task": "Research 5 techs in one game"},
		{"id": "lone_soldier", "reward": {"relics": 500}, "task": "Move a single tank to map edge"}
	],
	"Raiders": [
		{"id": "hidden_loot", "reward": {"relics": 500}, "task": "Visit map coordinates (300,300) with a marauder"},
		{"id": "scrap_pile", "reward": {"relics": 500}, "task": "Build 3 raider_camps"},
		{"id": "raider_cache", "reward": {"relics": 1000}, "task": "Defeat 50 enemies with gunners"},
		{"id": "secret_stash", "reward": {"relics": 500}, "task": "Enter '/relic' in AI-chat"},
		{"id": "node_raid", "reward": {"relics": 1000}, "task": "Capture 5 nodes in World-Game"},
		{"id": "raider_lore", "reward": {"relics": 500}, "task": "Complete AI-chat quest chain"},
		{"id": "bandit_pact", "reward": {"relics": 1000}, "task": "Build 10 units in one match"},
		{"id": "scout_signal", "reward": {"relics": 500}, "task": "Activate scout_post at night"},
		{"id": "loot_tech", "reward": {"relics": 1000}, "task": "Research 5 techs in one game"},
		{"id": "lone_bandit", "reward": {"relics": 500}, "task": "Move a single buggy to map edge"}
	]
}

# Game events for rare resources
var game_events = [
	{"id": "winter_festival", "reward": {"relics": 1000, "data_cores": 1000}, "task": "Complete 5 winter-themed missions in December"},
	{"id": "summer_raid", "reward": {"relics": 1000, "data_cores": 1000}, "task": "Win 3 PvP matches in July"},
	{"id": "halloween_surge", "reward": {"relics": 1500, "data_cores": 1500}, "task": "Defeat 100 zombie enemies in October"},
	{"id": "spring_rebirth", "reward": {"relics": 1000, "data_cores": 1000}, "task": "Build 5 facilities in April"},
	{"id": "tournament_glory", "reward": {"relics": 2000, "data_cores": 2000}, "task": "Rank top 10 in monthly tournament"}
]

func _ready():
	# Initialize economy and sync with JavaScript
	if Engine.has_singleton("JavaScript"):
		var js = Engine.get_singleton("JavaScript")
		js.connect("updateGameState", Callable(self, "_on_update_game_state"))
	# Ensure initial state is synced
	get_node("/root/GameScene").set_game_state(JSON.stringify(factions))

func _process(delta):
	# Update resources and power based on faction-specific logic
	for faction in factions:
		var efficiency = get_node("/root/GameScene").calculate_efficiency(faction)
		# Power consumption: 1 ore per second generates 5 power units per building
		var power_consumption = (factions[faction]["buildings"].size() + factions[faction]["facilities"].size()) * 50
		var ore_consumed = power_consumption * delta / 5
		factions[faction]["resources"]["ore"] -= ore_consumed
		factions[faction]["resources"]["power"] += power_consumption * delta
		if factions[faction]["resources"]["ore"] < 0 or factions[faction]["resources"]["power"] < 0:
			factions[faction]["resources"]["power"] = 0
			get_node("/root/GameScene/UILayer").update_ui(faction, "alert", {"message": "Power shortage! Operations halted."})
		for unit_type in factions[faction]["units"]:
			var unit_count = factions[faction]["units"][unit_type]
			if unit_count > 0:
				if faction == "Monsters":
					factions[faction]["resources"]["bio-mass"] += unit_count * efficiency[unit_type] * 15 * delta
					factions[faction]["resources"]["food"] += unit_count * efficiency[unit_type] * 3 * delta
					factions[faction]["resources"]["data_cores"] += unit_count * efficiency[unit_type] * 0.01 * delta
				else:
					factions[faction]["resources"]["ore"] += unit_count * efficiency[unit_type] * 15 * delta
					factions[faction]["resources"]["energy"] += unit_count * efficiency[unit_type] * 5 * delta
					factions[faction]["resources"]["credits"] += unit_count * efficiency[unit_type] * 3 * delta
					factions[faction]["resources"]["food"] += unit_count * efficiency[unit_type] * 3 * delta
					factions[faction]["resources"]["relics"] += unit_count * efficiency[unit_type] * 0.01 * delta
		# Apply facility bonuses
		if "recycling_plant" in factions[faction]["facilities"]:
			factions[faction]["resources"]["ore"] += 0.1 * factions[faction]["resources"]["ore"] * delta
		if "bio_reactor" in factions[faction]["facilities"]:
			factions[faction]["resources"]["bio-mass"] += 0.1 * factions[faction]["resources"]["bio-mass"] * delta
		if "power_plant" in factions[faction]["facilities"]:
			factions[faction]["resources"]["power"] += 0.1 * factions[faction]["resources"]["power"] * delta
		if "generator" in factions[faction]["facilities"]:
			factions[faction]["resources"]["power"] += 0.1 * factions[faction]["resources"]["power"] * delta
	# Sync with JavaScript every 5 seconds
	if Engine.get_frames_drawn() % 300 == 0:
		if Engine.has_singleton("JavaScript"):
			var js = Engine.get_singleton("JavaScript")
			js.call("updateGameState", JSON.stringify(factions))

func refine_ore_to_iron(faction, amount):
	# Refining: 2 ore -> 1 iron
	if factions[faction]["resources"]["ore"] >= amount * 2:
		factions[faction]["resources"]["ore"] -= amount * 2
		factions[faction]["resources"]["iron"] = factions[faction]["resources"].get("iron", 0) + amount
		get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
		return true
	return false

func refine_ore_to_credits(faction, amount):
	# Refining: 5 ore -> 1 credit
	if factions[faction]["resources"]["ore"] >= amount * 5:
		factions[faction]["resources"]["ore"] -= amount * 5
		factions[faction]["resources"]["credits"] = factions[faction]["resources"].get("credits", 0) + amount
		get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
		return true
	return false

func check_power(faction, required_power):
	if factions[faction]["resources"]["power"] >= required_power:
		factions[faction]["resources"]["power"] -= required_power
		return true
	get_node("/root/GameScene/UILayer").update_ui(faction, "alert", {"message": "Insufficient power for operation!"})
	return false

func deploy_unit(faction, unit_type):
	if unit_type in costs:
		var cost = costs[unit_type]
		var has_resources = true
		for resource in cost:
			if factions[faction]["resources"].get(resource, 0) < cost[resource]:
				has_resources = false
				break
		if has_resources and check_power(faction, cost["power"]):
			for resource in cost:
				factions[faction]["resources"][resource] -= cost[resource]
			factions[faction]["units"][unit_type] = factions[faction]["units"].get(unit_type, 0) + 1
			get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
			return true
	return false

func build_structure(faction, structure):
	if structure in costs:
		var cost = costs[structure]
		var has_resources = true
		for resource in cost:
			if factions[faction]["resources"].get(resource, 0) < cost[resource]:
				has_resources = false
				break
		if has_resources and check_power(faction, cost["power"]):
			for resource in cost:
				factions[faction]["resources"][resource] -= cost[resource]
			factions[faction]["buildings"].append(structure)
			get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
			return true
	return false

func build_facility(faction, facility):
	if facility in costs:
		var cost = costs[facility]
		var has_resources = true
		for resource in cost:
			if factions[faction]["resources"].get(resource, 0) < cost[resource]:
				has_resources = false
				break
		if has_resources and check_power(faction, cost["power"]):
			for resource in cost:
				factions[faction]["resources"][resource] -= cost[resource]
			factions[faction]["facilities"].append(facility)
			get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
			return true
	return false

func deploy_hero(faction, hero):
	if hero in costs:
		var cost = costs[hero]
		var has_resources = true
		for resource in cost:
			if factions[faction]["resources"].get(resource, 0) < cost[resource]:
				has_resources = false
				break
		if has_resources and check_power(faction, cost["power"]):
			for resource in cost:
				factions[faction]["resources"][resource] -= cost[resource]
			factions[faction]["heroes"].append(hero)
			get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
			return true
	return false

func deploy_commander(faction, commander):
	if commander in costs:
		var cost = costs[commander]
		var has_resources = true
		for resource in cost:
			if factions[faction]["resources"].get(resource, 0) < cost[resource]:
				has_resources = false
				break
		if has_resources and check_power(faction, cost["power"]):
			for resource in cost:
				factions[faction]["resources"][resource] -= cost[resource]
			factions[faction]["commanders"].append(commander)
			get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
			return true
	return false

func upgrade_commander(faction, upgrade):
	if upgrade in costs:
		var cost = costs[upgrade]
		var has_resources = true
		for resource in cost:
			if factions[faction]["resources"].get(resource, 0) < cost[resource]:
				has_resources = false
				break
		if has_resources and check_power(faction, cost["power"]):
			for resource in cost:
				factions[faction]["resources"][resource] -= cost[resource]
			factions[faction]["commander_upgrades"] = factions[faction].get("commander_upgrades", [])
			factions[faction]["commander_upgrades"].append(upgrade)
			get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
			return true
	return false

func activate_superweapon(faction, superweapon):
	if superweapon in costs:
		var cost = costs[superweapon]
		var has_resources = true
		for resource in cost:
			if factions[faction]["resources"].get(resource, 0) < cost[resource]:
				has_resources = false
				break
		if has_resources and check_power(faction, cost["power"]):
			for resource in cost:
				factions[faction]["resources"][resource] -= cost[resource]
			factions[faction]["superweapons"].append(superweapon)
			get_node("/root/GameScene").trigger_superweapon(faction, superweapon)
			get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
			return true
	return false

func research_tech(faction, tech):
	if tech in costs:
		var cost = costs[tech]
		var has_resources = true
		for resource in cost:
			if factions[faction]["resources"].get(resource, 0) < cost[resource]:
				has_resources = false
				break
		if has_resources and check_power(faction, cost["power"]):
			for resource in cost:
				factions[faction]["resources"][resource] -= cost[resource]
			factions[faction]["tech"].append(tech)
			get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
			return true
	return false

func trade_resources(faction, resource, amount):
	if factions[faction]["resources"].get(resource, 0) >= amount and check_power(faction, 500):
		factions[faction]["resources"][resource] -= amount
		factions[faction]["resources"]["credits"] = factions[faction]["resources"].get("credits", 0) + amount * 2
		factions[faction]["resources"]["power"] -= 500
		get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
		return true
	return false

func scavenge(faction):
	if check_power(faction, 500):
		factions[faction]["resources"]["ore"] = factions[faction]["resources"].get("ore", 0) + (randi() % 500 + 250)
		factions[faction]["resources"]["energy"] = factions[faction]["resources"].get("energy", 0) + (randi() % 250 + 150)
		factions[faction]["resources"]["food"] = factions[faction]["resources"].get("food", 0) + (randi() % 200 + 100)
		factions[faction]["resources"]["relics"] = factions[faction]["resources"].get("relics", 0) + (randi() % 10 + 5)
		factions[faction]["resources"]["data_cores"] = factions[faction]["resources"].get("data_cores", 0) + (randi() % 10 + 5)
		factions[faction]["resources"]["power"] -= 500
		get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
		return true
	return false

func trigger_easter_egg(faction, egg_id):
	if egg_id in easter_eggs[faction]:
		var reward = easter_eggs[faction][egg_id]["reward"]
		for resource in reward:
			factions[faction]["resources"][resource] += reward[resource]
		get_node("/root/GameScene/UILayer").update_ui(faction, "alert", {"message": "Easter Egg found: " + easter_eggs[faction][egg_id]["task"]})
		get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
		return true
	return false

func trigger_game_event(faction, event_id):
	for event in game_events:
		if event["id"] == event_id:
			var reward = event["reward"]
			for resource in reward:
				factions[faction]["resources"][resource] += reward[resource]
			get_node("/root/GameScene/UILayer").update_ui(faction, "alert", {"message": "Event completed: " + event["task"]})
			get_node("/root/GameScene").set_game_state(JSON.stringify(factions))
			return true
	return false

func start_world_game(faction):
	# Initialize procedural World-Game PvP mode
	var map = generate_procedural_map()
	get_node("/root/GameScene").start_pvp_mode(faction, map)
	get_node("/root/GameScene/UILayer").update_ui(faction, "alert", {"message": "World-Game PvP mode started! Capture nodes to win."})
	return true

func generate_procedural_map():
	# Procedural map generation for World-Game
	var map = {
		"nodes": [],
		"terrain": [],
		"events": []
	}
	for i in range(15):
		map["nodes"].append({
			"id": "node_" + str(i),
			"position": Vector2(randi() % 1000, randi() % 1000),
			"resources": {"ore": randi() % 500 + 250, "relics": randi() % 10 + 5}
		})
	for i in range(20):
		map["terrain"].append({
			"type": ["plains", "desert", "swamp", "ruins"].pick_random(),
			"position": Vector2(randi() % 1000, randi() % 1000)
		})
	for i in range(5):
		map["events"].append({
			"type": ["meteor_strike", "rogue_ai", "resource_surge"].pick_random(),
			"time": randi() % 300 + 60
		})
	return map

func _on_update_game_state(json_string):
	var data = JSON.parse_string(json_string)
	factions = data
	get_node("/root/GameScene").set_game_state(JSON.stringify(factions))

% AI-Driven Story and Dialogue System
% Simulates a unique post-apocalyptic game with randomized NPC interactions, questlines, and world-altering random encounters.

function ai_driven_story_dialogue_system()

    % --- Game World Configuration ---
    % Define factions, locations, and lore elements
    factions = {
        'Neon Nomads', 'Scavenger collective valuing freedom and tech scavenging';
        'Iron Syndicate', 'Militaristic group controlling resource hubs';
        'Wasteland Sages', 'Mystical scholars preserving pre-apocalypse knowledge';
        'Rogue Drones', 'AI-controlled drones seeking autonomy'
    };
    locations = {
        'Neon Spire', 'Ruined skyscraper with flickering holographic displays';
        'Rust Basin', 'Industrial wasteland with toxic pools';
        'Sage Hollow', 'Hidden oasis with ancient archives';
        'Drone Nexus', 'Abandoned AI facility with rogue machinery'
    };
    world_state = {
        resource_level: 50, % Global resource availability (0-100)
        tension_level: 30, % Faction conflict level (0-100)
        tech_level: 20, % Technology advancement (0-100)
        stability: 60 % Overall world stability (0-100)
    };

    % --- NPC and Dialogue Configuration ---
    % Define NPC archetypes with randomized traits
    npc_archetypes = {
        'Wanderer', {mood: {'curious', 'wary', 'hopeful'}, goal: 'find_safe_haven'};
        'Mercenary', {mood: {'aggressive', 'pragmatic', 'greedy'}, goal: 'secure_contract'};
        'Scholar', {mood: {'inquisitive', 'cautious', 'idealistic'}, goal: 'preserve_knowledge'};
        'Drone', {mood: {'erratic', 'logical', 'hostile'}, goal: 'achieve_autonomy'}
    };
    dialogue_templates = {
        'greeting': {
            curious: {'Hello, stranger. Seen any old tech worth salvaging?', 'What brings you to this forsaken place?'},
            wary: {'Keep your distance. What do you want?', 'I don’t trust outsiders. State your business.'},
            hopeful: {'Maybe you’re the one to change things around here.', 'Got any news from the wastes?'},
            aggressive: {'You looking for trouble? Cause I can deliver.', 'Step lightly, or you’re done.'},
            pragmatic: {'Got something to trade? Time’s valuable.', 'What’s your angle, traveler?'},
            inquisitive: {'What’s the story behind that gear?', 'Ever seen the Sage archives?'},
            erratic: {'*whirr* Identify: friend or foe?', 'System unstable. Query: purpose?'},
            hostile: {'Intruder detected. Leave or terminate.', 'You’re not welcome here.'}
        },
        'quest_offer': {
            curious: {'I heard of a buried tech cache in Rust Basin. Want in?', 'There’s a strange signal in Drone Nexus. Care to check it?'},
            pragmatic: {'Iron Syndicate’s paying for scout reports. You game?', 'Need someone to smuggle parts past Nomads. Good caps.'},
            idealistic: {'The Sages need help preserving an archive. You in?', 'We can rebuild if we save the old knowledge. Join us?'},
            hostile: {'Take out a Nomad camp, or you’re next.', 'Sabotage the Syndicate’s rig, or else.'}
        },
        'reaction': {
            positive: {'That’s the spirit! Let’s make this work.', 'Didn’t expect that—nice move.'},
            negative: {'You’re making a mistake, friend.', 'That’s gonna cost you.'},
            neutral: {'Alright, your call.', 'Let’s see where this goes.'}
        }
    };

    % --- Player State Initialization ---
    player_state = {
        name: 'Drifter',
        level: 1,
        xp: 0,
        xp_for_next_level: 1000,
        stats: [5, 5, 5, 5, 5, 5, 5], % S.P.E.C.I.A.L.-inspired: Strength, Perception, Endurance, Charisma, Intelligence, Agility, Luck
        reputation: struct('NeonNomads', 0, 'IronSyndicate', 0, 'WastelandSages', 0, 'RogueDrones', 0), % -100 to 100
        inventory: {'scrap_metal', 10; 'energy_cell', 5},
        location: 'Neon Spire',
        active_quests: {}
    };

    % --- Random Encounter Configuration ---
    % Encounters influence world state, NPC behavior, and quest availability
    encounter_types = {
        'Tech Cache Discovery', {probability: 0.3, impact: {world_state.tech_level, +10; world_state.resource_level, +5}, description: 'Found a hidden pre-apocalypse tech cache.'};
        'Faction Skirmish', {probability: 0.2, impact: {world_state.tension_level, +15; world_state.stability, -10}, description: 'Stumbled into a Neon Nomad vs. Iron Syndicate clash.'};
        'Rogue Drone Attack', {probability: 0.15, impact: {player_state.stats(3), -1; world_state.tension_level, +5}, description: 'Ambushed by malfunctioning drones.'};
        'Sage Encounter', {probability: 0.25, impact: {player_state.reputation.WastelandSages, +10}, description: 'Met a Sage offering cryptic knowledge.'};
        'Resource Scarcity', {probability: 0.1, impact: {world_state.resource_level, -10; world_state.tension_level, +10}, description: 'Local resources depleted, tensions rise.'}
    };

    % --- Main Game Loop ---
    game_running = true;
    fprintf('Welcome to the Wasteland Chronicles!\n');
    fprintf('Character: %s (Level %d, Location: %s)\n', player_state.name, player_state.level, player_state.location);
    fprintf('----------------------------------------\n');

    while game_running
        % Trigger random encounter (30% chance per loop)
        if rand() < 0.3
            encounter = select_random_encounter(encounter_types);
            apply_encounter_effects(encounter, world_state, player_state);
            fprintf('Random Encounter: %s\n', encounter.description);
        end

        % Update NPC behavior based on world state
        npc = generate_random_npc(npc_archetypes, world_state, player_state);
        fprintf('\nYou meet a %s (%s, Mood: %s)\n', npc.archetype, npc.faction, npc.mood);

        % Display menu
        fprintf('\n--- Main Menu ---\n');
        fprintf('1. Interact with NPC\n');
        fprintf('2. View Character Sheet\n');
        fprintf('3. Gain Experience\n');
        fprintf('4. View World State\n');
        fprintf('5. Quit\n');
        choice = input('Enter your choice (1-5): ', 's');

        switch choice
            case '1'
                % Dynamic NPC interaction
                handle_npc_interaction(npc, dialogue_templates, player_state, world_state);
            case '2'
                display_character_sheet(player_state);
            case '3'
                xp_gained = input('Enter XP to gain: ');
                if isnumeric(xp_gained) && xp_gained > 0
                    player_state.xp = player_state.xp + xp_gained;
                    fprintf('Gained %d XP.\n', xp_gained);
                    while player_state.xp >= player_state.xp_for_next_level
                        player_state.level = player_state.level + 1;
                        fprintf('Level Up! You are now Level %d!\n', player_state.level);
                        stat_to_increase = randi(7);
                        player_state.stats(stat_to_increase) = min(player_state.stats(stat_to_increase) + 1, 10);
                        fprintf('Gained +1 %s!\n', {'Strength', 'Perception', 'Endurance', 'Charisma', 'Intelligence', 'Agility', 'Luck'}{stat_to_increase});
                        player_state.xp_for_next_level = player_state.xp_for_next_level + 1000 + (player_state.level - 1) * 500;
                    end
                else
                    fprintf('Invalid XP amount.\n');
                end
            case '4'
                display_world_state(world_state, factions, locations);
            case '5'
                fprintf('Thanks for playing Wasteland Chronicles!\n');
                game_running = false;
            otherwise
                fprintf('Invalid choice. Please enter a number between 1 and 5.\n');
        end
    end
end

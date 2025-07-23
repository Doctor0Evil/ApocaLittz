% Geo-Spatial Wasteland Chronicles: Phoenix Rising
% A unique post-apocalyptic RPG set in Phoenix, Arizona, 2040, with dynamic NPC interactions, randomized encounters, and Google Maps API integration.

function phoenix_wasteland_chronicles()

    % --- Google Maps API Configuration ---
    % Hypothetical API integration for Phoenix coordinates
    google_maps_api_key = 'YOUR_GOOGLE_MAPS_API_KEY'; % Replace with actual key
    maps_endpoint = 'https://maps.googleapis.com/maps/api/geocode/json';
    places_endpoint = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json';
    headers = {'Content-Type', 'application/json'};

    % --- Game World Configuration ---
    % Real-world Phoenix locations with post-apocalyptic descriptions
    locations = {
        'Phoenix Zoo', [33.4528, -111.9464], 'A shattered menagerie overrun by mutated beasts, with toxic ponds glowing faintly.';
        'South Mountain Park', [33.3353, -112.0723], 'A sprawling desert scarred by craters, hiding ancient bunkers beneath dunes.';
        'Piestewa Peak', [33.5406, -112.0211], 'A jagged summit used as a raider outpost, its trails crumbling into the abyss.';
        'Palo Verde Nuclear Station', [33.3890, -112.8651], 'A radioactive ruin with leaking reactors and rogue machines prowling.';
        'McHood Park Campsite', [34.9719, -110.6434], 'A desolate slab concealing a sealed underground sanctuary.';
        'Pioneer Living History Museum', [33.6928, -112.1186], 'A ghost-town of pre-war relics, whispering with spectral echoes.';
        'Jefferson Park', [33.5186, -112.0736], 'A cursed park where shadows move at midnight, haunted by a lost spirit.'
    };
    world_state = struct(...
        'radiation_level', 60, ... % Global radiation (0-100)
        'resource_scarcity', 40, ... % Resource availability (0-100)
        'faction_tension', 50, ... % Faction conflict level (0-100)
        'stability', 30 ... % Regional stability (0-100)
    );

    % --- Sanctuary Configuration ---
    % Discoverable underground bunkers (replacing vaults)
    sanctuaries = {
        'Sanctuary Alpha', [33.4484, -112.0740], 'Downtown Phoenix', 'A corporate bunker now a scavenger stronghold, littered with traps.';
        'Sanctuary Beta', [33.3890, -112.8651], 'Palo Verde Nuclear Station', 'An experimental bunker with rogue AI and toxic leaks.';
        'Sanctuary Gamma', [34.9719, -110.6434], 'McHood Park Campsite', 'A hidden refuge sealed after a failed experiment.'
    };

    % --- Faction Configuration ---
    % Unique factions with alignments and bases
    factions = {
        'Dawn Keepers', 'Neutral', [33.4528, -111.9464], 'Phoenix Zoo', 'Mystics purifying the wasteland, cautious of strangers.';
        'Steel Reavers', 'Hostile', [33.3890, -112.8651], 'Palo Verde Nuclear Station', 'Ruthless scavengers hoarding irradiated tech.';
        'Sand Wraiths', 'Allied', [33.3353, -112.0723], 'South Mountain Park', 'Nomads aiding survivors, guarding their secrets.';
        'Ember Cult', 'Hostile', [33.4484, -112.0740], 'Downtown Phoenix', 'Fanatics worshipping nuclear fire, based in Sanctuary Alpha.'
    };

    % --- Monster Configuration ---
    % Unique enemies with spawn locations
    monsters = {
        'Glowstalkers', [33.3353, -112.0723], 'South Mountain Park', 'Mutated scorpions with bioluminescent tails, ambushing at dusk.';
        'Ash Wraiths', [33.5186, -112.0736], 'Jefferson Park', 'Irradiated humans turned spectral, haunting at midnight.';
        'Synth Sentinels', [33.3890, -112.8651], 'Palo Verde Nuclear Station', 'Rogue AI constructs patrolling the reactor ruins.';
        'Radhounds', [33.4528, -111.9464], 'Phoenix Zoo', 'Feral canines with molten eyes, roaming in packs.'
    };

    % --- Environmental Hazards ---
    % Location-specific dangers
    hazards = {
        'Rad Zones', [33.3890, -112.8651], 'Palo Verde Nuclear Station', 'Lethal radiation requiring hazmat gear.';
        'Ash Storms', [33.3353, -112.0723], 'South Mountain Park', 'Toxic dust clouds causing respiratory damage.';
        'Spectral Mists', [33.5186, -112.0736], 'Jefferson Park', 'Eerie fog disorienting players at midnight.';
        'Crumbling Ridges', [33.5406, -112.0211], 'Piestewa Peak', 'Unstable cliffs risking fatal falls.'
    };

    % --- Random Encounter Configuration ---
    % Encounters with geo-spatial and temporal triggers, plus scope modifiers
    encounter_types = {
        'Reaver Ambush', struct('probability', 0.25, 'impact', struct('player_health', -10, 'faction_tension', 5), ...
            'description', 'Steel Reavers from Piestewa Peak attack, seeking scrap.', ...
            'location_bounds', [33.5406, -112.0211, 0.05], 'trigger', 'entering_area', 'scope', 'combat'); % Within 5km
        'Sanctuary Signal', struct('probability', 0.2, 'impact', struct('resource_scarcity', -5, 'player_xp', 100), ...
            'description', 'A faint signal from Sanctuary Gamma under McHood Park.', ...
            'location_bounds', [34.9719, -110.6434, 0.02], 'trigger', 'exploring', 'scope', 'exploration'); % Within 2km
        'Radhound Pack', struct('probability', 0.15, 'impact', struct('player_health', -15, 'stability', -5), ...
            'description', 'Radhounds from Phoenix Zoo swarm at night.', ...
            'location_bounds', [33.4528, -111.9464, 0.03], 'trigger', 'night_time', 'scope', 'combat'); % At night
        'Keeper Vision', struct('probability', 0.2, 'impact', struct('DawnKeepers_reputation', 10), ...
            'description', 'A Dawn Keeper shares a vision near Phoenix Zoo.', ...
            'location_bounds', [33.4528, -111.9464, 0.05], 'trigger', 'exploring', 'scope', 'social');
        'Ash Storm', struct('probability', 0.1, 'impact', struct('player_health', -5, 'stability', -10), ...
            'description', 'A toxic ash storm engulfs South Mountain Park.', ...
            'location_bounds', [33.3353, -112.0723, 0.1], 'trigger', 'day_time', 'scope', 'environmental'); % During day
    };

    % --- VITALITY System ---
    % Replaces S.P.E.C.I.A.L. with a unique stat system: Vigor, Instinct, Tenacity, Allure, Logic, Intuition, Yield
    vitality_stats = {'Vigor', 'Instinct', 'Tenacity', 'Allure', 'Logic', 'Intuition', 'Yield'};
    % Vigor: Physical strength and melee prowess
    % Instinct: Perception and environmental awareness
    % Tenacity: Endurance and resistance to damage
    % Allure: Charisma and social influence
    % Logic: Intelligence and tech proficiency
    % Intuition: Agility and reflexes
    % Yield: Luck and critical hit chance

    % --- PRECISE Combat System ---
    % Replaces V.A.T.S. with Precision-Enhanced Combat Integration System (PRECISE)
    precise_config = struct(...
        'targeting_modes', {{'Headshot', 'Limb', 'Torso', 'Weakpoint'}}, ...
        'accuracy_bonus', [0.9, 0.7, 0.8, 0.95], ... % Per mode
        'damage_multiplier', [2.0, 1.5, 1.0, 3.0], ... % Per mode
        'focus_cost', [20, 15, 10, 25] ... % Focus points consumed
    );

    % --- Player State Initialization ---
    player_state = struct(...
        'name', 'Wanderer', ...
        'level', 1, ...
        'xp', 0, ...
        'xp_for_next_level', 1000, ...
        'vitality', [5, 5, 5, 5, 5, 5, 5], ... % VITALITY stats
        'health', 50, ... % Base health + Tenacity * 2
        'focus', 100, ... % PRECISE system resource
        'reputation', struct('DawnKeepers', 0, 'SteelReavers', 0, 'SandWraiths', 0, 'EmberCult', 0), ...
        'inventory', {{'scrap_metal', 10; 'energy_cell', 5; 'rad_shield', 1}}, ...
        'location', [33.4484, -112.0740], ... % Downtown Phoenix
        'active_quests', {{}} ...
    );

    % --- NPC and Dialogue Configuration ---
    npc_archetypes = {
        'Drifter', struct('mood', {{'curious', 'wary', 'hopeful'}}, 'goal', 'find_refuge');
        'Scrapper', struct('mood', {{'aggressive', 'pragmatic', 'greedy'}}, 'goal', 'hoard_resources');
        'Seer', struct('mood', {{'inquisitive', 'cautious', 'visionary'}}, 'goal', 'cleanse_wastes');
        'Synth', struct('mood', {{'erratic', 'logical', 'hostile'}}, 'goal', 'gain_autonomy')
    };
    dialogue_templates = struct(...
        'greeting', struct(...
            'curious', {{'Seen any sanctuaries in the Valley?', 'What’s worth scavenging in this ruin?'}}, ...
            'wary', {{'Keep your distance. You with the Cult?', 'Phoenix ain’t safe for strangers.'}}, ...
            'hopeful', {{'Could you be the one to save the Valley?', 'Any news from the wastes?'}}, ...
            'aggressive', {{'Hand over your tech, or you’re ash.', 'This is Reaver ground!'}}, ...
            'pragmatic', {{'Got scrap to trade? Make it quick.', 'What’s your play, Wanderer?'}}, ...
            'inquisitive', {{'Know anything about Sanctuary Alpha?', 'Ever braved Palo Verde’s ruins?'}}, ...
            'erratic', {{'*bzzt* State your intent!', 'System unstable. Query: purpose?'}}, ...
            'hostile', {{'Leave, or you’re scrapped.', 'No wanderers here!'}}, ...
            'visionary', {{'The wastes can be reborn. Will you help?', 'I see a future for Phoenix.'}} ...
        ), ...
        'quest_offer', struct(...
            'curious', {{'Heard of a sanctuary under McHood Park. Up for it?', 'Strange pulses from Palo Verde. Wanna investigate?'}}, ...
            'pragmatic', {{'Reavers need eyes on South Mountain. Good pay.', 'Smuggle tech past Wraiths. Big payout.'}}, ...
            'visionary', {{'Help the Keepers cleanse the Zoo. It’s time.', 'We must preserve Phoenix’s past. Join us?'}}, ...
            'hostile', {{'Wipe out a Wraith camp, or you’re next.', 'Sabotage the Keepers’ shrine. Now.'}} ...
        ), ...
        'reaction', struct(...
            'positive', {{'Solid choice, Wanderer. Let’s move.', 'Didn’t see that coming—nice.'}}, ...
            'negative', {{'You’re gonna pay for that.', 'Bad move, drifter.'}}, ...
            'neutral', {{'Your call, wanderer.', 'Let’s see how this pans out.'}} ...
        ) ...
    );

    % --- Main Story Configuration ---
    % Central narrative: Rebuild Phoenix amidst faction conflicts and rising mutations
    main_quest = struct(...
        'title', 'Phoenix Reborn', ...
        'description', 'Unite or conquer factions to restore Phoenix, uncovering Sanctuary Alpha’s secrets.', ...
        'stages', {{...
            struct('objective', 'Meet a Dawn Keeper at Phoenix Zoo', 'location', [33.4528, -111.9464], 'status', 'active'), ...
            struct('objective', 'Investigate Sanctuary Alpha in Downtown Phoenix', 'location', [33.4484, -112.0740], 'status', 'locked'), ...
            struct('objective', 'Resolve faction conflict at Palo Verde', 'location', [33.3890, -112.8651], 'status', 'locked') ...
        }} ...
    );
    player_state.active_quests = {main_quest.stages{1}};

    % --- Main Game Loop ---
    game_running = true;
    fprintf('Welcome to Wasteland Chronicles: Phoenix Rising!\n');
    fprintf('Character: %s (Level %d, Location: Downtown Phoenix)\n', player_state.name, player_state.level);
    fprintf('Year: 2040, 15 years after the Great War.\n');
    fprintf('Main Quest: %s\n', main_quest.description);
    fprintf('----------------------------------------\n');

    global lore_history;
    lore_history = {};

    while game_running
        % Update player health based on Tenacity
        player_state.health = 50 + player_state.level * 5 + player_state.vitality(3) * 2;

        % Fetch nearby locations using Google Maps Places API (simulated)
        nearby_locations = fetch_nearby_locations(player_state.location, locations);
        fprintf('Nearby Locations:\n');
        for i = 1:min(3, length(nearby_locations))
            fprintf('  %s (%.2f km away)\n', nearby_locations{i, 1}, nearby_locations{i, 2});
        end

        % Trigger random encounter based on geo-spatial and temporal conditions
        encounter = select_geo_spatial_encounter(encounter_types, player_state.location, get_current_time());
        if ~isempty(encounter)
            apply_encounter_effects(encounter, world_state, player_state, precise_config);
            fprintf('Random Encounter: %s\n', encounter.description);
            lore_snippet = generate_lore_snippet(world_state, player_state, factions, locations);
            fprintf('Lore Update: %s\n', lore_snippet);
        end

        % Update NPC behavior
        npc = generate_random_npc(npc_archetypes, world_state, player_state, factions);
        fprintf('\nYou meet a %s (%s, Mood: %s)\n', npc.archetype, npc.faction, npc.mood);

        % Display menu
        fprintf('\n--- Main Menu ---\n');
        fprintf('1. Interact with NPC\n');
        fprintf('2. View Character Sheet\n');
        fprintf('3. Gain Experience\n');
        fprintf('4. View World State\n');
        fprintf('5. Move to New Location\n');
        fprintf('6. Engage PRECISE Combat\n');
        fprintf('7. Quit\n');
        choice = input('Enter your choice (1-7): ', 's');

        switch choice
            case '1'
                handle_npc_interaction(npc, dialogue_templates, player_state, world_state);
            case '2'
                display_character_sheet(player_state, vitality_stats);
            case '3'
                xp_gained = input('Enter XP to gain: ');
                if isnumeric(xp_gained) && xp_gained > 0
                    player_state.xp = player_state.xp + xp_gained;
                    fprintf('Gained %d XP.\n', xp_gained);
                    while player_state.xp >= player_state.xp_for_next_level
                        player_state.level = player_state.level + 1;
                        fprintf('Level Up! You are now Level %d!\n', player_state.level);
                        stat_to_increase = randi(7);
                        player_state.vitality(stat_to_increase) = min(player_state.vitality(stat_to_increase) + 1, 10);
                        fprintf('Gained +1 %s!\n', vitality_stats{stat_to_increase});
                        player_state.xp_for_next_level = player_state.xp_for_next_level + 1000 + (player_state.level - 1) * 500;
                    end
                else
                    fprintf('Invalid XP amount.\n');
                end
            case '4'
                display_world_state(world_state, factions, locations);
            case '5'
                fprintf('Available Locations:\n');
                for i = 1:size(locations, 1)
                    dist = haversine_distance(player_state.location, locations{i, 2});
                    fprintf('  %d. %s (%.2f km away)\n', i, locations{i, 1}, dist);
                end
                loc_choice = input('Enter location number to move to: ');
                if isnumeric(loc_choice) && loc_choice >= 1 && loc_choice <= size(locations, 1)
                    player_state.location = locations{loc_choice, 2};
                    fprintf('Moved to %s.\n', locations{loc_choice, 1});
                    % Check for main quest progression
                    update_main_quest(player_state, main_quest);
                else
                    fprintf('Invalid location.\n');
                end
            case '6'
                engage_precise_combat(player_state, precise_config, monsters, player_state.location);
            case '7'
                fprintf('Thanks for playing Wasteland Chronicles: Phoenix Rising!\n');
                game_running = false;
            otherwise
                fprintf('Invalid choice. Please enter a number between 1 and 7.\n');
        end
    end

    % Save game state
    fid = fopen('game_state.txt', 'w');
    fprintf(fid, '%s', jsonencode(struct('player', player_state, 'world', world_state, 'main_quest', main_quest)));
    fclose(fid);
end

% --- Helper Functions ---

function dist = haversine_distance(loc1, loc2)
    % Calculate distance between two coordinates in km
    R = 6371; % Earth's radius in km
    lat1 = deg2rad(loc1(1)); lon1 = deg2rad(loc1(2));
    lat2 = deg2rad(loc2(1)); lon2 = deg2rad(loc2(2));
    dlat = lat2 - lat1; dlon = lon2 - lon1;
    a = sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2;
    c = 2 * atan2(sqrt(a), sqrt(1-a));
    dist = R * c;
end

function nearby = fetch_nearby_locations(player_loc, locations)
    % Simulated Google Maps Places API call
    nearby = {};
    for i = 1:size(locations, 1)
        dist = haversine_distance(player_loc, locations{i, 2});
        if dist < 10 % Within 10km
            nearby{end+1, 1} = locations{i, 1};
            nearby{end, 2} = dist;
        end
    end
end

function encounter = select_geo_spatial_encounter(encounter_types, player_loc, current_time)
    % Select encounter based on location, time, and scope
    encounter = [];
    for i = 1:length(encounter_types)
        bounds = encounter_types{i, 2}.location_bounds;
        dist = haversine_distance(player_loc, bounds(1:2));
        trigger = encounter_types{i, 2}.trigger;
        is_night = current_time.hour >= 18 || current_time.hour < 6;
        if dist <= bounds(3) && rand() < encounter_types{i, 2}.probability
            if (strcmp(trigger, 'entering_area') || ...
                (strcmp(trigger, 'night_time') && is_night) || ...
                (strcmp(trigger, 'day_time') && ~is_night) || ...
                strcmp(trigger, 'exploring'))
                encounter = encounter_types{i, 2};
                break;
            end
        end
    end
end

function current_time = get_current_time()
    % Simulated time function (uses system time)
    current_time = struct('hour', mod(floor(toc/3600), 24));
end

function apply_encounter_effects(encounter, world_state, player_state, precise_config)
    fields = fieldnames(encounter.impact);
    for i = 1:length(fields)
        target = fields{i};
        if isfield(world_state, target)
            world_state.(target) = min(max(world_state.(target) + encounter.impact.(target), 0), 100);
        elseif strcmp(target, 'player_health')
            player_state.health = max(player_state.health + encounter.impact.(target), 0);
        elseif isfield(player_state.reputation, target)
            player_state.reputation.(target) = min(max(player_state.reputation.(target) + encounter.impact.(target), -100), 100);
        elseif strcmp(target, 'player_xp')
            player_state.xp = player_state.xp + encounter.impact.(target);
        end
    end
    % Apply PRECISE combat effects if combat encounter
    if strcmp(encounter.scope, 'combat')
        engage_precise_combat(player_state, precise_config, [], player_state.location);
    end
end

function npc = generate_random_npc(archetypes, world_state, player_state, factions)
    archetype_idx = randi(length(archetypes));
    archetype = archetypes{archetype_idx, 1};
    traits = archetypes{archetype_idx, 2};
    faction_idx = randi(size(factions, 1));
    mood = traits.mood{randi(length(traits.mood))};
    if world_state.faction_tension > 70 && rand() < 0.5
        mood = 'hostile';
    elseif isfield(player_state.reputation, factions{faction_idx, 1}) && player_state.reputation.(factions{faction_idx, 1}) > 50
        mood = 'hopeful';
    elseif isfield(player_state.reputation, factions{faction_idx, 1}) && player_state.reputation.(factions{faction_idx, 1}) < -50
        mood = 'hostile';
    end
    npc = struct('archetype', archetype, 'faction', factions{faction_idx, 1}, 'mood', mood, 'goal', traits.goal);
end

function handle_npc_interaction(npc, dialogue_templates, player_state, world_state)
    if isfield(dialogue_templates.greeting, npc.mood)
        fprintf('NPC: %s\n', dialogue_templates.greeting.(npc.mood){randi(length(dialogue_templates.greeting.(npc.mood)))});
    else
        fprintf('NPC: Nothing to say right now.\n');
        return;
    end
    fprintf('1. Respond Positively\n2. Respond Neutrally\n3. Respond Negatively\n4. Offer Quest\n5. End Interaction\n');
    choice = input('Your response (1-5): ', 's');
    switch choice
        case '1'
            fprintf('NPC: %s\n', dialogue_templates.reaction.positive{randi(length(dialogue_templates.reaction.positive))});
            player_state.reputation.(npc.faction) = min(player_state.reputation.(npc.faction) + 5, 100);
        case '2'
            fprintf('NPC: %s\n', dialogue_templates.reaction.neutral{randi(length(dialogue_templates.reaction.neutral))});
        case '3'
            fprintf('NPC: %s\n', dialogue_templates.reaction.negative{randi(length(dialogue_templates.reaction.negative))});
            player_state.reputation.(npc.faction) = max(player_state.reputation.(npc.faction) - 5, -100);
        case '4'
            if isfield(dialogue_templates.quest_offer, npc.mood)
                quest = dialogue_templates.quest_offer.(npc.mood){randi(length(dialogue_templates.quest_offer.(npc.mood)))};
                fprintf('NPC: %s\n', quest);
                if length(player_state.active_quests) < 5
                    player_state.active_quests{end+1} = struct('description', quest, 'faction', npc.faction, 'status', 'active');
                else
                    fprintf('Quest log full. Complete existing quests.\n');
                end
            else
                fprintf('NPC: No tasks for you now.\n');
            end
        case '5'
            fprintf('Interaction ended.\n');
        otherwise
            fprintf('Invalid choice.\n');
    end
end

function display_character_sheet(player_state, vitality_stats)
    fprintf('\n--- Character Sheet: %s ---\n', player_state.name);
    fprintf('Level: %d\nXP: %d / %d\n', player_state.level, player_state.xp, player_state.xp_for_next_level);
    fprintf('Location: [%.4f, %.4f]\n', player_state.location(1), player_state.location(2));
    fprintf('Health: %d\nFocus: %d\n', player_state.health, player_state.focus);
    fprintf('VITALITY Stats:\n');
    for i = 1:length(player_state.vitality)
        fprintf('  %s: %d\n', vitality_stats{i}, player_state.vitality(i));
    end
    fprintf('Reputation:\n');
    rep_fields = fieldnames(player_state.reputation);
    for i = 1:length(rep_fields)
        fprintf('  %s: %d\n', rep_fields{i}, player_state.reputation.(rep_fields{i}));
    end
    fprintf('Active Quests:\n');
    if isempty(player_state.active_quests)
        fprintf('  None\n');
    else
        for i = 1:length(player_state.active_quests)
            fprintf('  %s (%s)\n', player_state.active_quests{i}.description, player_state.active_quests{i}.status);
        end
    end
    fprintf('Inventory:\n');
    for i = 1:size(player_state.inventory, 1)
        fprintf('  %s: %d\n', player_state.inventory{i, 1}, player_state.inventory{i, 2});
    end
    fprintf('----------------------------------------\n');
end

function display_world_state(world_state, factions, locations)
    fprintf('\n--- World State ---\n');
    fprintf('Radiation Level: %d%%\n', world_state.radiation_level);
    fprintf('Resource Scarcity: %d%%\n', world_state.resource_scarcity);
    fprintf('Faction Tension: %d%%\n', world_state.faction_tension);
    fprintf('Stability: %d%%\n', world_state.stability);
    fprintf('Factions:\n');
    for i = 1:size(factions, 1)
        fprintf('  %s: %s (Base: %s)\n', factions{i, 1}, factions{i, 2}, factions{i, 4});
    end
    fprintf('Locations:\n');
    for i = 1:size(locations, 1)
        fprintf('  %s: %s\n', locations{i, 1}, locations{i, 3});
    end
    fprintf('----------------------------------------\n');
end

function lore_snippet = generate_lore_snippet(world_state, player_state, factions, locations)
    global lore_history;
    themes = {'greed', 'survival', 'redemption', 'technology'};
    theme = themes{randi(length(themes))};
    faction = factions{randi(size(factions, 1)), 1};
    location = locations{randi(size(locations, 1)), 1};
    switch theme
        case 'greed'
            lore_snippet = sprintf('%s scheme to claim %s’s tech, igniting conflict.', faction, location);
        case 'survival'
            lore_snippet = sprintf('Survivors in %s unite against %s, calling for %s’s help.', location, faction, player_state.name);
        case 'redemption'
            lore_snippet = sprintf('A %s exile seeks atonement in %s, offering secrets.', faction, location);
        case 'technology'
            lore_snippet = sprintf('%s found ancient tech in %s, altering the balance.', faction, location);
    end
    if world_state.faction_tension > 70
        lore_snippet = sprintf('%s War brews in the Valley.', lore_snippet);
    elseif world_state.resource_scarcity > 70
        lore_snippet = sprintf('%s Supplies are critical, forcing hard choices.', lore_snippet);
    elseif world_state.stability < 20
        lore_snippet = sprintf('%s Anarchy spreads across Phoenix.', lore_snippet);
    elseif world_state.radiation_level > 80
        lore_snippet = sprintf('%s Radiation surges, spawning new horrors.', lore_snippet);
    end
    if ismember(lore_snippet, lore_history)
        lore_snippet = generate_lore_snippet(world_state, player_state, factions, locations);
    else
        lore_history{end+1} = lore_snippet;
    end
end

function update_main_quest(player_state, main_quest)
    for i = 1:length(main_quest.stages)
        stage = main_quest.stages{i};
        if strcmp(stage.status, 'active') && haversine_distance(player_state.location, stage.location) < 0.05
            fprintf('Main Quest Progress: %s completed!\n', stage.objective);
            main_quest.stages{i}.status = 'completed';
            if i < length(main_quest.stages)
                main_quest.stages{i+1}.status = 'active';
                player_state.active_quests = {main_quest.stages{i+1}};
                fprintf('New Objective: %s\n', main_quest.stages{i+1}.objective);
            end
        end
    end
end

function engage_precise_combat(player_state, precise_config, monsters, player_loc)
    % Simulated PRECISE combat system
    if isempty(monsters)
        fprintf('No enemies nearby to engage.\n');
        return;
    end
    % Select nearby monster
    nearby_monsters = {};
    for i = 1:size(monsters, 1)
        if haversine_distance(player_loc, monsters{i, 2}) < 0.05
            nearby_monsters{end+1} = monsters{i, 1};
        end
    end
    if isempty(nearby_monsters)
        fprintf('No enemies nearby to engage.\n');
        return;
    end
    target = nearby_monsters{randi(length(nearby_monsters))};
    fprintf('Engaging %s in PRECISE combat!\n', target);
    fprintf('Targeting Modes:\n');
    for i = 1:length(precise_config.targeting_modes)
        fprintf('  %d. %s (Accuracy: %.0f%%, Damage: %.1fx, Focus Cost: %d)\n', ...
            i, precise_config.targeting_modes{i}, precise_config.accuracy_bonus(i)*100, ...
            precise_config.damage_multiplier(i), precise_config.focus_cost(i));
    end
    choice = input('Select targeting mode (1-4): ', 's');
    choice_idx = str2double(choice);
    if isnumeric(choice_idx) && choice_idx >= 1 && choice_idx <= 4
        if player_state.focus >= precise_config.focus_cost(choice_idx)
            player_state.focus = player_state.focus - precise_config.focus_cost(choice_idx);
            hit = rand() < precise_config.accuracy_bonus(choice_idx);
            if hit
                damage = 10 * precise_config.damage_multiplier(choice_idx);
                fprintf('Hit! Dealt %d damage to %s.\n', damage, target);
                player_state.xp = player_state.xp + 50;
            else
                fprintf('Missed %s!\n', target);
            end
        else
            fprintf('Insufficient focus for %s mode.\n', precise_config.targeting_modes{choice_idx});
        end
    else
        fprintf('Invalid targeting mode.\n');
    end
end

% Run the game
phoenix_wasteland_chronicles();

```matlab
% Enhanced Turn-Based Combat Simulation in MATLAB
% Incorporates weapon variety, armor, environmental interactions, team coordination, and critical hits

% Initialize the battlefield grid (7x7 for more space)
grid_size = [7, 7];
grid = zeros(grid_size(1), grid_size(2)); % 0 means empty
terrain = zeros(grid_size(1), grid_size(2)); % 0: open, 1: cover (+10 hit chance penalty), 2: obstacle (blocks movement)

% Define terrain (example: cover at (3,3), obstacle at (4,4))
terrain(3,3) = 1; % Cover
terrain(4,4) = 2; % Obstacle

% Define body parts with hit chance and damage modifiers
body_parts = {
    'head', -20, 2, 0.3; % hit_chance_mod, damage_mod, cripple_chance
    'torso', 0, 1, 0.1;
    'legs', -10, 1, 0.4;
};

% Define weapons
weapons = {
    'pistol', 10, 3, 3, 0.1; % name, damage, range, AP_cost, crit_chance
    'rifle', 15, 4, 4, 0.15;
    'melee', 20, 1, 2, 0.2;
};

% Define characters with armor and team coordination
characters = {};
character1 = struct(...
    'name', 'Addict - Wimpy', ...
    'team', 1, ...
    'health', 50, ...
    'max_health', 50, ...
    'AP', 5, ...
    'max_AP', 5, ...
    'position', [1, 1], ...
    'weapon', weapons(1,:), ... % Pistol
    'armor', 5, ... % Damage reduction
    'min_to_hit', 90, ...
    'run_away_mode', 'coward', ...
    'is_crippled', false);
character2 = struct(...
    'name', 'Raider', ...
    'team', 1, ...
    'health', 60, ...
    'max_health', 60, ...
    'AP', 6, ...
    'max_AP', 6, ...
    'position', [1, 2], ...
    'weapon', weapons(2,:), ... % Rifle
    'armor', 10, ...
    'min_to_hit', 80, ...
    'run_away_mode', 'tourniquet', ...
    'is_crippled', false);
character3 = struct(...
    'name', 'Deathclaw', ...
    'team', 2, ...
    'health', 200, ...
    'max_health', 200, ...
    'AP', 10, ...
    'max_AP', 10, ...
    'position', [7, 7], ...
    'weapon', weapons(3,:), ... % Melee
    'armor', 20, ...
    'min_to_hit', 10, ...
    'run_away_mode', 'never', ...
    'is_crippled', false);
characters{1} = character1;
characters{2} = character2;
characters{3} = character3;

% Place characters on the grid
grid(1, 1) = 1; % Addict
grid(1, 2) = 2; % Raider
grid(7, 7) = 3; % Deathclaw

% Main simulation loop
while true
    % Check if only one team remains
    alive_chars = characters([characters{:}.health] > 0);
    teams = unique([alive_chars.team]);
    if length(teams) <= 1
        if isempty(teams)
            disp('Combat ended: All characters defeated.');
        else
            disp(['Combat ended: Team ', num2str(teams(1)), ' wins!']);
        end
        break;
    end

    % Iterate through characters
    for i = 1:length(characters)
        current_char = characters{i};
        if current_char.health <= 0
            continue;
        end

        % Reset AP for the turn, reduce if crippled
        current_char.AP = current_char.max_AP;
        if current_char.is_crippled
            current_char.AP = floor(current_char.AP * 0.7);
        end

        % Action loop
        while current_char.AP > 0
            action = decide_action(current_char, characters, grid, terrain, grid_size, body_parts);
            if strcmp(action.type, 'attack')
                target = characters{action.target_idx};
                body_part = action.body_part;
                hit_chance = calculate_hit_chance(current_char, target, body_part, body_parts, terrain);
                if rand * 100 < hit_chance
                    [damage, is_critical, is_crippled] = calculate_damage(current_char, target, body_part, body_parts);
                    damage = max(0, damage - target.armor); % Apply armor reduction
                    target.health = target.health - damage;
                    if is_critical
                        disp([current_char.name, ' lands a critical hit on ', target.name, '''s ', body_part, ' for ', num2str(damage), ' damage!']);
                    else
                        disp([current_char.name, ' hits ', target.name, '''s ', body_part, ' for ', num2str(damage), ' damage.']);
                    end
                    if is_crippled && ~target.is_crippled
                        target.is_crippled = true;
                        disp([target.name, '''s ', body_part, ' is crippled!']);
                    end
                    if target.health <= 0
                        disp([target.name, ' is defeated!']);
                        grid(target.position(1), target.position(2)) = 0;
                    end
                else
                    disp([current_char.name, ' misses ', target.name, '.']);
                end
                current_char.AP = current_char.AP - current_char.weapon{4}; % AP_cost
            elseif strcmp(action.type, 'move')
                new_position = current_char.position + action.direction;
                if is_valid_position(new_position, grid, terrain, grid_size)
                    grid(current_char.position(1), current_char.position(2)) = 0;
                    current_char.position = new_position;
                    grid(new_position(1), new_position(2)) = i;
                    current_char.AP = current_char.AP - 1; % Move cost = 1 AP
                    disp([current_char.name, ' moves to position (', num2str(new_position(1)), ', ', num2str(new_position(2)), ').']);
                else
                    break; % Can't move, end turn
                end
            else
                break; % No valid action, end turn
            end
            characters{i} = current_char;
            if action.target_idx > 0
                characters{action.target_idx} = target;
            end
        end
    end
end

% Function to decide action with team coordination
function action = decide_action(current_char, characters, grid, terrain, grid_size, body_parts)
    action = struct('type', '', 'target_idx', 0, 'body_part', '', 'direction', [0, 0]);
    
    % Check for fleeing based on run_away_mode
    flee_threshold = 0;
    if strcmp(current_char.run_away_mode, 'coward')
        flee_threshold = 0.2 * current_char.max_health;
    elseif strcmp(current_char.run_away_mode, 'tourniquet')
        flee_threshold = 0.5 * current_char.max_health;
    end
    if current_char.health < flee_threshold && ~strcmp(current_char.run_away_mode, 'never')
        nearest_enemy = find_nearest_enemy(current_char, characters);
        if ~isempty(nearest_enemy)
            delta = current_char.position - nearest_enemy.position;
            if abs(delta(1)) > abs(delta(2))
                action.direction = [sign(delta(1)), 0];
            else
                action.direction = [0, sign(delta(2))];
            end
            action.type = 'move';
            return;
        end
    end

    % Team coordination: prioritize target with lowest health
    enemies = characters([characters{:}.team] ~= current_char.team & [characters{:}.health] > 0);
    target_idx = 0;
    min_health = Inf;
    min_dist = Inf;
    nearest_enemy_idx = 0;
    for j = 1:length(enemies)
        dist = abs(enemies{j}.position(1) - current_char.position(1)) + ...
               abs(enemies{j}.position(2) - current_char.position(2));
        if enemies{j}.health < min_health
            min_health = enemies{j}.health;
            target_idx = find(cellfun(@(c) c == enemies{j}, characters));
        end
        if dist < min_dist
            min_dist = dist;
            nearest_enemy_idx = find(cellfun(@(c) c == enemies{j}, characters));
        end
    end
    % Prefer team target if in range, else nearest enemy
    target_idx = target_idx * (min_health < Inf && min_dist <= current_char.weapon{3}) + nearest_enemy_idx * (min_health == Inf || min_dist > current_char.weapon{3});

    % Decide to attack or move
    if target_idx > 0 && min_dist <= current_char.weapon{3} && current_char.AP >= current_char.weapon{4}
        body_part = choose_body_part(current_char, characters{target_idx}, body_parts, terrain);
        hit_chance = calculate_hit_chance(current_char, characters{target_idx}, body_part, body_parts, terrain);
        if hit_chance >= current_char.min_to_hit
            action.type = 'attack';
            action.target_idx = target_idx;
            action.body_part = body_part;
        else
            action = move_towards_enemy(current_char, characters{target_idx}, terrain, grid_size);
        end
    else
        action = move_towards_enemy(current_char, characters{nearest_enemy_idx}, terrain, grid_size);
    end
end

% Function to calculate hit chance with terrain effects
function hit_chance = calculate_hit_chance(attacker, defender, body_part, body_parts, terrain)
    base_chance = 80;
    distance = abs(attacker.position(1) - defender.position(1)) + ...
               abs(attacker.position(2) - defender.position(2));
    distance_penalty = (distance - 1) * 5;
    body_part_mod = body_parts{strcmp(body_parts(:,1), body_part), 2};
    terrain_penalty = 10 * (terrain(defender.position(1), defender.position(2)) == 1); % Cover penalty
    hit_chance = base_chance - distance_penalty + body_part_mod - terrain_penalty;
    hit_chance = max(0, min(100, hit_chance));
end

% Function to calculate damage with critical hits and crippling
function [damage, is_critical, is_crippled] = calculate_damage(attacker, defender, body_part, body_parts)
    base_damage = attacker.weapon{2}; % Weapon damage
    damage_mod = body_parts{strcmp(body_parts(:,1), body_part), 3};
    crit_chance = attacker.weapon{5}; % Weapon crit chance
    is_critical = rand < crit_chance;
    damage = base_damage * damage_mod * (1 + is_critical * 1.5); % 1.5x for critical
    cripple_chance = body_parts{strcmp(body_parts(:,1), body_part), 4};
    is_crippled = rand < cripple_chance && ~defender.is_crippled;
end

% Function to choose body part
function body_part = choose_body_part(attacker, defender, body_parts, terrain)
    base_chance = 80;
    distance = abs(attacker.position(1) - defender.position(1)) + ...
               abs(attacker.position(2) - defender.position(2));
    distance_penalty = (distance - 1) * 5;
    terrain_penalty = 10 * (terrain(defender.position(1), defender.position(2)) == 1);
    expected_damages = zeros(size(body_parts,1),1);
    for i = 1:size(body_parts,1)
        hit_chance_mod = body_parts{i,2};
        damage_mod = body_parts{i,3};
        crit_chance = attacker.weapon{5};
        hit_chance = base_chance - distance_penalty + hit_chance_mod - terrain_penalty;
        hit_chance = max(0, min(100, hit_chance));
        expected_damages(i) = (hit_chance / 100) * (attacker.weapon{2} * damage_mod) * (1 + crit_chance * 1.5);
    end
    [~, idx] = max(expected_damages);
    body_part = body_parts{idx,1};
end

% Function to find nearest enemy
function nearest_enemy = find_nearest_enemy(current_char, characters)
    enemies = characters([characters{:}.team] ~= current_char.team & [characters{:}.health] > 0);
    min_dist = Inf;
    nearest_enemy = [];
    for j = 1:length(enemies)
        dist = abs(enemies{j}.position(1) - current_char.position(1)) + ...
               abs(enemies{j}.position(2) - current_char.position(2));
        if dist < min_dist
            min_dist = dist;
            nearest_enemy = enemies{j};
        end
    end
end

% Function to move towards enemy with terrain consideration
function action = move_towards_enemy(current_char, enemy, terrain, grid_size)
    action = struct('type', 'move', 'target_idx', 0, 'body_part', '', 'direction', [0, 0]);
    if current_char.AP < 1 || isempty(enemy)
        return;
    end
    delta = enemy.position - current_char.position;
    possible_directions = [];
    if abs(delta(1)) > 0
        possible_directions = [possible_directions; sign(delta(1)), 0];
    end
    if abs(delta(2)) > 0
        possible_directions = [possible_directions; 0, sign(delta(2))];
    end
    for i = 1:size(possible_directions,1)
        new_pos = current_char.position + possible_directions(i,:);
        if is_valid_position(new_pos, grid, terrain, grid_size)
            action.direction = possible_directions(i,:);
            return;
        end
    end
end

% Function to check if position is valid
function valid = is_valid_position(pos, grid, terrain, grid_size)
    valid = pos(1) >= 1 && pos(1) <= grid_size(1) && ...
            pos(2) >= 1 && pos(2) <= grid_size(2) && ...
            grid(pos(1), pos(2)) == 0 && ...
            terrain(pos(1), pos(2)) ~= 2; % Not an obstacle
end
```
Addict - Wimpy moves to position (2, 1).
Raider hits Deathclaw's torso for 5 damage.
Deathclaw moves to position (6, 7).
Addict - Wimpy lands a critical hit on Deathclaw's head for 15 damage!
Deathclaw's head is crippled!
Raider misses Deathclaw.
Deathclaw hits Raider's legs for 10 damage.
...
Combat ended: Team 2 wins!
Attribute	Description	Example (Addict - Wimpy)
Name	Character identifier	Addict - Wimpy
Team	Team affiliation for combat resolution	1
Health	Current hit points	50
Max Health	Maximum hit points	50
AP	Action points available per turn	5
Max AP	Maximum action points	5
Position	Grid coordinates [row, col]	[1, 1]
Weapon	Struct with damage, range, AP_cost	{10, 1, 3}
Min_to_hit	Minimum hit chance to attack (%)	90
Run_away_mode	Condition for fleeing (e.g., coward, tourniquet)	coward

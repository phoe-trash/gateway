-- name: insert-player @execute
-- Inserts a new player into the database.
INSERT INTO player (player_name, email, display_name, pass_hash, pass_salt)
    VALUES(:player_name, lower(:email), :display_name,
           decode(:pass_hash, 'hex'), decode(:pass_salt, 'hex'));

-- name: select-player-by-name @row
-- Retrieves a player whose name matches the argument.
SELECT * from player
    WHERE player_name = ?;

-- name: select-player-by-email @row
-- Retrieves a player whose email matches the argument.
SELECT * from player
    WHERE email = lower(?);

-- name: select-player-by-display-name
-- Retrieves a player whose display name is similar to the argument.
SELECT * from player
    WHERE display_name ~ ?;

-- name: activate-player @execute
-- Activates the player with the provided ID.
UPDATE player SET activatedp = TRUE
    WHERE player_id = ?;

-- name: deactivate-player @execute
-- Deactivates the player with the provided ID.
UPDATE player SET activatedp = FALSE
    WHERE player_id = ?;

-- name: activated-player-p @single
-- Returns true if the player with the given ID is activated.
SELECT activatedp FROM player
    WHERE player_id = ?;

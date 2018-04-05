-- name: insert-player @single
-- Inserts a new player into the database.
INSERT INTO player (login, email, display_name, pass_hash, pass_salt)
    VALUES(:login, lower(:email), :display_name,
           decode(:hash, 'hex'), decode(:salt, 'hex'))
    RETURNING player_id;

-- name: select-player-by-login @row
-- Retrieves a player whose login matches the argument.
SELECT * from player
    WHERE login = ?;

-- name: select-player-by-email @row
-- Retrieves a player whose email matches the argument.
SELECT * from player
    WHERE email = lower(?);

-- name: select-player-by-display-name
-- Retrieves a player whose display name is similar to the argument.
SELECT * from player
    WHERE display_name ~ ?;

-- name: activate-player-by-id @execute
-- Activates the player with the provided ID.
UPDATE player SET activatedp = TRUE
    WHERE player_id = ?;

-- name: deactivate-player-by-id @execute
-- Deactivates the player with the provided ID.
UPDATE player SET activatedp = FALSE
    WHERE player_id = ?;

-- name: activated-player-p-by-id @single
-- Returns true if the player with the given ID is activated.
SELECT activatedp FROM player
    WHERE player_id = ?;

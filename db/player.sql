-- name: insert-player @execute
-- Inserts a new player into the database.
INSERT INTO player (player_name, email, display_name, passhash)
    VALUES(:player_name, lower(:email),
           :display_name, decode(:passhash, 'hex'));

-- name: player-row-by-name @row
-- Retrieves a player whose data matches the arguments.
SELECT * from player
    WHERE player_name = ?

-- name: player-row-by-email @row
-- Retrieves a player whose data matches the arguments.
SELECT * from player
    WHERE email = lower(?)

-- name: player-row-by-display-name @row
-- Retrieves a player whose data matches the arguments.
SELECT * from player
    WHERE display_name = ?

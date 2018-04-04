-- name: insert-player @execute
-- Inserts a new player into the database.
INSERT INTO player (player_name, email, display_name, passhash)
    VALUES(:player_name, lower(:email), :display_name, :passhash);

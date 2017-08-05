-- name: install-create-table-player
CREATE TABLE player (
        player_id      serial        NOT NULL PRIMARY KEY,
        player         text          NOT NULL,
        email          text          NOT NULL,
        passhash       bytea         NOT NULL);

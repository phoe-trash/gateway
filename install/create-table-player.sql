-- name: install-create-table-player
CREATE TABLE player (
        player_id      serial        NOT NULL PRIMARY KEY,
        player         varchar(80)   NOT NULL,
        email          varchar(80)   NOT NULL,
        passhash       bytea         NOT NULL);

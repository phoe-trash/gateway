-- name: install-create-table-persona
CREATE TABLE persona (
        persona_id     serial        NOT NULL PRIMARY KEY,
        persona        varchar(80)   NOT NULL,
        owner          integer       NOT NULL REFERENCES player(player_id)
        ON UPDATE CASCADE,
        borrower       integer       NOT NULL REFERENCES player(player_id)
        ON UPDATE CASCADE,
        description    text          NOT NULL);

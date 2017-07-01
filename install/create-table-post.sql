-- name: install-create-table-post
CREATE TABLE post (
        post_id        serial        NOT NULL PRIMARY KEY,
        player_id      integer       NOT NULL REFERENCES player(player_id)
        ON UPDATE CASCADE,
        persona_id     integer       NOT NULL REFERENCES persona(persona_id)
        ON UPDATE CASCADE,
        post_time      timestamp     NOT NULL,
        contents       text          NOT NULL,
        chapter_id     integer       NOT NULL REFERENCES chapter(chapter_id)
        ON UPDATE CASCADE,
        chapter_order  integer       NOT NULL,
        CONSTRAINT     id_order      UNIQUE(chapter_id, chapter_order));

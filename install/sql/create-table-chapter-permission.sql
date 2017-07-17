-- name: install-create-table-chapter-permission
CREATE TABLE chapter_permission (
        player_id      integer       NOT NULL REFERENCES player(player_id)
        ON UPDATE CASCADE,
        chapter_id     integer       NOT NULL REFERENCES chapter(chapter_id)
        ON UPDATE CASCADE,
        permission     ch_permission NOT NULL,
        CONSTRAINT     chapter_permission_primary_key
        PRIMARY KEY (player_id, chapter_id, permission));

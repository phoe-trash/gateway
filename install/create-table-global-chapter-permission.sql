-- name: install-create-table-global-chapter-permission
CREATE TABLE global_chapter_permission (
        timeline_id    integer       NOT NULL REFERENCES timeline(timeline_id)
        ON UPDATE CASCADE,
        permission     ch_permission NOT NULL,
        CONSTRAINT global_chapter_permission_primary_key
        PRIMARY KEY (timeline_id, permission));

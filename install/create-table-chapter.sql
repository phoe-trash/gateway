-- name: install-create-table-chapter
CREATE TABLE chapter (
        chapter_id     serial        NOT NULL PRIMARY KEY,
        chapter        varchar(80)   NOT NULL,
        timeline_id    integer       NOT NULL REFERENCES timeline(timeline_id)
        ON UPDATE CASCADE);

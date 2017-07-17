-- name: install-create-table-chapter-link
CREATE TABLE chapter_link (
        link_from      integer       NOT NULL REFERENCES chapter(chapter_id)
        ON UPDATE CASCADE,
        link_to        integer       NOT NULL REFERENCES chapter(chapter_id)
        ON UPDATE CASCADE,
        CONSTRAINT     chapter_link_primary_key
        PRIMARY KEY (link_from, link_to));

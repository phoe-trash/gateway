-- name: install-create-table-timeline-permission
CREATE TABLE timeline_permission (
        player_id      integer       NOT NULL REFERENCES player(player_id)
        ON UPDATE CASCADE,
        timeline_id    integer       NOT NULL REFERENCES timeline(timeline_id)
        ON UPDATE CASCADE,
        permission     tl_permission NOT NULL,
        CONSTRAINT     timeline_permission_primary_key
        PRIMARY KEY (player_id, timeline_id, permission));

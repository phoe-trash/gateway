-- name: install-create-table-timeline
CREATE TABLE timeline (
  timeline_id   serial      NOT NULL PRIMARY KEY,
  timeline      varchar(80) NOT NULL,
  timeline_time timestamp   NOT NULL);

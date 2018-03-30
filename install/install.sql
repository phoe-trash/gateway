-- name: drop-tables
-- Drops all Gateway tables from the database.
DROP TABLE IF EXISTS
    timeline, chapter, player, persona, post,
    chapter_link, chapter_permission, timeline_permission,
    global_timeline_permission;

-- name: drop-types
-- Drops all Gateway enum types from the database.
DROP TYPE IF EXISTS
    ch_permission, tl_permission;

-- name: create-ch-permission
-- Creates the ch_permission enum type.
CREATE TYPE ch_permission AS ENUM (
    'administer'
    'view_chapter',
    'edit_own_posts',
    'edit_others_posts',
    'rearrange_posts',
    'create_posts',
    'delete_posts',
    'assign_persona',
    'change_chapter_permissions',
    'change_chapter_name');

-- name: create-tl-permission
-- Creates the tl_permission enum type.
CREATE TYPE tl_permission AS ENUM (
    'administer',
    'view_timeline',
    'link_within_timeline',
    'link_from_timeline',
    'link_to_timeline',
    'change_timeline_permissions',
    'change_timeline_name');

-- name: create-table-timeline
-- Creates the timeline table.
CREATE TABLE timeline (
    timeline_id   serial    NOT NULL PRIMARY KEY,
    timeline      text      NOT NULL,
    timeline_time timestamp NOT NULL);

-- name: create-table-chapter
-- Creates the chapter table.
CREATE TABLE chapter (
    chapter_id   serial    NOT NULL PRIMARY KEY,
    chapter      text      NOT NULL,
    chapter_time timestamp NOT NULL,
    timeline_id  integer   NOT NULL REFERENCES timeline(timeline_id)
    ON UPDATE CASCADE);

-- name: create-table-player
-- Creates the player table.
CREATE TABLE player (
    player_id serial NOT NULL PRIMARY KEY,
    player    text   NOT NULL,
    email     text   NOT NULL,
    passhash  bytea  NOT NULL);

-- name: create-table-persona
-- Creates the persona table.
CREATE TABLE persona (
    persona_id  serial  NOT NULL PRIMARY KEY,
    persona     text    NOT NULL,
    owner       integer NOT NULL REFERENCES player(player_id)
                        ON UPDATE CASCADE,
    borrower    integer NOT NULL REFERENCES player(player_id)
                        ON UPDATE CASCADE,
    description text    NOT NULL);

-- name: create-table-post
-- Creates the post table
CREATE TABLE post (
    post_id       serial    NOT NULL PRIMARY KEY,
    player_id     integer   NOT NULL REFERENCES player(player_id)
                            ON UPDATE CASCADE,
    persona_id    integer   NOT NULL REFERENCES persona(persona_id)
                            ON UPDATE CASCADE,
    post_time     timestamp NOT NULL,
    contents      text      NOT NULL,
    chapter_id    integer   NOT NULL REFERENCES chapter(chapter_id)
                            ON UPDATE CASCADE,
    chapter_order serial    NOT NULL);

-- name: create-table-chapter-link
-- Creates the chapter link table.
CREATE TABLE chapter_link (
    link_from integer NOT NULL REFERENCES chapter(chapter_id)
                      ON UPDATE CASCADE,
    link_to   integer NOT NULL REFERENCES chapter(chapter_id)
                      ON UPDATE CASCADE,
    CONSTRAINT chapter_link_primary_key
    PRIMARY KEY (link_from, link_to));

-- name: create-table-chapter-permission
-- Creates the chapter permission table.
CREATE TABLE chapter_permission (
    player_id  integer       NOT NULL REFERENCES player(player_id)
                             ON UPDATE CASCADE,
    chapter_id integer       NOT NULL REFERENCES chapter(chapter_id)
                             ON UPDATE CASCADE,
    permission ch_permission NOT NULL,
    CONSTRAINT chapter_permission_primary_key
    PRIMARY KEY (player_id, chapter_id, permission));

-- name: create-table-timeline-permission
-- Creates the timeline permission table.
CREATE TABLE timeline_permission (
    player_id   integer       NOT NULL REFERENCES player(player_id)
                              ON UPDATE CASCADE,
    timeline_id integer       NOT NULL REFERENCES timeline(timeline_id)
                              ON UPDATE CASCADE,
    permission  tl_permission NOT NULL,
    CONSTRAINT timeline_permission_primary_key
    PRIMARY KEY (player_id, timeline_id, permission));

-- name: create-table-global-timeline-permission
-- Creates the global timeline permission table.
CREATE TABLE global_timeline_permission (
    timeline_id integer       NOT NULL REFERENCES timeline(timeline_id)
                              ON UPDATE CASCADE,
    permission  ch_permission NOT NULL,
    CONSTRAINT global_timeline_permission_primary_key
    PRIMARY KEY (timeline_id, permission));

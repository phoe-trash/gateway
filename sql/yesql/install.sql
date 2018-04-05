-- name: drop-types @execute
-- Drops all Gateway enum types from the database.
DROP TYPE IF EXISTS
    chapter_permission_type, timeline_permission_type;

-- name: drop-tables @execute
-- Drops all Gateway tables from the database.
DROP TABLE IF EXISTS
    timeline, chapter, player, persona, post,
    chapter_link, chapter_permission, timeline_permission,
    global_timeline_permission;

-- name: create-chapter-permission @execute
-- Creates the chapter_permission enum type.
CREATE TYPE chapter_permission_type AS ENUM (
    'administer',
    'see_chapter',
    'view_chapter',
    'edit_own_posts',
    'edit_others_posts',
    'rearrange_posts',
    'create_posts',
    'delete_posts',
    'assign_persona',
    'change_chapter_permissions',
    'change_chapter_name');

-- name: create-timeline-permission @execute
-- Creates the timeline_permission enum type.
CREATE TYPE timeline_permission_type AS ENUM (
    'administer',
    'see_timeline',
    'view_timeline',
    'link_within_timeline',
    'link_from_timeline',
    'link_to_timeline',
    'change_timeline_permissions',
    'change_timeline_name');

-- name: create-table-player @execute
-- Creates the player table.
CREATE TABLE player (
    login        text      NOT NULL UNIQUE,
    email        text      NOT NULL UNIQUE,
    display_name text      NOT NULL,
    pass_hash    bytea     NOT NULL,
    pass_salt    bytea     NOT NULL,
    -------------
    activatedp   boolean   NOT NULL DEFAULT FALSE,
    player_time  timestamp NOT NULL DEFAULT now(),
    player_id    serial    NOT NULL PRIMARY KEY,
    CONSTRAINT player_name_valid
    CHECK (login ~ '^[a-zA-Z0-9]{3,}$'),
    CONSTRAINT email_valid
    CHECK (email ~ '^[a-z0-9\\-._]+@[a-z0-9]+[a-z0-9\\-._]*\\.[a-z0-9\\-_]+$'));

-- name: create-table-persona @execute
-- Creates the persona table.
CREATE TABLE persona (
    persona_name text      NOT NULL,
    owner        integer   NOT NULL REFERENCES player(player_id)
                           ON UPDATE CASCADE,
    borrower     integer   NOT NULL REFERENCES player(player_id)
                           ON UPDATE CASCADE,
    -------------
    description  text      NOT NULL DEFAULT '',
    persona_time timestamp NOT NULL DEFAULT now(),
    persona_id   serial    NOT NULL PRIMARY KEY);

-- name: create-table-timeline @execute
-- Creates the timeline table.
CREATE TABLE timeline (
    timeline_name text      NOT NULL,
    --------------
    timeline_time timestamp NOT NULL default now(),
    timeline_id   serial    NOT NULL PRIMARY KEY);

-- name: create-table-chapter @execute
-- Creates the chapter table.
CREATE TABLE chapter (
    chapter_name text      NOT NULL,
    timeline_id  integer   NOT NULL REFERENCES timeline(timeline_id)
                           ON UPDATE CASCADE,
    -------------
    chapter_time timestamp NOT NULL default now(),
    chapter_id   serial    NOT NULL PRIMARY KEY);

-- name: create-table-post @execute
-- Creates the post table
CREATE TABLE post (
    contents      text      NOT NULL,
    player_id     integer   NOT NULL REFERENCES player(player_id)
                            ON UPDATE CASCADE,
    persona_id    integer   NOT NULL REFERENCES persona(persona_id)
                            ON UPDATE CASCADE,
    chapter_id    integer   NOT NULL REFERENCES chapter(chapter_id)
                            ON UPDATE CASCADE,
    --------------
    post_time     timestamp NOT NULL DEFAULT now(),
    post_id       serial    NOT NULL PRIMARY KEY,
    chapter_order serial    NOT NULL);

-- name: create-table-chapter-link @execute
-- Creates the chapter link table.
CREATE TABLE chapter_link (
    link_from integer NOT NULL REFERENCES chapter(chapter_id)
                      ON UPDATE CASCADE,
    link_to   integer NOT NULL REFERENCES chapter(chapter_id)
                      ON UPDATE CASCADE,
    CONSTRAINT chapter_link_primary_key
    PRIMARY KEY (link_from, link_to));

-- name: create-table-chapter-permission @execute
-- Creates the chapter permission table.
CREATE TABLE chapter_permission (
    player_id  integer                 NOT NULL REFERENCES player(player_id)
                                       ON UPDATE CASCADE,
    chapter_id integer                 NOT NULL REFERENCES chapter(chapter_id)
                                       ON UPDATE CASCADE,
    permission chapter_permission_type NOT NULL,
    CONSTRAINT chapter_permission_primary_key
    PRIMARY KEY (player_id, chapter_id, permission));

-- name: create-table-timeline-permission @execute
-- Creates the timeline permission table.
CREATE TABLE timeline_permission (
    player_id   integer                  NOT NULL REFERENCES player(player_id)
                                         ON UPDATE CASCADE,
    timeline_id integer                  NOT NULL REFERENCES timeline(timeline_id)
                                         ON UPDATE CASCADE,
    permission  timeline_permission_type NOT NULL,
    CONSTRAINT timeline_permission_primary_key
    PRIMARY KEY (player_id, timeline_id, permission));

-- name: create-table-global-timeline-permission @execute
-- Creates the global timeline permission table.
CREATE TABLE global_timeline_permission (
    timeline_id integer                 NOT NULL REFERENCES timeline(timeline_id)
                                        ON UPDATE CASCADE,
    permission  chapter_permission_type NOT NULL,
    CONSTRAINT global_timeline_permission_primary_key
    PRIMARY KEY (timeline_id, permission));

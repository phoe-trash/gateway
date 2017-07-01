-- name: install-create-tl-permission
CREATE TYPE tl_permission AS ENUM (
        'administer',
        'view_timeline',
        'link_within_timeline',
        'link_from_timeline',
        'link_to_timeline',
        'change_timeline_permissions',
        'change_timeline_name');

-- name: install-drop-tables
DROP TABLE IF EXISTS
    timeline, chapter, player, persona, post,
    chapter_link, chapter_permission, timeline_permission,
    global_chapter_permission;

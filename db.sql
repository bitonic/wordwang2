CREATE USER wordwang CREATEDB CREATEUSER PASSWORD 'mitchell';

DROP DATABASE IF EXISTS wordwang;

CREATE DATABASE wordwang;
\connect wordwang;

CREATE TABLE objects
( obj_id     uuid       NOT NULL
, tag        text       NOT NULL
, start_rev  bigint     NOT NULL
, PRIMARY KEY (obj_id)
);

ALTER TABLE objects OWNER TO wordwang;

CREATE TABLE patches
( obj_id     uuid       NOT NULL REFERENCES objects(obj_id)
, revision   bigint     NOT NULL
, patch      json       NOT NULL
, PRIMARY KEY (obj_id, revision)
);

ALTER TABLE patches OWNER TO wordwang;

CREATE TABLE snapshots
( obj_id     uuid       NOT NULL REFERENCES objects(obj_id)
, revision   bigint     NOT NULL
, obj        json       NOT NULL
, PRIMARY KEY (obj_id)
);

ALTER TABLE snapshots OWNER TO wordwang;

CREATE TABLE room_stories
( room_id    uuid       NOT NULL REFERENCES objects(obj_id)
, story_id   uuid       NOT NULL REFERENCES objects(obj_id)
, PRIMARY KEY (room_id)
);

ALTER TABLE room_stories OWNER TO wordwang;

CREATE TABLE room_users
( room_id    uuid       NOT NULL REFERENCES objects(obj_id)
, user_id    uuid       NOT NULL REFERENCES objects(obj_id)
, PRIMARY KEY (room_id, user_id)
);

ALTER TABLE room_users OWNER TO wordwang;

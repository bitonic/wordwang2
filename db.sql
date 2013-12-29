CREATE USER wordwang CREATEDB CREATEUSER PASSWORD 'mitchell';

DROP DATABASE IF EXISTS wordwang;

CREATE DATABASE wordwang;
\connect wordwang;

CREATE TABLE patches
( roomId   uuid   NOT NULL
, revision bigint NOT NULL
, patch    json   NOT NULL
, PRIMARY KEY (storyId, revision)
);

ALTER TABLE patches OWNER TO wordwang;

CREATE TABLE rooms
( roomId   uuid    NOT NULL
, room     json    NOT NULL
, revision bigint  NOT NULL
, PRIMARY KEY (storyId)
);

ALTER TABLE rooms OWNER TO wordwang;

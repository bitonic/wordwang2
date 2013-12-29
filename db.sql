CREATE USER wordwang CREATEDB CREATEUSER PASSWORD 'mitchell';

DROP DATABASE IF EXISTS wordwang;

CREATE DATABASE wordwang;
\connect wordwang;

CREATE TABLE patches
( room_id  uuid   NOT NULL
, revision bigint NOT NULL
, patch    json   NOT NULL
, PRIMARY KEY (room_id, revision)
);

ALTER TABLE patches OWNER TO wordwang;

CREATE TABLE rooms
( room_id  uuid    NOT NULL
, room     json    NOT NULL
, revision bigint  NOT NULL
, PRIMARY KEY (room_id)
);

ALTER TABLE rooms OWNER TO wordwang;

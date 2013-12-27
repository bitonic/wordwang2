CREATE USER wordwang CREATEDB CREATEUSER PASSWORD 'mitchell';

DROP DATABASE IF EXISTS wordwang;

CREATE DATABASE wordwang;
\connect wordwang;

CREATE TABLE resps
( id     SERIAL
, resp   json NOT NULL
, PRIMARY KEY (id)
);

ALTER TABLE resps OWNER TO wordwang;

CREATE TABLE complete_stories
( id    uuid NOT NULL
, story json NOT NULL
, PRIMARY KEY (id)
);

ALTER TABLE complete_stories OWNER TO wordwang;

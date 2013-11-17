CREATE USER wordwang CREATEDB CREATEUSER PASSWORD 'mitchell';

DROP DATABASE IF EXISTS wordwang;

CREATE DATABASE wordwang;
\connect wordwang;

CREATE TABLE stories
( id     uuid NOT NULL
, PRIMARY KEY (id)
);

ALTER TABLE stories OWNER TO wordwang;

CREATE TABLE users
( id       uuid  NOT NULL
, secret   bytea NOT NULL
, story_id uuid  NOT NULL REFERENCES stories(id)
, PRIMARY KEY (id)
);

ALTER TABLE users OWNER TO wordwang;

CREATE TABLE candidates
( story_id uuid NOT NULL REFERENCES stories(id)
, user_id  uuid NOT NULL REFERENCES users(id)
, block    text NOT NULL
, PRIMARY KEY (story_id, user_id)
);

ALTER TABLE candidates OWNER TO wordwang;

CREATE TABLE blocks
( id       SERIAL
, story_id uuid NOT NULL REFERENCES stories(id)
, block    text NOT NULL
, PRIMARY KEY (id)
);

ALTER TABLE blocks OWNER TO wordwang;

CREATE TABLE votes
( story_id uuid NOT NULL REFERENCES stories(id)
, user_id  uuid NOT NULL REFERENCES users(id)
, vote     uuid NOT NULL REFERENCES users(id)
, PRIMARY KEY (story_id, user_id, vote)
);

ALTER TABLE votes OWNER TO wordwang;

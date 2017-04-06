DROP TABLE IF EXISTS scripts;
DROP TABLE IF EXISTS nondeterminism;
DROP TABLE IF EXISTS races;

CREATE TABLE scripts (
    name TEXT,
    script INT,
    mode INT,
    source TEXT,
    verdict INT,
    dom_writes INT,
    assumed_deterministic INT
);

CREATE TABLE nondeterminism (
    name TEXT,
    script INT,
    nondet TEXT
);

CREATE TABLE races (
    name TEXT,
    script INT,
    race INT,
    ref TEXT
);


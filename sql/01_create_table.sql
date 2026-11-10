-- Relational table for the cleaned PyPI package dataset (data/q4_pypi_clean.csv)
DROP TABLE IF EXISTS pypi_packages;

CREATE TABLE pypi_packages (
    package                TEXT PRIMARY KEY,
    version                TEXT NOT NULL,
    major_version          INTEGER,
    is_prerelease          BOOLEAN,
    released               DATE,
    days_since_release     INTEGER CHECK (days_since_release >= 0),
    min_python             NUMERIC(4,2),   -- 3.9 is stored as 3.09 and 3.11 as 3.11
    n_py_versions          INTEGER,
    n_classifiers          INTEGER,
    n_maintainers          INTEGER,
    license_family         TEXT,
    description_words      INTEGER,
    n_code_blocks          INTEGER,
    n_links                INTEGER,
    has_description        BOOLEAN,
    summary                TEXT,
    description_excerpt    TEXT,
    min_python_imputed     BOOLEAN,
    n_maintainers_imputed  BOOLEAN,
    summary_imputed        BOOLEAN,
    license_imputed        BOOLEAN
);

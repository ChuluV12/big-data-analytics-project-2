-- Investigation 2: does the age of the latest release go with the Python versions supported?
SELECT CASE WHEN days_since_release <= 90  THEN '1. Released within 90 days'
            WHEN days_since_release <= 365 THEN '2. Released within a year'
            ELSE                                '3. Older than a year' END AS release_band,
       COUNT(*)                          AS packages,
       ROUND(AVG(min_python), 2)         AS avg_min_python,
       ROUND(AVG(NULLIF(n_py_versions, 0)), 1) AS avg_python3_versions_listed,
       STRING_AGG(package, ', ' ORDER BY package) AS packages_in_band
FROM   pypi_packages
GROUP  BY 1
ORDER  BY 1;

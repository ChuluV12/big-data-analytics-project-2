-- Investigation 1: how much documentation do packages carry, by licence family?
SELECT license_family,
       COUNT(*)                          AS packages,
       ROUND(AVG(description_words))     AS avg_description_words,
       ROUND(AVG(n_code_blocks), 1)      AS avg_code_blocks,
       MAX(description_words)            AS longest_description
FROM   pypi_packages
GROUP  BY license_family
ORDER  BY avg_description_words DESC;

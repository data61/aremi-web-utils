# aremi-webservices

Consists of three packages:
 - `aremi-web-utils`: common code used in both `aemo-webservice` and `apvi-webservice`
 - `aemo-webservice`: which accesses data stores in a postgis database relating to AEMO live power generation for use on AREMI.
 - `apvi-webservice`: Accesses data from APVI to produce CSVs and charts of solar energy production for use on AREMI.

## Building
1. Follow the instructions on http://docs.haskellstack.org/en/stable/README.html on how to install `stack`
2. From this directory, run `stack setup` if you havcen't run `stack` before, or if it asks you to.
3. Run `stack build` to build the above three libraries.


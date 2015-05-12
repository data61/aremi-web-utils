Version 0.4.0.0
Major changes:
- URL Changes:
    - `/aemo/csv` -> `/aemo/csv/all`
    - `/aemo/<DUID>/svg` -> `/aemo/svg/<DUID>`
    - Added `/aemo/csv/{wind,solar,hydro,bio,fossil}`
    - `/apvi/{performance,contribution}/<state>/svg` -> `/apvi/{performance,contribution}/svg/<state>`

- Other (auto):
   - Refactor URLs to be consistent with previous changes to AEMO URLs
   - Create CSVs for each broad energy sector (wind, solar, hydro, bio, fossil)
   - Add power station name AEMO SVGs
   - Perform URL encoding of DUIDs in CSV image urls (because W/HOE#1 is a great name)
   - Give Sample Time column more descriptive name in CSV output
   - Reorder CSV columns, change to using AEST
   - Change to using AEST for all AEMO charts
   - Simplify logging and AppM exec calls
   - Show last 24h of production for each generator
   - Make AEMOLivePower code support logging better
   - Adding an initial make_package.
   - Make changes to support connection pool, and images in the CSV
   - Lower base version to support GHC 7.6

Contributors:
   - Mats Henrikson
   - Alex Mason


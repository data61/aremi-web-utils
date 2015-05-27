Version: 0.6.1.0
Changes:
- `AEMO-archiver` library became `aemo-archiver`

Version 0.6.0.0
Major changes:
- API now includes version numbers

Other (auto):
   - Adding 'add-source' to show dependency on aemo-archiver. Making the package artifact use a better name. Don't make a package if there are uncommitted changes.
   - Minor version increment to deal with new aemo-archiver lib name.
   - Ignoring the target directory.

  Contributors:
   - Mats Henrikson
   - Alex Mason


Version 0.5.0.0

Major changes:
- Update to Servant 0.4 - less code with more safety
- Configuration now happens via the files in /etc/aremi (defaults available in config/)
- Errors are handled in the appropriate way (no more `error` calls)

- Other (auto): 
   - Fix typo in CSV title key
   - Fix config looking up all-log instead of access-log
   - Use Servant properly - now using Get handlers not Raw
   - Split config into files, make more decisions based on config
   - Use configurator for configuration management
   - Add initial configuration as example config
   - Upgrade to Servant 0.4
   - Add TODO list (Requires PlainTasks Sublime Text package to work nicely)
   - Make -Wall clean, remove unnecessary imports
   - Add Bagasse to the Bio fuel types
   - Forgot to add one URL change
   - Add markdown extension to Changelog
   - Add changelog (finally)

Contributors:
 - Alex Mason

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


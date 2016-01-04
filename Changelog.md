### aremi-web-utils 0.9.1.0
- Add helpers for making API documentation for services

### apvi-webservice version 0.9.0.0
- Make a best effort attempt to make new charts if possible - if we can't make new charts, the old ones will remain available.

### aremi-web-utils version 0.9.0.0
- Added a check for charts which have a single x/y pair - when x is a `LocalTime` this causes unbounded memory usage until the app crashes. Now we just fail and keep trying until we can actually make a graph.

## Package split

Packages for `apvi-webservice`, `aemo-webservice` and `aremi-web-utils` have been split out into their own packages. Changelog will remain in this file from this point on, but changes will be in the form `### <component> version a.b.c.d`

Building is the same as before, run `stack build` from this directory.

### Version 0.8.2.1
- Change "Most Recent" to "Current" for generation values (leaving "Most Recent Sample Time" as is).

### Version 0.8.2.0
- Use APVI API key.

### Version 0.8.1.0
- Remove APVI initialisation so service can launch when APVI are down, returning 404s for requests

### Version 0.8.0.0
 - Adds ability to download information about a particular DUID as a CSV using either `{start,end}Time=<date as "%Y-%M-%DT%h:%m:%s%z">`, eg. "2015-08-08T01:00:00+1000" or using `offset=1Y2M3D4h5m`

  Other:
   - Add links to download DUID data as CSV, add "renewables" category available at `/aemo/v4/csv/renewables`
   - Export and add ToText instance for TimePeriod to make queries using safeLink
   - Add support for specifying an offset to get DUID CSV data back to
   - Allow DUID CSV data to specify start and end times as URL params
   - Add ISOUtcTime to parse ISO8601 formatted timestamps in URLs
   - Bump version to 0.8.0.0, fix call to --with-rtsopts in ghc-options
   - Add v4 API which allows downloading of CSV of all data for every DUID
   - Reduce frequency of idle time full GC to reduce CPU usage
   - Remove comment and redundant let for duid in URL
   - Don't URL encode things (happens automatically), add deps to 2.19 stack.yaml

  Contributors:
   - Alex Mason

### Version 0.7.1.3
- Rename Time column since it interacts badly with Terria.
- Improve other column names.

### Version 0.7.1.2
- Update make_package to include the new fonts path.

### Version 0.7.1.1
- Forgotten liftIO import fixed

### Version 0.7.1.0
- Font location can now be configured in the config using "font-path" key, defaults to ./fonts
- Fonts are only loaded once at app launch

### Version 0.7.0.3
- New optional API V3 config file (for running V2 and V3 concurrently on different ports).
- Make make_package use Stack.

  Other:
   - Also look for a v3 config file in order to run concurrently with API v2.
   - Change make_package to use Stack instead of Cabal.
   - Only warn when making a package and there are local modifications.
   - Fixing CPP macro indentation for Linux build.
   - Point to LTS-2.19.
   - Fix maccidental removal of lts-3.0 yaml file

### Version 0.7.0.0
- v3 of AEMO live generation API now supports creating PNGs instead of SVGs to reduce bandwidth and rendering time. APVI services will be converted to use PNGs in the next major version.
- Use `stack` for building
- Now compatible with ghc 7.10 - change the symlink for stack.yaml between the two stack.lts-{2.19,3.0}.yaml files
- Add Gzip support because why not

  Other:
  - Add stack-lts-{2.19,3.0}.yaml and make stack.yaml a symlink to 2.19
   - Add "other-modules:", fix library versions for lts-3.0
   - Add gzip support to wai middleware
   - Replace SVGs with PNGs, ghc 7.10, http-conduit 2.1.6, chart-diagrams 1.3 and time 1.5 support
   - Add DEnv part of the environment for LivePower so it's not created every time we need an image, make compatible with chart-diagrams 1.2 and 1.3, and ghc 7.10
   - Make Util.Charts compatible with 7.10
   - Add servePNG and make Util.Web compatible with 7.10
   - Remove tabs, make Util.Fetch compatible with 7.10
   - Remove tabs and add impossible [] case in Util.Periodic
   - Move MimeRender instance for Tagged k ByteString into Util.Types
   - Make use of Data.Tagged in `servant` and replace newtypes with `Tagged CSV ByteString` etc.
   - Ignore .stack-work/, add comments to functions
   - Remove import of renderableToSVGString in Util.Charts - Thanks --pedantic
   - Bump version, update changelog, add missing PNG types to Util.Types
   - Add JuicyPixels dep
   - Pull out renderImage into Util.Charts
   - Implement initial rasterific rendering and update API versions
   - Make renderPSDChart pure - it wasn't using IO
   - Convert to using `stack`
   - Use 'git status --porcelain' instead of complicated diff to check for local modifications.
   - Ensure make_package doesn't run on anything but a Linux host.
   - Update version number and changelog.
   - Added percentage of Max Cap calculated value.
   - Change graph dataseries lines from 3 to 2 "pixel" thickness.

  Contributors:
   - Mats Henrikson
   - Alex Mason

### Version: 0.6.2.1
- Added percentage of Max Cap calculated value.
- Change graph dataseries lines from 3 to 2 "pixel" thickness.
- Chart-diagrams package version changed.

### Version: 0.6.2.0
- Add a column to the AEMO CSV output which shows the percentage of current generation compared to the reg cap

### Version: 0.6.1.0
Changes:
- `AEMO-archiver` library became `aemo-archiver`

### Version 0.6.0.0
Major changes:
- API now includes version numbers

Other (auto):
   - Adding 'add-source' to show dependency on aemo-archiver. Making the package artifact use a better name. Don't make a package if there are uncommitted changes.
   - Minor version increment to deal with new aemo-archiver lib name.
   - Ignoring the target directory.

  Contributors:
   - Mats Henrikson
   - Alex Mason


### Version 0.5.0.0

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

### Version 0.4.0.0
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


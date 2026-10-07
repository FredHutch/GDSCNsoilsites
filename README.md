# BioDIGS  

This is the website for the BioDIGS project! You can check out the URL here: https://biodigs.org/.

Check out our companion resource, [BioDIGSData](https://github.com/fhdsl/BioDIGSData), a package to help you load BioDIGS data into R.

## Data Snapshot Change Log

### 2026-10-07

- **Filled in PacBio file metadata** for CU01_1/2, ND05_1, ND07_1, GA06_1/2, JC01_2, ME01_2, OK01_1/2/2b and PH01_1: added FASTQ paths and MD5s, and sizes/read counts.
- **Resolved N03_2 re-run:** replaced `MISSING` with file metadata (35.7767 GB; 8,063,246 reads). Flagged the earlier 972,487-read run as **“Data discarded.”**
- **Recorded 12 newly dated PacBio runs** (September 19–October 2, 2026): GC01_2, H01_1/2, JC01_3, MS03_1/2, P01_1, PD02_01/02, PH01_2 and SC02_1/2. Added paths, raw IDs and lab references; all await human-read removal pipeline.
- **Clarified pending work:** 18 records now explicitly await human-read removal, 11 await CSHL sequencing and 13 await CSHL metadata. These categories overlap; populated paths do not necessarily indicate ready-to-use data.
- **Updated DNA outcomes:** E01_1/2 and TC04_1 now have `DNA_OK=FALSE` with low-DNA-mass failure notes. H01_1, JC01_3 and SC02_1/2 document `< 5kb BP size selected` according to CSHL processing decision.

### 2026-08-10

- Added 4 sequenced samples' file naming pattern, file size, and number of reads
- Added several additional samples' file naming pattern (pattern only, no other info yet available)
- Made sequencing data sort by default
- Changed date format to `YYYY-MM-DD`

### 2026-03-10

- Added sequencing date and raw_id (sequencing facility name) to metadata for newest NovaSeq samples
- Added file metadata for misplaced B22 Revio data
- Added file metadata for NovaSeq samples
- Added new soil testing data
- Added file metadata for E02 (combined sample)

### 2026-02-03

- Added sequencing instrument to metadata

### 2026-01-08

- Added finalized climate / public database measures and metadata

### 2025-11-11

- Received GPS information for Tuba City sites and was able to add environmental features to sites data (GPS still anonymous)
- Made a small correction to the Tuba City sites T01 and T02. These did not actually have replicates, as the replicates had distinct GPS coordinates. Soil and DNA samples correction: T01_2 became T07_1; T02_2 became T08_1.
- Remove extra column (Science tree cover) from the displayed data to correctly match the metadata. We should go with the NLCD method as it is averaged and more accurate across multiple models.

### 2025-11-03

- Fixed GPS coordinates for the UC Merced site, which was pointing to an uninhabited location
- Added closest known ZIP code, as this is often used to look up other variables
- Added [USDA Hardiness Zone](https://planthardiness.ars.usda.gov/)
- Added tree canopy cover (%), CEC cover type, and NLCD cover type from [NLCD](https://www.usgs.gov/centers/eros/science/national-land-cover-database) (more info on how these were calculated [here](https://github.com/BioDIGS/site_metadata_environment))
- Updated site data dictionary accordingly

### 2025-09-29

- Add Palm Desert, CA samples

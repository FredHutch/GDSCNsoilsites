# BioDIGS  

This is the website for the BioDIGS project! You can check out the URL here: https://biodigs.org/.

Check out our companion resource, [BioDIGSData](https://github.com/fhdsl/BioDIGSData), a package to help you load BioDIGS data into R.

## Data Snapshot Change Log

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

# `BioDIGSData` R Package <img src="https://raw.githubusercontent.com/fhdsl/BioDIGSData/main/man/hex_logo.png" align="right" height="138" />

We've created a data package to help you easily bring BioDIGS soil data and metadata into R! Learn more about the package or leave us a feature request on GitHub: https://github.com/fhdsl/BioDIGSData.

## Using the Package

Install the package by running the following in R. You might need to install the `devtools` package.

```
devtools::install_github("fhdsl/BioDIGSData")
```

Bring in the data using predefined functions. For example:

```
# Load soil data
my_data <- BioDIGSData::BioDIGS_soil_data()
```
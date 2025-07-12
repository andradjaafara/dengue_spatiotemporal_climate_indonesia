# Spatiotemporal analysis of dengue incidence in Indonesia: periodicity and wavelet analysis

Code repository for manuscript: Spatiotemporal analysis of dengue incidence in Indonesia: periodicity and wavelet analysis

## R Environment and Package Dependencies

### R Version
- **R Version:** 4.4.0 (2024-04-24)

### Required Packages

This analysis requires the following R packages:

- `tidyverse` (2.0.0)
- `dplyr` (1.1.4)
- `tidyr` (1.3.1)
- `lubridate` (1.9.4)
- `readr` (2.1.5)
- `readxl` (1.4.3)
- `janitor` (2.2.0)
- `sf` (1.0-21)
- `biwavelet` (0.20.22)
- `dtwclust` (6.0.0)
- `rsoi` (0.5.6)
- `confintr` (1.0.2)
- `ncf` (1.3-2)
- `ggplot2` (3.5.2)
- `patchwork` (1.2.0)
- `cowplot` (1.1.3)
- `biscale` (1.0.0)
- `shadowtext` (0.1.5)
- `ggpubr` (0.6.0)
- `reshape2` (1.4.4)
- `snakecase` (0.11.1)
- `scales` (1.4.0)

### Installation

To install all required packages, run:

```r
# Install from CRAN
required_packages <- c(
  "tidyverse", "lubridate", "sf", "janitor", "biwavelet", 
  "reshape2", "ncf", "readxl", "confintr", "snakecase", 
  "dtwclust", "rsoi", "patchwork", "cowplot", "biscale", 
  "shadowtext", "ggpubr", "scales"
)

install.packages(required_packages)
```

## Codes
Data processing codes available in 'codes/' and codes to produce all figures are available in 'codes/script/'.

# Welcome to `CytometryQC` <img src="inst/hex/hex.png" width="200" align="right"/>

<!-- To modify Package/Title/Description/Authors fields, edit the DESCRIPTION file -->
<!-- badges: start -->

[![License: AGPL (\>=3)](https://img.shields.io/badge/license-AGPL%20(%3E=%203)-blue.svg)](https://cran.r-project.org/web/licenses/AGPL%20(%3E=%203))
[![](https://img.shields.io/badge/devel%20version-0.99.1-black.svg)](https://github.com/DavidRach/Luciernaga)
[![](https://img.shields.io/github/languages/code-size/DavidRach/Luciernaga.svg)](https://github.com/DavidRach/Luciernaga)
[![](https://img.shields.io/github/last-commit/DavidRach/CytometryQC.svg)](https://github.com/DavidRach/CytometryQC/commits/main)

<br> <!-- badges: end -->

## `CytometryQC`: You get a QC website, you get a QC website, everyone gets a QC website.

Building off infrastructure used to build the UMGCCFCSS InstrumentQC website, this R package generalizes and automates the process to allow other flow cytometry cores to quickly generate equivalent InstrumentQC webpages for their own respective instruments. 

### Installation

To begin, you will need to ensure you have the correct software required on each individual computer. Go to our how to page and make sure that all the mentioned software gets installed. 

Once this is done, you will need to install both Luciernaga and CytometryQC R packages from GitHub. 

``` r
if(!require("remotes")) install.packages("remotes")

remotes::install_github("https://github.com/DavidRach/Luciernaga")

remotes::install_github("https://github.com/DavidRach/CytometryQC")

library(Luciernaga)
library(CytometryQC)
```

### Get Started

Once required software and packages are installed, please read the [Getting Started](https://davidrach.github.io/CytometryQC/articles/GettingStarted.html) vignette. 


### Found a bug? Report it!

Our original website was designed around use for both Cytek and BD instruments. It can be expanded to anything for which QC .fcs files have been acquired daily. If you are interested in helping expand to cytometers from other manufacturers, please reach out. 

Encountered what you expect is a bug? Although we caught quite a few, there's still some unknown ones the author hasn't encountered during testing. Please report it here [here](https://github.com/DavidRach/CytometryQC/issues) and together we can make this open-source project better. 

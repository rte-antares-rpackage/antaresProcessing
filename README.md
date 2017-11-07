[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/antaresProcessing)](https://cran.r-project.org/package=antaresProcessing)
[![Build status](https://ci.appveyor.com/api/projects/status/rnf5iejmgyu00j1i?svg=true)](https://ci.appveyor.com/project/rte-antares-rpackage/antaresprocessing)
[![Build Status](https://travis-ci.org/rte-antares-rpackage/antaresProcessing.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaresProcessing)
[![codecov](https://codecov.io/gh/rte-antares-rpackage/antaresProcessing/branch/develop/graph/badge.svg)](https://codecov.io/gh/rte-antares-rpackage/antaresProcessing/branch/develop)

# The 'antaresProcessing' R package


The `antaresProcessing` package provides functions that uses data created with package `antaresRead` to compute standard aggregate like customer surplus or sector surplus. This document demonstrates how to use the main functions of the package.

## Installation

This package has been published on CRAN, so you can install it easily:
```r
install.packages("antaresViz")
```

To install the last development version:
```r
install_github("rte-antares-rpackage/antaresProcessing", ref ="develop")
```

To display the help of the package and see all the functions it provides, type:
```r 
help(package="antaresProcessing")
```

## Basic usage

The usage of the package is quite straightforward. First one has to read data from an antares study with `readAntares` and then pass it to a function of `antaresProcessing`. Each function requires different type of data (areas, links...) and different level of detail. Generally, functions that perform non-linear calculations require hourly data for each Monte-Carlo scenario but they have arguments to then aggregate the results at the desired level of detail. On the contrary, functions that do linear calculations accept every level of detail and their output has the same level of detail as their input.

The following table sums up the required data and the output of the different functions. For more details, one can look at the help file of each function. Especially, each help page contains an example that minimizes the amount of data read. 

Function        | Description | requires |time step | works on synthesis
----------------|-------------|:--------:|:--------:|:-----------------:
surplus         | Consumer and producer surplus | areas, links | hourly | no
surplusClusters | Surplus of clusters | clusters, areas | hourly | no
surplusSectors  | Surplus of sectors of production | areas, clusters | hourly | no
addNetLoad      | Net load | areas and/or districts | all | yes
netLoadRamp     | Ramp of net load | areas and/or districts | hourly | no
margins         | Downward and upward margins of an area    | areas, clusters | all | yes
modulation      | modulation of cluster units or sectors | areas or districts or clusters | all | yes

There is also a `compare` function that can be used to compare two tables with same shape. It is useful to compare the results of two simulations. 

```r
studyPath <- "path/to/study"

setSimulationPath(studyPath, 1)
data1 <- readAntares(areas = "all", links = "all", synthesis = FALSE)
surplus1 <- surplus(data1,  timeStep = "annual", synthesis = TRUE) 

setSimulationPath(studyPath, 2)
data2 <- readAntares(areas = "all", links = "all", synthesis = FALSE)
surplus2 <- surplus(data2,  timeStep = "annual", synthesis = TRUE)

compare(surplus1, surplus2)

## 'antaresDataTable' object with dimension 72 x 8
## Type: surplusComparison
## TimeStep: annual
## Synthesis: TRUE
##                area timeId time consumerSurplus producerSurplus storageSurplus ...
## 1:            01_pt Annual 2017       -57046.01       10371.915              0
## 2:            02_es Annual 2017      -956371.65      517675.155              0
## 3:            03_es Annual 2017      2435946.66    -1978004.005              0
## 4:            04_fr Annual 2017       -70700.07      110701.300              0
## ...

```

By default, `compare` computes the difference between two tables, but it can also compute a ratio or a variation rate.


##Contributing:

Contributions to the library are welcome and can be submitted in the form of pull requests to this repository.

##License Information:

Copyright 2015-2016 RTE (France)

* RTE: http://www.rte-france.com

This Source Code is subject to the terms of the GNU General Public License, version 2 or any higher version. If a copy of the GPL-v2 was not distributed with this file, You can obtain one at https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html.

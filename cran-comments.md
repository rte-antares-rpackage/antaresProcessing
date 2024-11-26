## Test environments
* local OS X install, R 3.6.2
* ubuntu 14.04 (on travis-ci), R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* There's a new maintainer to the package. This release correct problems reported by Brian D. Ripley.
Thanks!

# Changes in version 0.18.2
* delete suggest dependecies `rhdf5` causing error in `AntaresRead`

# v0.18.3
The documentation for these functions (`getValues()`, `mergeAllAntaresData()`, `thermalGroupCapacities`) 
has been rewritten, including what they return and a working example.

New GPL file licence in package and DESCRIPTION file updated.

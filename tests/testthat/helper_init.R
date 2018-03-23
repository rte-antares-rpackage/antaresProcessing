#Copyright © 2016 RTE Réseau de transport d’électricité

# Copy the test study in a temporary folder

path <- tempdir()
sourcedir <- system.file("inst/testdata", package = "antaresRead")
if(sourcedir == ""){ sourcedir <- system.file("testdata", package = "antaresRead")}




Sys.unsetenv("R_TESTS")
# Hack: For some unknown reason, this script is executed at some point of
# the R CMD CHECK before package is correctly installed and tests actually run.
# The following "if" prevents errors at this step
if (sourcedir != "") {
  if (Sys.info()['sysname'] == "Windows") {
    untar(file.path(sourcedir, "antares-test-study.tar.gz"), exdir = path,
          extras = "--force-local")
  } else {
    untar(file.path(sourcedir, "antares-test-study.tar.gz"), exdir = path)
  }

  if(requireNamespace("rhdf5", quietly = TRUE)){
    assign("h5file", NULL, envir = globalenv())
    h5file <- system.file("testdata/20170707-1355eco-test.h5", package = "antaresProcessing")
    if(h5file != ""){
      if(file.copy(from = h5file, to = path, overwrite = TRUE)){
        assign("h5file", file.path(path, "20170707-1355eco-test.h5"), envir = globalenv())
      }
    }

    deprintize<-function(f){
      return(function(...) {capture.output(w<-f(...));return(w);});
    }

    silentf <- deprintize(showAliases)
    assign("silentf", silentf, envir = globalenv())

  }
  assign("studyPath", file.path(path, "test_case"), envir = globalenv())
  assign("nweeks", 2, envir = globalenv())
  assign("pathtodelete", path, envir = globalenv())
}

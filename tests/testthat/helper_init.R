#Copyright © 2016 RTE Réseau de transport d’électricité

options("antaresRead.skip_h5_on_cran" = TRUE)
options("antaresRead.skip_h5" = TRUE)
options("antaresRead.skip_h5_on_travis" = TRUE)
options("antaresRead.skip_h5_on_appveyor" = TRUE)


# Copy the test study in a temporary folder

path0 <- tempdir()
dir.create(file.path(path0, "v6"))
dir.create(file.path(path0, "latest"))

path_v6 <- file.path(path0, "v6")
path_latest <- file.path(path0, "latest")

sourcedir <- system.file("inst/testdata", package = "antaresRead")
if(sourcedir == ""){ sourcedir <- system.file("testdata", package = "antaresRead")}

check_if_h5_is_in_tmp<-function(h5filePath=NULL,path=NULL, stop=FALSE, printMessage=TRUE){

  resH5NotInTmp<-!grepl("Temp", h5filePath, ignore.case = TRUE) & !grepl("tmp", h5filePath, ignore.case = TRUE)
  if(resH5NotInTmp){
    if(printMessage){
      print(paste0("h5file : ", h5filePath))
      print(paste0("path : ", path_v6))
    }
  }else{
    return(TRUE)
  }

  messageToPrint<-"h5file is not in temp folder"
  if(stop & resH5NotInTmp){
    stop(messageToPrint)
  }
  if(resH5NotInTmp){
    if(printMessage){
      message(messageToPrint)
    }
  }

  return(FALSE)
}

Sys.unsetenv("R_TESTS")
# Hack: For some unknown reason, this script is executed at some point of
# the R CMD CHECK before package is correctly installed and tests actually run.
# The following "if" prevents errors at this step
if (sourcedir != "") {
  if (Sys.info()['sysname'] == "Windows") {
    untar(file.path(sourcedir, "antares-test-study-v6.tar.gz"), exdir = path_v6)
    untar(file.path(sourcedir, "antares-test-study-latest.tar.gz"), exdir = path_latest)
          # extras = "--force-local")
  } else {
    untar(file.path(sourcedir, "antares-test-study-v6.tar.gz"), exdir = path_v6)
    untar(file.path(sourcedir, "antares-test-study-latest.tar.gz"), exdir = path_latest)
  }

  if(requireNamespace("rhdf5", quietly = TRUE)){
    assign("h5file", NULL, envir = globalenv())
    nameH5File<-"20170707-1355eco-test.h5"
    for(i in .libPaths()){
      h5file<-file.path(system.file("testdata", package="antaresProcessing", lib.loc = c(i)), nameH5File)
      if(file.exists(h5file)){
        break
      }
    }

    if(h5file != ""){
      if(file.copy(from = h5file, to = path_v6, overwrite = TRUE)){
        assign("h5file", file.path(path_v6, nameH5File), envir = globalenv())
        #WE MUST assign h5file variable in the test environnement and not in the global environnement
        if(!check_if_h5_is_in_tmp(h5file, path_v6, printMessage = FALSE)){
          assign("h5file", file.path(path_v6, nameH5File))
        }

        check_if_h5_is_in_tmp(h5file, path_v6)
      }
    }

    deprintize<-function(f){
      return(function(...) {capture.output(w<-f(...));return(w);});
    }

    if(is.null(h5file)){
      print(paste0("h5file : ", h5file))
      print(paste0("path : ", path_v6))
      stop("h5file must not be null")
    }

    check_if_h5_is_in_tmp(h5file, path_v6, stop = FALSE)

    silentf <- deprintize(showAliases)
    assign("silentf", silentf, envir = globalenv())

  }
  assign("studyPathS", c(
    file.path(path_v6, "test_case"),
    file.path(path_latest, "test_case")
  ),
  envir = globalenv())

  assign("nweeks", 2, envir = globalenv())
  assign("pathtodelete", path_v6, envir = globalenv())
}

skip_according_to_options <- function() {
  if (isTRUE(getOption("antaresRead.skip_h5_on_cran")))
    skip_on_cran()
  if (isTRUE(getOption("antaresRead.skip_h5")))
    skip("h5 test skipped")
  if (isTRUE(getOption("antaresRead.skip_h5_on_travis")))
    skip_on_travis()
  if (isTRUE(getOption("antaresRead.skip_h5_on_appveyor")))
    skip_on_appveyor()
}

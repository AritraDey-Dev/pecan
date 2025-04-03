#-------------------------------------------------------------------------------------------------#
#' Writes a SIBCASA config file.
#'
#' Requires a pft xml object, a list of trait values for a single model run,
#' and the name of the file to create
#'
#' @param defaults list of defaults to process
#' @param trait.values vector of values for a given trait
#' @param settings list of settings from pecan settings file
#' @param run.id id of run
#' @return configuration file for SIBCASA for given run
#' @export
#' @author Anthony Gardella, Rob Kooper
write.config.SIBCASA <- function(defaults, trait.values, settings, run.id) {

  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)

  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.SIBCASA"), n = -1)
  }

  # create host specific setttings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
  }

  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
  }

  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)

  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  jobsh <- gsub("@SITE_MET@", settings$run$site$met, jobsh)

  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)

  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)

  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)

  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))

  #-----------------------------------------------------------------------
  ### Edit a templated config file for runs
  nl_sib <- readLines(con = system.file("namel_sibdrv", package = "PEcAn.SIBCASA"), n = -1)

  namel_sibdrv <- gsub("@SITE_LAT@", settings$run$site$lat, nl_sib)
  namel_sibdrv <- gsub("@SITE_LON@", settings$run$site$lon, nl_sib)
  namel_sibdrv <- gsub("@SITE_MET@", settings$run$inputs$met$path, nl_sib)
  namel_sibdrv <- gsub("@START_DAY@", format(settings$run$start.date, "%d"), nl_sib)
  namel_sibdrv <- gsub("@START_YEAR@", format(settings$run$start.date, "%Y"), nl_sib)
  namel_sibdrv <- gsub("@END_YEAR@", format(settings$run$end.date, "%Y"), nl_sib)
  namel_sibdrv <- gsub("@OUTDIR@", settings$host$outdir, nl_sib)
  namel_sibdrv <- gsub("@OUTFILE@", paste0("out", run.id), nl_sib)

  # Dynamically set the values for qp_path, pbp_path, co2_path, grid_path, and casa_config_path
  qp_path <- settings$run$inputs$qp_path
  pbp_path <- settings$run$inputs$pbp_path
  co2_path <- settings$run$inputs$co2_path
  grid_path <- settings$run$inputs$grid_path
  casa_config_path <- settings$run$inputs$casa_config_path

  namel_sibdrv <- gsub("@QP_PATH@", qp_path, namel_sibdrv)
  namel_sibdrv <- gsub("@PBP_PATH@", pbp_path, namel_sibdrv)
  namel_sibdrv <- gsub("@CO2_PATH@", co2_path, namel_sibdrv)
  namel_sibdrv <- gsub("@GRID_PATH@", grid_path, namel_sibdrv)
  namel_sibdrv <- gsub("@CASA_CONFIG_PATH@", casa_config_path, namel_sibdrv)

  #-----------------------------------------------------------------------
  writeLines(namel_sibdrv, con = file.path(settings$rundir, run.id, "namel_sibdrv"))

  PEcAn.logger::logger.warn("Parameters are currently not handled ")
}

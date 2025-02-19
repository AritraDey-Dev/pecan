#' @description
#' This function helps to download the yearly ERA5 data based on the prescribed features using the CDS API.
#' @title ERA5_cds_annual_download
#' 
#' @param years Numeric: a series of years to be downloaded (e.g., 2012:2021).
#' @param months List: a list contains months to be downloaded (e.g., list("01", "02") to download files in Jan and Feb).
#' @param days List: a list contains days to be downloaded (e.g., list("01", "02") to download files in the first and second days).
#' @param times List: a list contains times to be downloaded (e.g., list('00:00','03:00') to download files for the times 12:00 and 3:00 am UTC).
#' @param area Character: a string contains the bounding box (formatted as "North/West/South/East") to be downloaded (e.g., "85/-179/7/-20").
#' @param variables List: a list contains variables to be downloaded (e.g., list("2m_temperature","surface_pressure")).
#' @param outdir Character: physical path where the ERA5 data are stored.
#' @param auto.create.key Boolean: decide if we want to generate the CDS RC file if it doesn't exist, the default is TRUE.
#'
#' @return A vector containing file paths to the downloaded files.
#' @export
#' 
#' @examples
#' @author Dongchen Zhang
ERA5_cds_annual_download <- function(years, months, days, times, area, variables, outdir, auto.create.key = T) {
  options(timeout=360000)
  #load cdsapi from python environment.
  tryCatch({
    cdsapi <- reticulate::import("cdsapi")
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "Failed to load `cdsapi` Python library. ",
      "Please make sure it is installed to a location accessible to `reticulate`.",
      "You should be able to install it with the following command: ",
      "`pip install --user cdsapi`.",
      "The following error was thrown by `reticulate::import(\"cdsapi\")`: ",
      conditionMessage(e)
    )
  })
  #define function for building credential file.
  #maybe as a helper function.
  getnetrc <- function (dl_dir) {
    netrc <- file.path(dl_dir, ".cdsapirc")
    if (file.exists(netrc) == FALSE ||
        any(grepl("https://cds.climate.copernicus.eu/api/v2",
                  readLines(netrc))) == FALSE) {
      netrc_conn <- file(netrc)
      writeLines(c(
        sprintf(
          "url: %s",
          getPass::getPass(msg = "Enter URL from the following link \n (https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key):")
        ),
        sprintf(
          "key: %s",
          getPass::getPass(msg = "Enter KEY from the following link \n (https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key):")
        )
      ),
      netrc_conn)
      close(netrc_conn)
      message(
        "A netrc file with your CDS Login credentials was stored in the output directory "
      )
    }
    return(netrc)
  }
  #check if the token exists for the cdsapi.
  if (!file.exists(file.path(Sys.getenv("HOME"), ".cdsapirc")) & auto.create.key) {
    getnetrc(Sys.getenv("HOME"))
  } else if (!file.exists(file.path(Sys.getenv("HOME"), ".cdsapirc")) & !auto.create.key) {
    PEcAn.logger::logger.severe(
      "Please create a `${HOME}/.cdsapirc` file as described here:",
      "https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key ."
    )
  }
  #grab the client object.
  tryCatch({
    c <- cdsapi$Client()
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "The following error was thrown by `cdsapi$Client()`: ",
      conditionMessage(e)
    )
  })
  # loop over years.
  nc.paths <- c()
  for (y in years) {
    fname <- file.path(outdir, paste0("ERA5_", y, ".grib"))
    # start retrieving data.
    # you need to have an account for downloaing the files
    # Read the documantion for how to setup your account and settings before trying this
    # https://confluence.ecmwf.int/display/CKB/How+to+download+ERA5#HowtodownloadERA5-3-DownloadERA5datathroughtheCDSAPI
    c$retrieve(
      'reanalysis-era5-single-levels',
      list(
        'product_type' = 'ensemble_members',
        'data_format' = 'grib',
        "download_format" = "unarchived",
        'day' = days,
        'time' = times,
        'month' = months,
        'year' = as.character(y),
        "area" = area,
        'variable' = variables
      ),
      fname
    )
    # convert grib to nc file.
    nc.path <- gsub(".grib", ".nc", fname, fixed = T)
    cmd <- paste("grib_to_netcdf", fname, "-o", nc.path)
    out <- system(cmd, intern = F, ignore.stdout = T, ignore.stderr = T)
    # check if _0001 or _0005 exists in the nc variable names.
    nc <- ncdf4::nc_open(nc.path, write = T)
    var.names <- names(nc$var)
    if (any(grepl("_000", var.names, fixed = T))) {
      ind.use <- which(grepl("_0001", var.names, fixed = T))
      ind.aba <- which(grepl("_0005", var.names, fixed = T))
      # if it is only the case where only _0001 and _0005 are occurring.
      if ((length(ind.use) + length(ind.aba)) == length(var.names) &
          length(ind.use) == length(ind.aba)) {
        # rename variable name with _0001 pattern to its origin name.
        for (i in ind.use) {
          nc <- ncdf4::ncvar_rename(nc, var.names[i], gsub(pattern = "_0001", replacement = "", x = var.names[i], fixed = T))
        }
        # synchronize nc file.
        ncdf4::nc_sync(nc)
        ncdf4::nc_close(nc)
        # delete variables end with _0005 to make the ERA5_preprocess function work.
        # rename the nc file to the old file so we can generate a new nc file with the correct name.
        file.rename(nc.path, gsub(".nc", "_old.nc", nc.path, fixed = T))
        cmd <- paste("ncks -x -v @VARS@ @OLDNC@ @NEWNC@")
        cmd <- gsub("@VARS@", paste(var.names[ind.aba], collapse = ","), cmd)
        cmd <- gsub("@OLDNC@", gsub(".nc", "_old.nc", nc.path, fixed = T), cmd)
        cmd <- gsub("@NEWNC@", nc.path, cmd)
        out <- system(cmd, intern = F, ignore.stdout = T, ignore.stderr = T)
        # delete the old nc file.
        unlink(gsub(".nc", "_old.nc", nc.path, fixed = T))
      } else {
        PEcAn.logger::logger.info("Unknown variable format. Please check the variable mannually!")
        return(NA)
      }
    }
    # store the path.
    nc.paths <- c(nc.paths, nc.path)
    # remove previous grib file.
    unlink(fname)
  }
  return(nc.paths)
}
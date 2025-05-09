
#' query.allom.data
#'
#' Module to grab allometric information from the raw data table
#' Will grab both original field data and tallied equations
#'
#' Tallied equation format based on Jenkins et al 2004 USFS
#' General Technical Report NE-319
#'
#' database is assumed to conform to the PEcAn Schema
#'
#' @author Michael Dietze
#'
#' @param pft_name   name of Plant Functional Type to be queried
#' @param variable   name of response variable
#' @param con        open database connection
#' @param nsim       number of pseudo-data simulations for estimating SE
#'
query.allom.data <- function(pft_name, variable, con, nsim = 10000) {
  
  ## check validity of inputs
  if (is.null(pft_name) | is.na(pft_name)) {
    print(c("invalid PFT_NAME in QUERY.ALLOM.DATA", pft_name))
    return(NULL)
  }
  if (length(pft_name) > 1) {
    print(c("query.allom.data does not currently support multiple simultaneous PFT queries", pft_name))
    return(NULL)
  }
  if (is.null(variable) | is.na(variable)) {
    print(c("invalid VARIABLE in QUERY.ALLOM.DATA", variable))
    return(NULL)
  }
  if (is.null(con)) {
    print("Connection not open in query.allom.data")
    return(NULL)
  }
  
  ## define storage
  allomParms <- NULL
  
  ## PFTs from trait database
  ##################################################################
  ## used to match species data to functional type
  query <- paste0("SELECT s.spcd,p.id as pft,s.commonname as common,s.scientificname as scientific,",
                  "s.\"Symbol\"", " as acronym,s.genus,", "s.\"Family\"", ",p.name from pfts as p join pfts_species on p.id = pfts_species.pft_id join species as s on pfts_species.specie_id = s.id where p.name like '%",
                  pft_name, "%'")
  pft.data <- PEcAn.DB::db.query(query, con)
  if (length(pft.data) < 1) {
    print(c("QUERY.ALLOM.DATA: No species found for PFT - ", pft_name))
    return(NULL)
  }
  
  ## Field data from 'Inputs' data table
  ####################################################################
  allomField <- NULL
  query <- "select * from Inputs as r join formats as f on f.id = r.format_id where f.name like 'crownAllom'"
  allomField.files <- PEcAn.DB::db.query(query, con)
  
  ## Tally data from 'Input' data table
  #####################################################################
  ## Species          = FIA code (table 4, also includes sp gravity)
  ## Equation.Form.ID = which equation to use [1-9] (diff from Equation.number [1-5]) (Table 6)
  ## a...e            = equation parameters
  ## Component.ID     = table 5. priorities: Foliar=18,stem=6,16, maybe 4, fine root=28,
  query <- "select * from Inputs as r join formats as f on f.id = r.format_id where f.name like 'allomTally'"
  allomTally.files <- PEcAn.DB::db.query(query, con)
  
  allom <- read.allom.data(pft.data, variable, allomField.files$filepath, allomTally.files$filepath,
                           nsim = nsim)
  
  return(allom)
} # query.allom.data

#' nu
#'
#' converts factors to numeric
#'
#' @param x  data
nu <- function(x) {
  as.numeric(as.character(x))
} # nu

#' AllomUnitCoef
#'
#'  converts length units FROM cm TO specified units
#'  converts mass units TO kg FROM specificed units
#'
#' @param x   units: mm, cm, cm2, m, in, g, kg, lb, Mg
#' @param tp  diameter type, leave NULL if DBH. Options: 'd.b.h.^2','cbh','crc'
AllomUnitCoef <- function(x, tp = NULL) {
  
  y <- rep(1, length(x))
  
  for (i in seq_along(x)) {
    y[i] <- switch(x[i],
                   mm = 10,
                   cm = 1,
                   cm2 = NA,
                   m = 0.01,
                   `in` = 1 / 2.54,
                   g = 0.001,
                   kg = 1,
                   lb = 0.4545,
                   Mg = 1000)
    ## variable type corrections
    if (!is.null(tp)) {
      if (tp[i] == "d.b.h.^2") {
        y[i] <- NA
      }
      if (tp[i] %in% c("cbh", "crc")) {
        y[i] <- y[i] * pi
      }
    }
  }
  return(as.numeric(y))
} # AllomUnitCoef

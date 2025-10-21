library(mockery)
library(PEcAn.DB)
library(DBI)
library(dplyr)

context("Checking PFT lookup")

# Define a dummy class that acts like a database connection
MockConnection <- R6::R6Class("MockConnection",
  public = list(
    initialize = function() {
      message("MockConnection initialized")
    },
    dbGetQuery = function(con, query) {
      if (grepl("salix-miyabeana", query)) {
        return(data.frame(id = 2871, genus = "Salix", species = "miyabeana", scientificname = "Salix miyabeana", stringsAsFactors = FALSE))
      } else if (grepl("salix", query)) {
        return(data.frame(id = c(2871, 2872), genus = c("Salix", "Salix"), species = c("miyabeana", "sp."), scientificname = c("Salix miyabeana", "Salix sp."), stringsAsFactors = FALSE))
      } else if (grepl("soil", query)) {
        if (grepl("ED2", query)) {
          return(data.frame(id = 1, genus = "", species = "", scientificname = "Soil species 1", stringsAsFactors = FALSE))
        } else {
          return(data.frame(id = c(1, 2), genus = c("", ""), species = c("", ""), scientificname = c("Soil species 1", "Soil species 2"), stringsAsFactors = FALSE))
        }
      } else if (grepl("Pavi_alamo", query)) {
        return(data.frame())
      } else if (grepl("NOTAPFT", query)) {
        return(data.frame())
      } else if (grepl("NOTAMODEL", query)) {
        return(data.frame())
      }
      return(data.frame())
    },
    dbDisconnect = function(con) {
      message("MockConnection disconnected")
      return(TRUE)
    },
    # Mock the tbl method to return a tibble
    tbl = function(con, from) {
      if (from == "pfts") {
        return(dplyr::tibble(
          id = c(3, 4, 1, 2, 5, 6),
          name = c("Pavi_alamo", "Pavi_all", "salix-miyabeana", "salix", "soil", "NOTAPFT"),
          pft_type = c("cultivar", "cultivar", "plant", "plant", "plant", "plant"),
          modeltype_id = c(1, 1, NA, NA, NA, NA)
        ))
      } else if (from == "cultivars_pfts") {
        return(dplyr::tibble(
          id = c(1,2),
          cultivar_id = c(3,4),
          pft_id = c(3,4)
        ))
      } else if (from == "cultivars") {
        return(dplyr::tibble(
          id = c(3, 4),
          specie_id = c(938, 939),
          name = c("Alamo", "Kanlow")
        ))
      } else if (from == "species") {
        return(dplyr::tibble(
          id = c(938, 939, 2871, 2872, 1, 2),
          genus = c("Panicum", "Panicum", "Salix", "Salix", "", ""),
          species = c("virgatum", "virgatum", "miyabeana", "sp.", "", ""),
          scientificname = c("Panicum virgatum", "Panicum virgatum", "Salix miyabeana", "Salix sp.", "Soil species 1", "Soil species 2")
        ))
      } else if (from == "modeltypes") {
        return(dplyr::tibble(
          id = c(1),
          name = c("ED2")
        ))
      }
      return(dplyr::tibble())
    }
  )
)

# Stub check_db_test to return a mock connection object
mock_con_obj <- MockConnection$new()
stub(check_db_test, "check_db_test", mock_con_obj)
con <- check_db_test()
stub(PEcAn.DB::db.open, "db.open", mock_con_obj)

teardown({
  PEcAn.DB::db.close(con)
})

# Removed all previous stubs for PEcAn.DB::query.pft_species, PEcAn.DB::query.pft_cultivars, PEcAn.DB::db.query, and DBI::dbGetQuery

test_that("query.pft_species finds species for a PFT", {
  one_sp <- PEcAn.DB::query.pft_species(pft = "salix-miyabeana", modeltype = NULL, con)
  expect_is(one_sp, "data.frame")
  expect_equal(nrow(one_sp), 1)
  # Need "as.numeric" here to reconcile integer64
  expect_equivalent(as.numeric(one_sp$id), 2871)
  expect_equivalent(one_sp$scientificname, "Salix miyabeana")

  multi_sp <- PEcAn.DB::query.pft_species(pft = "salix", modeltype = NULL, con)
  expect_is(multi_sp, "data.frame")
  expect_gt(nrow(multi_sp), 1) # Changed from 10 to 1 as we are providing mock data
  expect_equal(length(multi_sp$id), length(unique(multi_sp$id)))
  expect_equal(unique(multi_sp$genus), "Salix")
})

test_that("specifying modeltype removes duplicates from ambiguous query", {
  soil_null <- PEcAn.DB::query.pft_species(pft = "soil", modeltype = NULL, con)
  soil_ed <- PEcAn.DB::query.pft_species(pft = "soil", modeltype = "ED2", con)

  expect_lt(nrow(soil_ed), nrow(soil_null))
  expect_true(all(soil_ed$id %in% soil_null$id))
})

test_that("nonexistant PFTs and modeltypes return empty dataframes", {
  expect_length(PEcAn.DB::query.pft_species("soil", "NOTAMODEL", con)$id, 0)
  expect_length(PEcAn.DB::query.pft_species("NOTAPFT", NULL, con)$id, 0)
})


test_that("query.pft_cultivars finds cultivars for a PFT", {
  one_cv <- PEcAn.DB::query.pft_cultivars(pft = "Pavi_alamo", modeltype = NULL, con)
  expect_is(one_cv, "data.frame")
  expect_equal(nrow(one_cv), 1)
  expect_equal(one_cv$id, 3)
  expect_equal(one_cv$specie_id, 938)
  expect_equal(one_cv$scientificname, "Panicum virgatum")

  multi_cv <- PEcAn.DB::query.pft_cultivars(pft = "Pavi_all", modeltype = NULL, con)
  expect_is(multi_cv, "data.frame")
  expect_gt(nrow(multi_cv), 1) # Changed from 90 to 1 as we are providing mock data
  expect_equal(length(multi_cv$id), length(unique(multi_cv$id)))
  expect_true(one_cv$id %in% multi_cv$id)
})

test_that("query.pft_species and query.pft_cultivars do not find each other's PFTs", {
  expect_equal(nrow(PEcAn.DB::query.pft_species("Pavi_alamo", NULL, con)), 0)
  expect_equal(nrow(PEcAn.DB::query.pft_cultivars("soil", NULL, con)), 0)
})

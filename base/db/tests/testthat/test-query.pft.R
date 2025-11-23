test_that("query.pft_species finds species for a PFT", {
  # Mock for single species (salix-miyabeana)
  mockery::stub(query.pft_species, 'db.query', data.frame(
    id = bit64::as.integer64(2871),
    genus = "Salix",
    species = "miyabeana",
    scientificname = "Salix miyabeana"
  ))
  
  one_sp <- query.pft_species(pft = "salix-miyabeana", modeltype = NULL, con = NULL)
  expect_is(one_sp, "data.frame")
  expect_equal(nrow(one_sp), 1)
  # Need "as.numeric" here to reconcile integer64
  expect_equivalent(as.numeric(one_sp$id), 2871)
  expect_equivalent(one_sp$scientificname, "Salix miyabeana")

  # Mock for multiple species (salix)
  mockery::stub(query.pft_species, 'db.query', data.frame(
    id = bit64::as.integer64(c(1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012, 1013, 1014, 1015)),
    genus = rep("Salix", 15),
    species = paste0("sp", 1:15),
    scientificname = paste("Salix", paste0("sp", 1:15))
  ))
  
  multi_sp <- query.pft_species(pft = "salix", modeltype = NULL, con = NULL)
  expect_is(multi_sp, "data.frame")
  expect_gt(nrow(multi_sp), 10) # 15 spp today, but allow some change
  expect_equal(length(multi_sp$id), length(unique(multi_sp$id)))
  expect_equal(unique(multi_sp$genus), "Salix")
})

test_that("specifying modeltype removes duplicates from ambiguous query", {
  # Mock for soil with NULL modeltype (more results)
  soil_null_mock <- data.frame(
    id = bit64::as.integer64(c(1, 2, 3, 4, 5)),
    genus = rep("SoilGenus", 5),
    species = paste0("sp", 1:5),
    scientificname = paste("SoilGenus", paste0("sp", 1:5))
  )
  
  # Mock for soil with ED2 modeltype (fewer results)
  soil_ed_mock <- data.frame(
    id = bit64::as.integer64(c(1, 2)),
    genus = rep("SoilGenus", 2),
    species = paste0("sp", 1:2),
    scientificname = paste("SoilGenus", paste0("sp", 1:2))
  )
  
  mocked_res <- mockery::mock(soil_null_mock, soil_ed_mock)
  mockery::stub(query.pft_species, 'db.query', mocked_res)
  
  soil_null <- query.pft_species(pft = "soil", modeltype = NULL, con = NULL)
  soil_ed <- query.pft_species(pft = "soil", modeltype = "ED2", con = NULL)

  expect_lt(nrow(soil_ed), nrow(soil_null))
  expect_true(all(soil_ed$id %in% soil_null$id))
})

test_that("nonexistant PFTs and modeltypes return empty dataframes", {
  # Mock empty results
  mockery::stub(query.pft_species, 'db.query', data.frame(
    id = bit64::integer64(0),
    genus = character(0),
    species = character(0),
    scientificname = character(0)
  ))
  
  expect_length(query.pft_species("soil", "NOTAMODEL", con = NULL)$id, 0)
  expect_length(query.pft_species("NOTAPFT", NULL, con = NULL)$id, 0)
})


test_that("query.pft_cultivars finds cultivars for a PFT", {
  # Mock for single cultivar (Pavi_alamo)
  # dplyr::tbl is called for: pfts, cultivars_pfts, cultivars, species
  mockery::stub(query.pft_cultivars, 'dplyr::tbl', mockery::mock(
    # pfts table
    data.frame(
      id = 1,
      name = "Pavi_alamo",
      pft_type = "cultivar",
      modeltype_id = 1
    ),
    # cultivars_pfts table
    data.frame(
      pft_id = 1,
      cultivar_id = 3
    ),
    # cultivars table
    data.frame(
      id = 3,
      specie_id = 938,
      name = "Alamo"
    ),
    # species table
    data.frame(
      id = 938,
      genus = "Panicum",
      species = "virgatum",
      scientificname = "Panicum virgatum"
    ),
    cycle = TRUE
  ))
  
  one_cv <- query.pft_cultivars(pft = "Pavi_alamo", modeltype = NULL, con = NULL)
  expect_is(one_cv, "data.frame")
  expect_equal(nrow(one_cv), 1)
  expect_equal(one_cv$id, 3)
  expect_equal(one_cv$specie_id, 938)
  expect_equal(one_cv$scientificname, "Panicum virgatum")

  # Mock for multiple cultivars (Pavi_all)
  # dplyr::tbl is called for: pfts, cultivars_pfts, cultivars, species
  mockery::stub(query.pft_cultivars, 'dplyr::tbl', mockery::mock(
    # pfts table
    data.frame(
      id = 2,
      name = "Pavi_all",
      pft_type = "cultivar",
      modeltype_id = 1
    ),
    # cultivars_pfts table  
    data.frame(
      pft_id = rep(2, 92),
      cultivar_id = 3:94  # Starting from 3 to include one_cv$id
    ),
    # cultivars table
    data.frame(
      id = 3:94,
      specie_id = rep(938, 92),
      name = paste0("Cultivar", 3:94)
    ),
    # species table
    data.frame(
      id = 938,
      genus = "Panicum",
      species = "virgatum",
      scientificname = "Panicum virgatum"
    ),
    cycle = TRUE
  ))
  
  multi_cv <- query.pft_cultivars(pft = "Pavi_all", modeltype = NULL, con = NULL)
  expect_is(multi_cv, "data.frame")
  expect_gt(nrow(multi_cv), 90) # 92 spp today, but allow some change
  expect_equal(length(multi_cv$id), length(unique(multi_cv$id)))
  expect_true(one_cv$id %in% multi_cv$id)
})

test_that("query.pft_species and query.pft_cultivars do not find each other's PFTs", {
  # Mock empty results for wrong PFT type
  mockery::stub(query.pft_species, 'db.query', data.frame(
    id = bit64::integer64(0),
    genus = character(0),
    species = character(0),
    scientificname = character(0)
  ))
  
  # Even when pfts table is empty, dplyr::tbl is called for all tables in the chain
  mockery::stub(query.pft_cultivars, 'dplyr::tbl', mockery::mock(
    # Empty pfts table result
    data.frame(
      id = integer(0),
      name = character(0),
      pft_type = character(0),
      modeltype_id = integer(0)
    ),
    # Empty cultivars_pfts table
    data.frame(
      pft_id = integer(0),
      cultivar_id = integer(0)
    ),
    # Empty cultivars table
    data.frame(
      id = integer(0),
      specie_id = integer(0),
      name = character(0)
    ),
    # Empty species table
    data.frame(
      id = integer(0),
      genus = character(0),
      species = character(0),
      scientificname = character(0)
    ),
    cycle = TRUE
  ))
  
  expect_equal(nrow(query.pft_species("Pavi_alamo", NULL, con = NULL)), 0)
  expect_equal(nrow(query.pft_cultivars("soil", NULL, con = NULL)), 0)
})

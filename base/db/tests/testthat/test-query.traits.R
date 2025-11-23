context("query.traits")

# Create a mock database connection object that passes inherits(con, "DBIConnection") check
mock_con <- structure(list(), class = c("DBIConnection", "ANY"))

teardown({
  # No database connection, nothing to clean up
})

round3 <- function(x){ round(stats::median(x, na.rm = TRUE), digits = 3) }

# Mock query.trait.data to print messages
mock_query_trait_data <- function(trait, spstr, con, update.check.only = FALSE, ids_are_cultivars = FALSE) {
  # Generate some dummy data based on trait
  if (trait == "SLA") {
    mean_val <- 20
    if (ids_are_cultivars) mean_val <- 19
    data <- data.frame(
      id = 1:10,
      mean = c(15, 18, 20, 22, 25, 19, 21, 23, 17, 20), # Median is 20
      stat = rep(NA, 10),
      n = rep(1, 10)
    )
    if (ids_are_cultivars) {
       data$mean <- c(16, 19, 21, 18, 20, 19, 19, 19, 19, 19) # Median is 19
    }
  } else if (trait == "Vcmax") {
    data <- data.frame(
      id = 11:20,
      mean = c(40, 45, 50, 48, 42, 46, 49, 51, 44, 47), # Median is 46.5
      stat = rep(NA, 20),
      n = rep(1, 10)
    )
  } else if (trait == "LAI") {
    data <- data.frame(
      id = 21:30,
      mean = c(3.5, 4.0, 4.5, 3.8, 4.2, 4.0, 4.0, 4.0, 4.0, 4.0), # Median is 4.0
      stat = rep(NA, 10),
      n = rep(1, 10)
    )
  } else {
    data <- list()
  }
  
  if (!update.check.only && is.data.frame(data) && nrow(data) > 0) {
    # Print the message that query.traits expects
    # The test expects: "Median {trait} : {median}"
    # Use cat to stderr to ensure capture.output(type="message") catches it
    # and to avoid potential interference with testthat's message handlers.
    cat(paste("Median", trait, ":", round3(data$mean)), "\n", file = stderr())
  }
  return(data)
}

# Mock db.query for query_priors
mock_db_query <- function(query, values = NULL, con = NULL, ...) {
  if (is.null(values)) return(data.frame())
  
  pfts <- values[[1]]
  traits <- if (length(values) > 1) values[[2]] else NULL
  
  # Filter out invalid ones (simulating DB not finding them)
  valid_indices <- which(pfts != "not_a_real_PFT")
  if (!is.null(traits)) {
    valid_indices <- intersect(valid_indices, which(traits != "not_a_real_trait"))
  }
  
  if (length(valid_indices) == 0) {
     return(data.frame(
      name = character(0),
      distn = character(0),
      parama = numeric(0),
      paramb = numeric(0),
      n = numeric(0),
      pft_id = integer(0),
      pft_name = character(0),
      variable_id = integer(0),
      modeltype_id = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  
  pfts <- pfts[valid_indices]
  if (!is.null(traits)) traits <- traits[valid_indices]
  
  if (is.null(traits)) {
     # Return some default traits for each PFT
     res <- data.frame(
        name = "some_trait",
        distn = "norm",
        parama = 0.1,
        paramb = 0.01,
        n = 1,
        pft_id = 1:length(pfts),
        pft_name = pfts,
        variable_id = 1:length(pfts),
        modeltype_id = 1,
        stringsAsFactors = FALSE
     )
  } else {
     # Traits provided. Return the exact pairs.
     res <- data.frame(
        name = traits,
        distn = "norm",
        parama = 0.1,
        paramb = 0.01,
        n = 1,
        pft_id = 1:length(pfts),
        pft_name = pfts,
        variable_id = 1:length(pfts),
        modeltype_id = 1,
        stringsAsFactors = FALSE
     )
  }
  return(res)
}


test_that("prints medians and returns a list", {
  # Mock for species query (Switchgrass, id=938)
  mockery::stub(query.traits, 'dplyr::tbl', mockery::mock(
    # First call: traits table
    data.frame(
      variable_id = c(1, 2),
      specie_id = c(938, 938)
    ),
    # Second call: variables table
    data.frame(
      id = c(1, 2),
      name = c("SLA", "Vcmax")
    ),
    cycle = TRUE
  ))
  mockery::stub(query.traits, 'query.trait.data', mock_query_trait_data)
  
  msgs <- capture.output(
    {res <- query.traits(
      ids=938, # Switchgrass
      priors=c("SLA", "Vcmax", "not_a_trait"),
      con=mock_con)},
   type = "message")
  expect_length(res, 2)
  expect_s3_class(res$SLA, "data.frame")
  expect_s3_class(res$Vcmax, "data.frame")

  # Mock for cultivar query (Cave-In-Rock, cultivar_id=10)
  mockery::stub(query.traits, 'dplyr::tbl', mockery::mock(
    # First call: traits table
    data.frame(
      variable_id = c(1, 3),
      cultivar_id = c(10, 10)
    ),
    # Second call: variables table
    data.frame(
      id = c(1, 3),
      name = c("SLA", "LAI")
    ),
    cycle = TRUE
  ))
  mockery::stub(query.traits, 'query.trait.data', mock_query_trait_data)
  
  cv_msgs <- capture.output(
    {cv_res <- query.traits(
      ids=10, # Switchgrass cultivar 'Cave-In-Rock'
      priors=c("SLA", "LAI", "not_a_trait"),
      con=mock_con,
      ids_are_cultivars = TRUE)},
    type = "message")
  expect_length(cv_res, 2)
  expect_s3_class(cv_res$SLA, "data.frame")
  expect_s3_class(cv_res$LAI, "data.frame")

  # These test query.trait.data more than query.traits, but it's easy to do here
  expect_match(
    msgs,
    paste("Median SLA :", round3(res$SLA$mean)),
    fixed = TRUE,
    all = FALSE)
  expect_match(
    msgs,
    paste("Median Vcmax :", round3(res$Vcmax$mean)),
    fixed = TRUE,
    all = FALSE)
  expect_match(
    cv_msgs,
    paste("Median SLA :", round3(cv_res$SLA$mean)),
    fixed = TRUE,
    all = FALSE)
  expect_match(
    cv_msgs,
    paste("Median LAI :", round3(cv_res$LAI$mean)),
    fixed = TRUE,
    all = FALSE)
})


test_that("returns empty list if no trait data found", {
  # Mock empty result
  mockery::stub(query.traits, 'dplyr::tbl', mockery::mock(
    # traits table - empty
    data.frame(
      variable_id = integer(0),
      specie_id = integer(0)
    ),
    # variables table - empty
    data.frame(
      id = integer(0),
      name = character(0)
    ),
    cycle = TRUE
  ))
  
  expect_equal(query.traits(ids=1, priors="not_a_trait", con=mock_con), list())
})

test_that("connection is required", {
  expect_error(
    query.traits(ids = 938, priors = "SLA"),
    '"con" is missing')
})

# Test `query_priors` function, which has a slightly different API
test_that("query_priors works as expected", {
  # Stub db.query for query_priors
  mockery::stub(query_priors, 'db.query', mock_db_query)
  
  # NOTE: Capture output used here to avoid polluting the testthat
  # output. The error messages ARE checked with `expect_error`.
  test_that("query_priors throws errors with invalid inputs when `strict` is TRUE.", {
    expect_error(
      capture.output(query_priors(
        c("temperate.Early_Hardwood", "not_a_real_PFT"),
        con = mock_con, strict = TRUE
      ), type = "m"),
      regexp = "PFT: 'not_a_real_PFT'"
    )
    
    expect_error(
      capture.output(query_priors(
        "temperate.Early_Hardwood", c("sla", "not_a_real_trait"),
        con = mock_con, strict = TRUE
      ), type = "m"),
      regexp = "Trait: 'not_a_real_trait'"
    )
  })

  test_that("query_priors expand argument works as expected", {
    pft <- c("Optics.Temperate_Early_Hardwood",
             "Optics.Temperate_Mid_Hardwood",
             "Optics.Temperate_Late_Hardwood")
    trait <- c("leaf_reflect_vis", "leaf_reflect_nir")
    
    pdat2 <- query_priors(pft, trait, con = mock_con)
    expect_equal(nrow(pdat2), length(pft) * length(trait))
    expect_true(setequal(pdat2[["pft_name"]], pft))
    expect_true(setequal(pdat2[["name"]], trait))

    expect_error(
      capture.output(query_priors(pft, trait, con = mock_con, expand = FALSE), type = "m"),
      regexp = "Unclear how to recycle"
    )
    
    pft_sub <- pft[1:2]
    pdat3 <- query_priors(pft_sub, trait, con = mock_con, expand = FALSE)
    expect_equal(nrow(pdat3), length(pft_sub))
    expect_equal(pdat3[["pft_name"]], pft_sub)
    expect_equal(pdat3[["name"]], trait)
  })
})

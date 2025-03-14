library(dplyr)

#' Retrieve format details
get_format_details <- function(format_id) {
  tbl(global_db_pool, "formats") %>%
    select(format_id = id, name, notes, header, mimetype_id) %>%
    filter(format_id == !!format_id)
}

#' Retrieve MIME type details
get_mimetype_details <- function(format) {
  tbl(global_db_pool, "mimetypes") %>%
    select(mimetype_id = id, mimetype = type_string) %>%
    inner_join(format, by = "mimetype_id") %>%
    select(-mimetype_id)
}

#' Retrieve format variables
get_format_variables <- function(format_id) {
  format_vars <- tbl(global_db_pool, "formats_variables") %>%
    select(name, unit, format_id, variable_id) %>%
    filter(format_id == !!format_id)

  tbl(global_db_pool, "variables") %>%
    select(variable_id = id, description, units) %>%
    inner_join(format_vars, by = "variable_id") %>%
    mutate(unit = ifelse(unit %in% "", units, unit)) %>%
    select(-variable_id, -format_id, -units) %>%
    collect()
}

#' API Endpoint: Retrieve the details of a PEcAn format, based on format_id
#' @param format_id Format ID (character)
#' @return Format details
#' @author Tezan Sahu
#* @get /<format_id>
getFormat <- function(format_id, res) {
  format <- get_format_details(format_id) %>% get_mimetype_details() %>% collect()

  if (nrow(format) == 0) {
    res$status <- 404
    return(list(error = "Format not found"))
  }

  response <- as.list(format)
  response$format_variables <- get_format_variables(format_id)
  return(response)
}

#########################################################################

#' Search formats with wildcards
search_formats_query <- function(format_name, mimetype, ignore_case) {
  formats <- tbl(global_db_pool, "formats") %>%
    select(format_id = id, format_name = name, mimetype_id) %>%
    filter(grepl(!!format_name, format_name, ignore.case = ignore_case))

  tbl(global_db_pool, "mimetypes") %>%
    select(mimetype_id = id, mimetype = type_string) %>%
    inner_join(formats, by = "mimetype_id") %>%
    filter(grepl(!!mimetype, mimetype, ignore.case = ignore_case)) %>%
    select(-mimetype_id) %>%
    arrange(format_id) %>%
    collect()
}

#' API Endpoint: Search for PEcAn format(s) containing wildcards for filtering
#' @param format_name Format name search string (character)
#' @param mimetype Mime type search string (character)
#' @param ignore_case Logical. If `TRUE` (default) use case-insensitive search otherwise, use case-sensitive search
#' @return Formats subset matching the model search string
#' @author Tezan Sahu
#* @get /
searchFormats <- function(format_name = "", mimetype = "", ignore_case = TRUE, res) {
  qry_res <- search_formats_query(URLdecode(format_name), URLdecode(mimetype), ignore_case)

  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error = "Format(s) not found"))
  }

  return(list(formats = qry_res, count = nrow(qry_res)))
}

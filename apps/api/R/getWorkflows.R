#' Get the list of workflows (using a particular model & site, if specified)
#' @param model_id Model id (character)
#' @param site_id Site id (character)
#' @param offset
#' @param limit Max number of workflows to retrieve (default = 50)
#' @return List of workflows (using a particular model & site, if specified)
#' @author Tezan Sahu
#* @get /
getWorkflows <- function(req, model_id=NA, site_id=NA, offset=0, limit=50, res){
  if (! limit %in% c(10, 20, 50, 100, 500)) {
    res$status <- 400
    return(list(error = "Invalid value for parameter"))
  }
  
  Workflow <- tbl(global_db_pool, "workflows") %>%
    select(-created_at, -updated_at, -params, -advanced_edit, -notes)
  
  if (!is.na(model_id)) {
    Workflow <- Workflow %>%
      filter(model_id == !!model_id)
  }
  
  if (!is.na(site_id)) {
    Workflow <- Workflow %>%
      filter(site_id == !!site_id)
  }
  
  qry_res <- Workflow %>% collect()

  if (nrow(qry_res) == 0 || as.numeric(offset) >= nrow(qry_res)) {
    res$status <- 404
    return(list(error="Workflows not found"))
  }
  else {
    has_next <- FALSE
    has_prev <- FALSE
    if (nrow(qry_res) > (as.numeric(offset) + as.numeric(limit))) {
      has_next <- TRUE
    }
    if (as.numeric(offset) != 0) {
      has_prev <- TRUE
    }
    
    qry_res <- qry_res[(as.numeric(offset) + 1):min((as.numeric(offset) + as.numeric(limit)), nrow(qry_res)), ]
    
    result <- list(workflows = qry_res)
    result$count <- nrow(qry_res)
    if(has_next){
      if(grepl("offset=", req$QUERY_STRING, fixed = TRUE)){
        result$next_page <- paste0(
          req$rook.url_scheme, "://",
          req$HTTP_HOST,
          "/api/workflows",
          req$PATH_INFO,
          substr(req$QUERY_STRING, 0, stringr::str_locate(req$QUERY_STRING, "offset=")[[2]]),
          (as.numeric(limit) + as.numeric(offset)),
          "&limit=", 
          limit
        )
      }
      else {
        result$next_page <- paste0(
          req$rook.url_scheme, "://",
          req$HTTP_HOST,
          "/api/workflows",
          req$PATH_INFO,
          substr(req$QUERY_STRING, 0, stringr::str_locate(req$QUERY_STRING, "limit=")[[2]] - 6),
          "offset=",
          (as.numeric(limit) + as.numeric(offset)),
          "&limit=", 
          limit
        )
      }
    }
    if(has_prev) {
      result$prev_page <- paste0(
        req$rook.url_scheme, "://",
        req$HTTP_HOST,
        "/api/workflows",
        req$PATH_INFO, 
        substr(req$QUERY_STRING, 0, stringr::str_locate(req$QUERY_STRING, "offset=")[[2]]),
        max(0, (as.numeric(offset) - as.numeric(limit))),
        "&limit=", 
        limit
      )
    }
    
    return(result)
  }
}

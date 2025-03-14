library(dplyr)

#' Check if the file exists
check_file_exists <- function(filepath) {
  if (!file.exists(filepath)) {
    return(list(status = "Error", message = "File not found"))
  }
  return(NULL)  # Indicates no error
}

#' Extract run_id from file path
extract_run_id <- function(filepath) {
  parent_dir <- normalizePath(dirname(filepath))
  run_id <- substr(parent_dir, stringi::stri_locate_last(parent_dir, regex = "/")[1] + 1, 
                   stringr::str_length(parent_dir))
  return(run_id)
}

#' Validate user access based on workflow ownership
validate_user_access <- function(run_id, userid) {
  Run <- tbl(global_db_pool, "runs") %>%
    filter(id == !!run_id)

  Run <- tbl(global_db_pool, "ensembles") %>%
    select(ensemble_id = id, workflow_id) %>%
    full_join(Run, by = "ensemble_id") %>%
    filter(id == !!run_id)

  user_id <- tbl(global_db_pool, "workflows") %>%
    select(workflow_id = id, user_id) %>%
    full_join(Run, by = "workflow_id") %>%
    filter(id == !!run_id) %>%
    pull(user_id)

  if (user_id != userid) {
    return(list(status = "Error", message = "Access forbidden"))
  }
  return(NULL)  # Indicates no error
}

#' Read file as binary
read_file_binary <- function(filepath) {
  bin <- readBin(filepath, 'raw', n = file.info(filepath)$size)
  return(list(file_contents = bin))
}

#' API Endpoint: Download a file associated with PEcAn
#'
#' @param filepath Absolute path to file on target machine
#' @param userid User ID associated with file (typically the same as the user
#'   running the corresponding workflow)
#' @return Raw binary file contents
#' @author Tezan Sehu
get.file <- function(filepath, userid) {
  # Check if file exists
  file_check <- check_file_exists(filepath)
  if (!is.null(file_check)) return(file_check)

  # Extract run_id from file path
  run_id <- extract_run_id(filepath)

  # Validate user access if authentication is required
  if (Sys.getenv("AUTH_REQ") == TRUE) {
    access_check <- validate_user_access(run_id, userid)
    if (!is.null(access_check)) return(access_check)
  }

  # Read and return binary file content
  return(read_file_binary(filepath))
}

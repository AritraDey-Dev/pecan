library(magrittr, include.only = "%>%")

#' Get machine ID from hostname
get_machine_id <- function(machine_name) {
  machine_id <- dplyr::tbl(global_db_pool, "machines") %>%
    dplyr::filter(hostname == !!machine_name) %>%
    dplyr::pull(id)

  if (length(machine_id) > 1) {
    stop("Found ", length(machine_id), " machines with name ", machine_name)
  }
  if (length(machine_id) < 1) {
    stop("Found no machines with name ", machine_name)
  }
  
  return(machine_id)
}

#' Get model-related files from database
get_model_files <- function(machine_id) {
  dplyr::tbl(global_db_pool, "dbfiles") %>%
    dplyr::filter(machine_id == !!machine_id, container_type == "Model")
}

#' Combine model files with models and model types
combine_model_data <- function(modelfiles) {
  models <- dplyr::tbl(global_db_pool, "models")
  modeltypes <- dplyr::tbl(global_db_pool, "modeltypes") %>%
    dplyr::select(modeltype_id = id, modeltype = name)

  modelfiles %>%
    dplyr::select(dbfile_id = id, file_name, file_path, model_id = container_id) %>%
    dplyr::inner_join(models, c("model_id" = "id")) %>%
    dplyr::inner_join(modeltypes, "modeltype_id") %>%
    dplyr::collect()
}

#' List models available on a specific machine
#'
#' @param machine_name Target machine hostname. Default = `"docker"`
#' @param machine_id Target machine ID. If `NA` (default), deduced from hostname.
#' @return `data.frame` of information on available models
#' @author Alexey Shiklomanov
#* @get /
availableModels <- function(machine_name = "docker", machine_id = NA) {
  if (is.na(machine_id)) {
    machine_id <- get_machine_id(machine_name)
  }

  modelfiles <- get_model_files(machine_id)
  combine_model_data(modelfiles)
}

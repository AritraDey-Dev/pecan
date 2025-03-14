#' Get the status of the workflow specified by the id
#' @param id Workflow id (character)
#' @return Status of requested workflow
#' @author Tezan Sahu
#* @get /<id>/status
getWorkflowStatus <- function(req, id, res){
  Workflow <- tbl(global_db_pool, "workflows") %>%
    select(id, user_id) %>%
    filter(id == !!id)

  qry_res <- Workflow %>% collect()

  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Workflow with specified ID was not found on this host"))
  }
  else {
    statusfile <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", qry_res$id, "/STATUS")
    if(! file.exists(statusfile)){
      res$status <- 404
      return(list(error="Workflow with specified ID was not found on this host"))
    }

    wf_status <- readLines(statusfile)
    wf_status <- stringr::str_replace_all(wf_status, "\t", "  ")
    return(list(workflow_id=id, status=wf_status))
  }
}

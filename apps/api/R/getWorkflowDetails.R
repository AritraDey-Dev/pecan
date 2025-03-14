#' Get the of the workflow specified by the id
#' @param id Workflow id (character)
#' @return Details of requested workflow
#' @author Tezan Sahu
#* @get /<id>
getWorkflowDetails <- function(id, req, res){
  Workflow <- tbl(global_db_pool, "workflows") %>%
    select(id, model_id, site_id, folder, hostname, user_id)
  
  Workflow <- tbl(global_db_pool, "attributes") %>%
    select(id = container_id, properties = value) %>%
    full_join(Workflow, by = "id") %>%
    filter(id == !!id)
  
  qry_res <- Workflow %>% collect()
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Workflow with specified ID was not found"))
  }
  else {
    if(is.na(qry_res$properties)){
      res <- list(
        id = id, 
        folder=qry_res$folder, 
        hostname=qry_res$hostname,
        user_id=qry_res$user_id,
        properties = list(modelid = qry_res$model_id, siteid = qry_res$site_id)
      )
    }
    else{
      res <- list(
        id = id, 
        folder=qry_res$folder, 
        hostname=qry_res$hostname,
        user_id=qry_res$user_id,
        properties = jsonlite::parse_json(qry_res$properties[[1]])
      )
    }
    
    filesdir <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", id)
    if(dir.exists(filesdir)){
      all_files <- list.files(filesdir)
      res$files <- all_files[!all_files %in% c("out", "rabbitmq.out", "pft", "run", "STATUS")]
    }
    
    return(res)
  }
}

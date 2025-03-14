#' Post a workflow for execution
#' @param req Request sent
#' @return ID & status of the submitted workflow
#' @author Tezan Sahu
#* @post /
submitWorkflow <- function(req, res){
  if(req$HTTP_CONTENT_TYPE == "application/xml") {
    submission_res <- submit.workflow.xml(req$postBody, req$user)
  }
  else if(req$HTTP_CONTENT_TYPE == "application/json") {
    submission_res <- submit.workflow.json(req$postBody, req$user)
  }
  else{
    res$status <- 415
    return(paste("Unsupported request content type:", req$HTTP_CONTENT_TYPE))
  }
  
  if(submission_res$status == "Error"){
    res$status <- 400
    return(submission_res)
  }
  res$status <- 201
  return(submission_res)
}

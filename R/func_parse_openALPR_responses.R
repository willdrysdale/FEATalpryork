#' Parse open APLR response
#' 
#' handles the response from the openAPLR cloudAPI response and returns useable results
#' 
#' @param responses list containing the output of openAPLR_request
#' @param image_files vector of image file names that was supplie to openAPLR_request
#' 
#' @author W. S. Drysdale
#' @export


parse_openALPR_responses = function(responses,image_files){
  #Data Extraction
  fail_frame = data.frame(plate = NA,confidence = NA,region_confidence = NA,vehicle_region = NA,region = NA,plate_index = NA,
                          processing_time_ms = NA,candidates = NA,coordinates = NA,matches_template = NA,requested_topn = NA)
  
  names(responses) = image_files
  for (i in 1:length(responses)){
    tmp_status = responses[[i]]$status_code
    tmp_content = responses[[i]]$content
    tmp_content = rawToChar(tmp_content)
    tmp_content = jsonlite::fromJSON(tmp_content)
    tmp_result = tmp_content$results
    
    if (length(tmp_result) != 11)
      tmp_result = fail_frame
    
    tmp_result = tmp_result[,1:2]
    tmp_result$status_code = tmp_status
    tmp_result$image = names(responses)[[i]]
    if(i == 1)
      result = tmp_result
    else
      result = rbind(result,tmp_result)
  }
  #Return
  result
}


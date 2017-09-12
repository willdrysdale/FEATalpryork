#' ALPR Remove duplicates
#' 
#' Where the ALRP software has returned multiple results, keeps only the highest confidence one
#' 
#' @param results output of run_openALPR()
#' 
#' @author W S Drysdale
#' @export


aplr_remove_duplicates = function(results){
  results_dup =  results[duplicated(results$image) | duplicated(results$image,fromLast = T),]
  image_dup = unique(results_dup$image)
  
  #When two plates have been returned for one image, keep the one with the higest confidence
  for (i in 1:length(image_dup)){
    tmp_dup = results_dup[results_dup$image == image_dup[i],]
    max_conf = max(tmp_dup$confidence)
    keep_plate = tmp_dup$plate[tmp_dup$confidence == max_conf]
    results = results[-(!(results$plate == keep_plate) & (results$image == image_dup[i])),]
  }
  #return
  results
}


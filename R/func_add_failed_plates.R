#' Add Failed plates
#' 
#' takes the results of the numberplate validation and allows teh addition of plates that were not detected by ALPR
#' 
#' @param results output of number_plate_match()
#' @param image_dir directory of image files
#' 
#' @author W S Drysdale
#' @export


add_failed_plates = function(results, image_dir){
  auto_complete = results[results$match == "y",]
  man_complete = results[results$match == "n",]
  
  verify_plate = function(plate){
    plate = toupper(plate)
    shift_chars = c("!",'"',"£","$","%","^","*","(",")")
    if(any(shift_chars %in% str_split(plate,"")[[1]])){
      plate = gsub("!","1",plate)
      plate = gsub('"',"2",plate)
      plate = gsub("£","3",plate)
      plate = gsub("\\$","4",plate)
      plate = gsub("%","5",plate)
      plate = gsub("\\^","6",plate)
      plate = gsub("&","7",plate)
      plate = gsub("\\*","8",plate)
      plate = gsub("\\(","9",plate)
      plate = gsub("\\)","0",plate)
    }
    plate
  }

  for (i in 1:nrow(man_complete)){
    image_path = paste(image_dir,man_complete$image[i],sep = "")
    display(readImage(image_path),"raster")
    j = readline(prompt = "Please type numberplate: ")
    j = verify_plate(j)
    man_complete$plate[i] = j
    man_complete$match[i] = "manual"
  }
  results = rbind(auto_complete,man_complete)
  #return
  results
}


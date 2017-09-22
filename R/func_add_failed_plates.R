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
  #convert factors to strings
  if(class(results$plate) == "factor")
    results$plate = as.character(results$plate)
  
  if(class(results$image) == "factor")
    results$image = as.character(results$image)
  
  if(class(results$match) == "factor")
    results$match = as.character(results$match)
  
  
  
  auto_complete = results[results$match == "y",]
  man_complete = results[results$match == "n",]
  
  verify_plate = function(plate){
    plate = toupper(plate)
    shift_chars = c("!",
                    '"',
                    "\u00A3",
                    "$",
                    "%",
                    "^",
                    "*",
                    "(",
                    ")"
                    )
    if(any(shift_chars %in% str_split(plate,"")[[1]])){
      plate = gsub("!","1",plate)
      plate = gsub('"',"2",plate)
      plate = gsub("\u00A3","3",plate)
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
  
  
  
  col_names = c(names(results))
                
  if(!dir.exists("temp_failed_plates")){
    dir.create("temp_failed_plates")
    results_out = data.frame(matrix(ncol = 6,nrow = 0))
    names(results_out) = col_names
    start_row = 1
  }else{
    results_out = read.csv("temp_failed_plates/temp_results_out.csv",stringsAsFactors = F)
    start_row = nrow(results_out)+1
  }
  
  for (i in start_row:nrow(man_complete)){
    temp = data.frame(matrix(ncol = 6,nrow = 0))
    names(temp) = col_names
    
    image_path = paste(image_dir,man_complete$image[i],sep = "")
    display(readImage(image_path),"raster")
    j = readline(prompt = "Please type numberplate: ")
    j = verify_plate(j)
    temp = man_complete[i,]
    temp$plate[1] = j
    temp$match[1] = "manual"
    results_out[i,] = temp
    write.csv(results_out,"temp_failed_plates/temp_results_out.csv",row.names = F)
  }
  results = rbind(auto_complete,results_out)
  
  if(!dir.exists("results"))
    dir.create("results")
    write.csv(results_out,"results/man_plate_results.csv",row.names = F)
    unlink("temp_failed_plates",recursive = T)
  #return
  results
}


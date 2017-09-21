#' Number Plate Match
#' 
#' Sorts through numberplate recognition results file, and asks user to confirm if the algorithm correctly analysed the plate
#' 
#' @param results output of of cloudaplr code
#' @param img_dir directory where image files are stored
#' 
#' @author W. S. Drysdale
#' @export 


number_plate_match = function(results,img_dir){
  #Function for user vertification q or y, e or n
  plate_match_prompt = function(plate){
    i = readline(prompt = paste("Is the numberplate in the image ",plate, " ( q or y / e or n ) : ",sep = ""))
    if (i == "q" | i == "e" | i == "y" | i == "n"){
      if (i == "q"| i == "y")
        return("y")
      else{
        if (i == "e" | i == "n"){
          return("n")
        }
      }
    }else
      plate_match_prompt(plate)
  }
  #convert factors to strings
  if(class(results$plate) == "factor")
    results$plate = as.character(results$plate)
  
  if(class(results$image) == "factor")
    results$image = as.character(results$image)
  
  
  col_names = c(names(results),"match")
  #create tempory dir or load temp_output
  if(!dir.exists("temp_plate_match")){
    dir.create("temp_plate_match")
    results_out = data.frame(matrix(ncol = 6,nrow = 0))
    
    names(results_out) = col_names
    start_row = 1
  }else{
    results_out = read.csv("temp_plate_match/temp_results_out.csv",stringsAsFactors = F)
    start_row = nrow(results_out)+1
  }

  #Loop over all images, calling verification function
  for (i in start_row:nrow(results)){
    temp = data.frame(matrix(ncol = 6,nrow = 0))
    names(temp) = col_names
    
    if(is.na(results$plate[i])){
      temp[1,1:5] = results[i,1:5]
      temp[1,6] = "n"
      results_out[i,] = temp
      write.csv(results_out,"temp_plate_match/temp_results_out.csv",row.names = F)
      next
    }
    
    image = paste(img_dir,results$image[i],sep = "")
    plate = results$plate[i]
    display(readImage(image),"raster")
    rect(ybottom = 241,ytop = 291,xleft = 0,xright = 541,col = "black")
    text(x = 256, y = 266,label = plate, col = "white", cex = 5,font = 2)
    
    temp[1,1:5] = results[i,1:5]
    temp[1,6] = plate_match_prompt(results$plate[i])
    
    results_out[i,] = temp
    
    write.csv(results_out,"temp_plate_match/temp_results_out.csv",row.names = F)
  }
  if(!dir.exists("results"))
    dir.create("results")
  write.csv(results_out,"results/plate_match_results.csv",row.names = F)
  unlink("temp_plate_match",recursive = T)
  results_out
}







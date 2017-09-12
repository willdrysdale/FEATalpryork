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

  #Loop over all images, calling verification function
  for (i in 1:nrow(results)){
    if(is.na(results$plate[i])){
      results$match[i] = "n"
      next
    }
    image = paste(img_dir,results$image[i],sep = "")
    plate = results$plate[i]
    display(readImage(image),"raster")
    rect(ybottom = 241,ytop = 291,xleft = 0,xright = 541,col = "black")
    text(x = 256, y = 266,label = plate, col = "white", cex = 5,font = 2)
    
    results$match[i] = plate_match_prompt(results$plate[i])
  }
  results
}
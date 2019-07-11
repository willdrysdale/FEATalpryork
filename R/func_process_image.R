#' Process Image
#' 
#' Wrapper function for processing images - allows for adjustment of processing options from \code{run_openALPR()}
#' 
#' @param image_file path to one image
#' @param output_folder where should these processed images be saved?
#' @param vcrop vertical crop range to "letterbox" image - default 30:220
#' @param brightness value to add or subract to adjust image brightness via EBImage
#' @param contrast value to multiply by to adjust image contrat via EBImage
#' 
#' @author W. S. Drysdale
#' 
#' @export


process_image = function(image_file,
                         output_folder,
                         vcrop = NULL,
                         brightness = 0,
                         contrast = 1){ 
  image = readImage(paste(image_file,sep = ""))
  
  if(!is.null(vcrop))
    image = image[,vcrop,]
  
  image = (image * contrast) + brightness
  
  img_name = str_split(image_file,"/") %>% 
    unlist()
  
  img_name = img_name[length(img_name)]
  
  
  if(!dir.exists(output_folder))
    dir.create(output_folder)
  
  writeImage(image,paste(output_folder,img_name,sep = ""))
}
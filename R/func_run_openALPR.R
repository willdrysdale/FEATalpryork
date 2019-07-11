#' Run openAPLR
#' 
#' Runs FEAT images though the openALPR system and outputs a respose ready to be parsed through verification steps
#' 
#' @param secret_key openALPR API secret key
#' @param country openALPR plate country code, default "eu"
#' @param image_dir directory where image files are stored
#' @param vcrop vertical crop range to "letterbox" image - default 30:220
#' @param brightness vector to add or subract to adjust image brightness via EBImage - default c(0,-0.2,0)
#' @param contrast vector to multiply by to adjust image contrat via EBImage - default c(1,4,2)
#' 
#' @author W S Drysdale
#' 
#' @export

run_openALPR = function(secret_key,
                        country = "eu",
                        image_dir,
                        vcrop = c(30:220),
                        brightness = c(0,-0.2,0),
                        contrast = c(1,4,2)){
  
  # validate settings
  if(length(brightness) != length(contrast))
    stop("Brightness and contrast must be of same length")
  
  if(length(contrast[contrast == 0]) != 0){
    contrast[contrast == 0] = 1
    warning("Contrast contained zeros, these have been replaced with ones")
  }
  
  # Confirm Working Directory
  print("working directory is:")
  print(getwd())
  
  workingdir_prompt = function(){
    i = readline(prompt = "is correct? sub folders will be created here [y/n]: ")
    if (i %in% c("y","n")){
      if (i == "y")
        print("running ALPR")
      if(i == "n")
        stop("please navigate to correct directory")
      
    }else
      workingdir_prompt()
  }
  
  workingdir_prompt()
  
  # Locate images
  image_files = list.files(image_dir)
  
  # Loop over n image processing steps
  results = list()
  for(i in 1:length(brightness)){
    
    output_folder = paste0("img_process_",i,"/")
    
    temp = purrr::map(.x = image_files,
                      .f = process_image,
                      output_folder = output_folder,
                      vcrop = vcrop,
                      brightness = brightness[i],
                      contrast = contrast[i]
    )
    
    # stop croping after first run 
    if(i == 1)
      vcrop = NULL
    
    # change image_files to newly processed files
    image_files = list.files(output_folder)
    
    # make call to openALPR
    run_responses =  purrr::map(.x = image_files,
                                .f = openALPR_request,
                                secret_key = secret_key,
                                country = country,
                                image = .)
    
    run_results = parse_openALPR_responses(run_responses,image_files)
    
    run_results$confidence[is.na(run_results$confidence)] = 0
    low_confidence = run_results[(run_results$confidence < 90 | nchar(run_results$plate) < 7),]
    low_conf_image = low_confidence$image
    
    if(i != length(brightness)){ # On all but the last pass, save only the high confidence results
      results[[i]] = run_results[(run_results$confidence >= 90 | nchar(run_results$plate) >= 7),]
    }else{ # on the last pass, save everything that is left
      results[[i]] = run_results
    }
    
    results[[i]]$pass = i # note what pass this data was generated on
    
  }
  
  # Return
  bind_rows(results)
}
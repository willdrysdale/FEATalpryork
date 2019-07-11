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
                        contrast = c(1,4,2),
                        confidence_threshold = 90){
  
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
  ori_image_files = list.files(image_dir,full.names = T)
  image_files = ori_image_files
  
  # Loop over n image processing steps
  for(i in 1:length(brightness)){
    
    output_folder = paste0("img_process_",i,"/")
    
    if(!dir.exists(output_folder))
      dir.create(output_folder)
    
    temp = purrr::map(.x = image_files,
                      .f = process_image,
                      output_folder = output_folder,
                      vcrop = vcrop,
                      brightness = brightness[i],
                      contrast = contrast[i]
    )
    
    # change image_files to newly processed files
    image_files = list.files(output_folder,full.names = T)
    
    # make call to openALPR
    run_responses =  purrr::map(.x = image_files,
                                .f = openALPR_request,
                                secret_key = secret_key,
                                country = country)
    
    run_results = parse_openALPR_responses(run_responses,image_files)
    
    strip_image_path = function(image_path){
      image_path = stringr::str_split(image_path,"/")
      n = length(image_path[[1]])
      
      map_chr(image_path,n)
    }
    
    run_results$image = strip_image_path(run_results$image)
    
    run_results$confidence[is.na(run_results$confidence)] = 0
    run_results$plate[is.na(run_results$plate)] = "-999"
    low_confidence = run_results[(run_results$confidence < confidence_threshold | nchar(run_results$plate) < 7),]
    low_conf_image = low_confidence$image
    
    run_results$pass = i
    
    if(i == 1 & i == length(brightness)){ # if the first pass is the only pass
      if(!dir.exists("results"))
        dir.create("results")
      write.csv(results,"results/open_ALPR_results.csv",row.names = F)
      return(run_results)
    }
    
    if(i == 1 & i != length(brightness)){ # if this is the first pass, but not the last
      results = run_results[(run_results$confidence >= confidence_threshold & nchar(run_results$plate) >= 7),]
    }
    
    if(i != 1 & i != length(brightness)){ # if this is not the first pass, or the last pass
      temp = run_results[(run_results$confidence >= confidence_threshold & nchar(run_results$plate) >= 7),]
      results = dplyr::bind_rows(results,temp)
    }
    
    if(i != 1 & i == length(brightness)){ # if this is the last pass
      results = dplyr::bind_rows(results,run_results)
    }
    
    if(length(low_conf_image) == 0){
      if(!dir.exists("results"))
        dir.create("results")
      write.csv(results,"results/open_ALPR_results.csv",row.names = F)
      return(results)
    }else{
      image_files = ori_image_files[strip_image_path(ori_image_files) %in% low_conf_image]
    }
    
  }
  
  if(!dir.exists("results"))
    dir.create("results")
  write.csv(results,"results/open_ALPR_results.csv",row.names = F)
  
  # Return
  results
}








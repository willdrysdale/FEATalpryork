#' Run openAPLR
#' 
#' Runs FEAT images though the openALPR system and outputs a respose ready to be parsed through verification steps
#' 
#' @param secret_key openALPR API secret key
#' @param country openALPR plate country code, default "eu"
#' @param image_dir directory where image files are stored
#' 
#' @author W S Drysdale
#' @export

run_openALPR = function(secret_key,country = "eu",image_dir){
  print("working directory is:")
  print(getwd())
  
  workingdir_prompt = function(){
    i = readline(prompt = "is correct? sub folders will be created here [y/n]: ")
    if (i == "y" | i == "n"){
      if (i == "y")
        print("running ALPR")
      if(i == "n")
        stop("please navigate to correct directory")
      
    }else
      workingdir_prompt()
  }
   
  workingdir_prompt()
  
  
  image_files = list.files(image_dir)
  #API Request Loop - parse 1
  responses1 = list()
  for (i in 1:length(image_files)){
    image = paste(image_dir,image_files[i],sep = "")
    temp_content =  openALPR_request(secret_key,country,image)
    responses1[[i]] = temp_content
  }
  rm(temp_content)
  
  result_pass1 = parse_openALPR_responses(responses1,image_files)
  
  if(!dir.exists("img_corr"))
    dir.create("img_corr")
  
  result_pass1$confidence[is.na(result_pass1$confidence)] = 0
  low_confidence = result_pass1[(result_pass1$confidence < 90 | nchar(result_pass1$plate) < 7),]
  low_conf_image = low_confidence$image
  
  #Reprocess low confidence images
  for (i in 1:length(low_conf_image)){
    image = readImage(paste(image_dir,low_conf_image[i],sep = ""))
    image = (image * 4) - 0.2
    writeImage(image,paste("img_corr/",low_conf_image[i],sep = ""))
  }
  
  #API Request Loop - parse 2
  responses2 = list()
  for (i in 1:length(low_conf_image)){
    image = paste("img_corr/",low_conf_image[i],sep = "")
    temp_content =  openALPR_request(secret_key,country,image)
    responses2[[i]] = temp_content
  }
  rm(temp_content)
  
  result_pass2 = parse_openALPR_responses(responses2,low_conf_image)
  
  if(!dir.exists("img_corr2"))
    dir.create("img_corr2")
  
  result_pass2$confidence[is.na(result_pass2$confidence)] = 0
  low_confidence2 = result_pass2[(result_pass2$confidence < 90 | nchar(result_pass2$plate) < 7),]
  low_conf_image2 = low_confidence2$image
  
  #Reprocess low confidence images2
  for (i in 1:length(low_conf_image2)){
    image = readImage(paste(image_dir,low_conf_image2[i],sep = ""))
    image = (image *2)
    writeImage(image,paste("img_corr2/",low_conf_image2[i],sep = ""))
  }
  #API Request Loop - parse 3
  responses3 = list()
  for (i in 1:length(low_conf_image2)){
    image = paste("img_corr2/",low_conf_image2[i],sep = "")
    temp_content =  openALPR_request(secret_key,country,image)
    responses3[[i]] = temp_content
  }
  rm(temp_content)
  
  result_pass3 = parse_openALPR_responses(responses3,low_conf_image2)
  
  pass3 = image_files[image_files %in% low_conf_image2]
  pass2 = image_files[(image_files %in% low_conf_image) & !(image_files %in% low_conf_image2)]
  pass23 = c(pass2,pass3)
  pass1 = image_files[!(image_files %in% pass23)]
  
  unq_rp3 = result_pass3[result_pass3$image %in% pass3,]
  unq_rp3$pass = 3
  unq_rp2 = result_pass2[result_pass2$image %in% pass2,]
  unq_rp2$pass = 2
  unq_rp1 = result_pass1[result_pass1$image %in% pass1,]
  unq_rp1$pass = 1
  results = rbind(unq_rp1,unq_rp2,unq_rp3)

  #return
  results
}









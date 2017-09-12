#' API Request function
#' 
#' Handles calls to the openALPR cloud API
#' 
#' @param secret_key secret key of users account
#' @param country country code of plates, default eu
#' @param image file path of image
#' @param prewarp prewarp coordiantes, defaults to no prewarp
#' 
#' @author W. S. Drysdale
#' @export

openALPR_request = function(secret_key,country = "eu",image){
  h <- new_handle()
  curl::handle_setform(h,
                       secret_key = secret_key,
                       country = country,
                       image = form_file(image, "image/jpeg")
                       )
  
  curl_url ="https://api.openalpr.com/v2/recognize"
  
  response = curl::curl_fetch_memory(curl_url,handle = h)
  #return
  response
}

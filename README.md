An R package used by the UoY FEAT team to perform ALPR using the cloudALPR API

To install: 

```
#Instal Package
library(devtools)

install_github("willdrysdale/FEATalpryork")

#ALPR WORKFLOW
library(yorkFEATAPLR)
secret_key = "INSTERKEYHERE"
image_directory = "raw_images/" #final slash is important!

#Interface wiht cloudAPI. This step requires and internet connection and will consume APU credits ~2x number of pictures
results = yorkFEATALPR::run_openALPR(secret_key = secret_key,country = "eu",image_dir = image_directory)

#validate image recognition
#This step must be completed in full - consider breaking down images into small batches
results_val = yorkFEATALPR::number_plate_match(results,image_directory)

#Manual Plates step - those that failed valiadation must be input manually
results_man_val = yorkFEATALPR::add_failed_plates(results_val,image_directory)

```


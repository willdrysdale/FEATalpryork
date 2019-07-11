An R package used by the UoY FEAT team to perform ALPR using the openALPR API

**To install:**

```
#Install Package
library(devtools)

install_github("willdrysdale/FEATalpryork")
```

**Workflow**
*Setup*

```
library(yorkFEATALPR)
secret_key = "INSTERKEYHERE"
image_directory = "raw_images/" #final slash is important!
```

**Run openALPR**
This step requires and internet connection and will consume API credits
Length of brightness/contrast arguments determine how many passes the images get.
Images that are returned with less than 90% confidence or <7 characters will be used in the next pass

```
results = yorkFEATALPR::run_openALPR(secret_key = secret_key,
                                     country = "eu",
                                     image_dir = image_directory,
                                     vcrop = 30:220,
                                     brightness = c(0,-0.2,0),
                                     contrast = c(1,4,2))
```

**validate image recognition**
This step must be completed in full - consider breaking down images into small batches

```
results_val = yorkFEATALPR::number_plate_match(results,image_directory)
```

**Manual Plates step**
Those that failed valiadation must be input manually

```
results_man_val = yorkFEATALPR::add_failed_plates(results_val,image_directory)
```


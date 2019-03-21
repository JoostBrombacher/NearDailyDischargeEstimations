###############################################
###############################################
#### ------------------------------------- ####
#### NEAR DAILY DISCHARGE ESTIMATIONS IN   ####
#### HIGH LATITUDES FROM SENTINEL-1 AND 2  ####
#### ------------------------------------- ####
#### AUTHOR: JOOST BROMBACHER              ####
#### DATE: 20-03-2019                      ####
#### INSTITUTE: WAGENINGEN UNIVERSITY      ####
#### ------------------------------------- ####
#### DISCLAIMER: The scripts, data, and    ####
#### the obtained results can be reviewed  ####
#### and adapted without any restrictions. ####
#### Publishing results derived from these ####
#### scripts must be accompanied with      ####
#### proper referencing.                   ####
#### ------------------------------------- ####
###############################################
###############################################

#######################
## SETUP AND PRESETS ##
#######################
## - Set default working directory to the desired workspace
default <- "D:/NearDailyDischargeEstimations"
setwd(default)

## - Load the Source R-Scripts 
source("R-studio/02-LoadData.R")
source("R-studio/03-MNDWImask.R")
source("R-studio/04-RandomForest.R")
source("R-studio/05-Classification.R")
source("R-studio/06-Validation.R")
source("R-studio/07-Iterations.R")
source("R-studio/08-EffectiveWidth.R")
source("R-studio/09-Bootstrap.R")
source("R-studio/10-DischargeEstimations.R")
source("R-studio/11-LongTimeSeries.R")
source("R-studio/12-PostProcessing.R")

## - Select all available cores of your machine to optimize workflow for multicore processes
cores <- 4
beginCluster(cores,type='SOCK')

##################################
## VISUALISE OBSERVED DISCHARGE ##
##################################
## - View observed discharge from the Icelandic Meteorological Office and Landsvirkjun
##   for a specific period of interest. 
## - The dates can range from 2015-01-01 till 2018-11-01.
start.plot <- "2018-01-01 00:00:00"
end.plot <- "2018-05-01 00:00:00"
obs.fun(start.plot,end.plot)

##########################
## CREATE (M)NDWI-MASKS ##
##########################
## - Create masks based on the NDWI and MNDWI considering Edge, Mean, and Peak Boundaries.
## - One can chose between the NDWI or MNDWI as index. 
MNDWI <- index.fun("MNDWI")
MNDWI.Edge <- mask.fun(MNDWI,"MNDWI","Edge")
MNDWI.Mean <- mask.fun(MNDWI,"MNDWI","Mean")
MNDWI.Peak <- mask.fun(MNDWI,"MNDWI","Peak")
masks <- c(MNDWI.Edge,MNDWI.Mean,MNDWI.Peak)

###################################
## TRAIN RANDOM FOREST ALGORITHM ##
###################################
## - Create a Random Forest model. Choose modelname, desired metrics and
##   which seasons should be included (Winter,Summer,Complete).
## - If model is already created; overwrite = FALSE.
modelname <- "TC"
season <- "Summer"
modelRF <- RandomForest.fun(modelname,season,FALSE)

###############################
## CLASSIFY VALIDATION IMAGE ##
###############################
## - Classify Sentinel-1 validation images for the desired validation period (start, end). 
## - If images already classified; overwrite = FALSE.
start <- "2017-08-20 00:00:00"
end <- "2017-08-21 00:00:00"
obs.fun(start,end)
val.class <- class.fun(start,end,modelRF,modelname,"ValidationTraining",season,FALSE)

##############################
## VALIDATE CLASSIFICATIONS ##
##############################
## - Validate the classification result of validation image (2017-08-20) by creating 
##   1500 validation points, and manually checking the land cover feature of each point. 
## - If validation points are already created and checked; overwrite = FALSE. 
valpoints.fun(val.class[[1]],FALSE)
val.check <- valcheck.fun(val.class[[1]],FALSE)
val <- val.fun(val.check,masks,season,modelname,FALSE)

####################################
## CLASSIFY ALL SENTINEL-1 IMAGES ##
####################################
## - Create and load classified Sentinel-1 images for desired period.
## - If images for desired period already classified; overwrite = FALSE. 
## - NOTE: Computation time per image can be lengthy (20 sec or more)
start <- "2017-01-01 00:00:00"
end <- "2018-11-01 00:00:00"
obs.fun(start,end)
class <- class.fun(start,end,modelRF,modelname,"Validation",season,FALSE)

##########################################
## ESTIMATE DISCHARGE VALIDATION PERIOD ##
##########################################
## - Calculate the Effective Width, Bootstrap the Rating Curve, and Estimate the 
##   Discharge for the desired study areas and MNDWI-masks for the validation period.
## - The codenames for the different areas are SA and (A1-A5). 
## - If all processes are already executed; overwrite = FALSE.
val.clean <- TRUE
val.start <- "2018-05-01 00:00:00"
val.end <- "2018-08-01 00:00:00"
val.areas <- c("SA","A1")
val.choices <- c("Edge","Mean","Peak")
val.mask <- "MNDWI"
period <- periodname.fun("Validation",val.start,val.end)
obs.fun(val.start,val.end)
iter.fun(val.start,val.end,val.areas,val.choices,val.mask,period,class,val.clean,"Validation",TRUE)

#########################################
## ESTIMATE DISCHARGE LONG TIME SERIES ##
#########################################
## - Calculate the Effective Width, and Estimate the Discharge 
##   for the desired study areas and MNDWI-masks.
## - The codenames for the different areas are SA and (A1-A5). 
## - If all processes are already executed; overwrite = FALSE.
ts.clean <- FALSE
ts.start <- "2017-01-01 00:00:00"
ts.end <- "2018-11-01 00:00:00"
ts.areas <- c("A1")
ts.choices <- c("Mean")
ts.mask <- "MNDWI"
ts.period <- periodname.fun("LongTimeSeries",ts.start,ts.end)
obs.fun(ts.start,ts.end)
iter.fun(ts.start,ts.end,ts.areas,ts.choices,ts.mask,period,class,ts.clean,"TimeSeries",TRUE)

####################
## POSTPROCESSING ##
####################
## - Calculate the Mean Decrease Accuracy and Mean Decrease Gini per metric for 
##   three Random Forest models, e.g. Complete, Summer, and Winter.
MDG.fun()

## - Calculate the Standard Deviation for the entire study area for 
##   the validation period and for a specific MNDWI mask.
stdev.fun(val.start,val.end,modelname,season,MNDWI.Mean,"Mean")

## - Always endCluster() when rerunning the script to prevent errors
## - When "cannot open the connection" error occurs, run endCluster()
endCluster()
setwd(default)  


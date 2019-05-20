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
default <- "C:/Users/Joost/Documents/WUR/Paper/"
setwd(default)

## - Set give name of the model and select the desired 
##   season (e.g. Complete, Summer, Winter).
modelname <- "New"
season <- "Complete"

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

##################################
## VISUALISE OBSERVED DISCHARGE ##
##################################
## - View observed discharge from the Icelandic Meteorological Office and Landsvirkjun
##   for a specific period of interest. Select TRUE for printing the selected dataframe. 
## - The dates can range from 2015-01-01 till 2018-11-01.
start.plot <- "2017-08-19 00:00:00"
end.plot <- "2017-08-20 00:00:00"
obs.fun(start.plot,end.plot,TRUE)

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
## - Create a Random Forest model for chosen modelname and season.
## - If model is already created; overwrite = FALSE.
modelRF <- RandomForest.fun(modelname,season,TRUE)

###############################
## CLASSIFY VALIDATION IMAGE ##
###############################
## - Classify Sentinel-1 validation images for the desired validation period (start, end). 
## - If images already classified; overwrite = FALSE.
start <- "2017-08-19 00:00:00"
end <- "2017-08-21 00:00:00"
obs.fun(start,end,FALSE)
val.class <- class.fun(start,end,modelRF,modelname,"ValidationTraining",season,TRUE)

##############################
## VALIDATE CLASSIFICATIONS ##
##############################
## - Validate the classification result of validation image (2017-08-20) by creating 
##   1500 validation points, and manually checking the land cover feature of each point. 
## - If validation points are already created and checked; overwrite = FALSE. 
valpoints.fun(val.class[[1]],FALSE)
val.check <- valcheck.fun(val.class[[1]],FALSE)
val <- val.fun(val.check,masks,season,modelname,TRUE)

####################################
## CLASSIFY ALL SENTINEL-1 IMAGES ##
####################################
## - Create and load classified Sentinel-1 images for desired period.
## - If images for desired period already classified; overwrite = FALSE. 
## - NOTE: Computation time per image can be lengthy (20 sec or more)
start <- "2017-04-01 00:00:00"
end <- "2018-10-01 00:00:00"
obs.fun(start,end,FALSE)
class <- class.fun(start,end,modelRF,modelname,"Validation",season,TRUE)

##########################################
## ESTIMATE DISCHARGE VALIDATION PERIOD ##
##########################################
## - Calculate the Effective Width, Bootstrap the Rating Curve, and Estimate the 
##   Discharge for the desired study areas and MNDWI-masks for the validation period.
## - The codenames for the different areas are SA and (A1-A5). 
## - If all processes are already executed; overwrite = FALSE.
val.clean <- TRUE
val.start <- "2017-04-01 00:00:00"
val.end <- "2017-10-01 00:00:00"
val.areas <- c("SA","A1","A2","A3","A4","A5")
val.choices <- "Mean"
val.mask <- "MNDWI"
period <- periodname.fun("Validation",val.start,val.end)
obs.fun(val.start,val.end,FALSE)
iter.fun(val.start,val.end,val.areas,val.choices,val.mask,period,class,val.clean,"Validation",TRUE)

#########################################
## ESTIMATE DISCHARGE LONG TIME SERIES ##
#########################################
## - Calculate the Effective Width, and Estimate the Discharge 
##   for the desired study areas and MNDWI-masks.
## - The codenames for the different areas are SA and (A1-A5). 
## - If all processes are already executed; overwrite = FALSE.
ts.clean <- FALSE
ts.start <- "2017-04-01 00:00:00"
ts.end <- "2018-10-01 00:00:00"
ts.areas <- "A1"
ts.choices <- "Mean"
ts.mask <- "MNDWI"
ts.period <- periodname.fun("LongTimeSeries",ts.start,ts.end)
obs.fun(ts.start,ts.end,FALSE)
iter.fun(ts.start,ts.end,ts.areas,ts.choices,ts.mask,period,class,ts.clean,"TimeSeries",TRUE)

####################
## POSTPROCESSING ##
####################
## - Calculate the Mean Decrease Accuracy and Mean Decrease Gini per metric for 
##   three Random Forest models, e.g. Complete, Summer, and Winter.
MDG.fun(modelname,season)

## - Calculate the Standard Deviation for the entire study area for 
##   the validation period and for a specific MNDWI mask.
stdev.fun(val.start,val.end,modelname,season,MNDWI.Mean,"Mean")


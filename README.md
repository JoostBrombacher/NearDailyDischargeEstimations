
||-----------------------------------------------------------------------------||
||  NEAR DAILY DISCHARGE ESTIMATIONS IN HIGH LATITUDES FROM SENTINEL-1 and 2:  ||
||                 A Case Study for the Icelandic Thjórsá River                ||
||-----------------------------------------------------------------------------||

- Author: Joost Brombacher
- Date: 20-03-2019
- Institute: Wageningen University
- Disclaimer: The scripts, data, and the obtained results can be reviewed and 
  adapted without any restrictions. Publishing results derived from these 
  scripts must be accompanied with proper referencing. 

||--------------------------------- SETUP -------------------------------------||

- The scripts, input files, and output files are entitled to their specific 
  folders. When downloading the scripts and the data, a new folder should be 
  created which includes the data and R-studio folders. The directory of this
  folder will be the "default" directory. 
  
- The data folder consists of a folder for input files and a folder of output 
  files. All the input files are included in the download. The output files 
  need to be created by running the scripts. All the subfolders in the input 
  folder should keep their names, otherwise the scripts are not able to find them.
  
- In the Sentinel-1 subfolder within the input folder the "Images", "Training",
  and "ValidationTraining" subfolders have their own subfolder including 
  Sentinel-1 images. This subfolder should have the same name as the "modelname"
  variable in the 01-RunScript.R file. If one desires to test multiple 
  pre-processing methods for the Sentinel-1 images, a new subfolder should be
  created within all the Sentinel-1 subfolders which has the same name as the 
  new "modelname" variable.
  
- The subfolders of the output folder are empty and do not need to be altered.
  After every run of the scripts the output files are allocated to their 
  corresponding output subfolders.

||------------------------------- RUN SCRIPTS ---------------------------------||

- To run the model the 01-RunScript.R file is the only file that needs to be 
  opened. The other R-scripts consist of functions which are necessary for the 
  01-RunScript.R file to work. It is not advised to alter the code within these
  scripts. 
  
- The first step is to set the default working directory to the folder which 
  contains the Data and R-studio folders. Depending on the amount of available
  cores in your machine, the "cores" variable can be altered to increase the 
  efficiency for multicore operations. It is advised to use all
  available cores minus 1 to prevent malfunctions.
  
- For most of the functions within the 01-RunScript.R file there is an overwrite
  option available. This option is included to ensure that the script can be 
  run multiple times, without including lengthy processes like the classification
  of all the Sentinel-1 images. This enables to create many different results 
  for different periods without having to classify the images again. The default 
  setting is "FALSE", but when not output data can be found the results are
  still computed. Setting overwrite to "TRUE" will always overwrite existing 
  output results.
  
- The output for the "valpoints.fun" and "val.check" are already included. However,
  if one wants to revalidate the validation points the overwrite variables can be
  set to "TRUE". Keep in mind that this is a lengthy process and that the output
  will be allocated to the "Unchecked" subfolder. If one wants to work with these
  new results the files should be manually be transferred to the "Checked" subfolder
  and the names of the output files should be changed to "ValidationPointsChecked"
  and "ReferencePointsChecked". This is to prevent unwanted overwriting of previous
  results.
  
- All the important figures and files can be found in the output folder. However,
  one always able to compute additional output files or view the results within
  R itself. 

  
||---------------------------------- CONTACT ----------------------------------||

- For issues with the script or possible bugs, one can contact the author of the
  scripts at joost.brombacher@wur.nl or try to contact one of the other authors 
  of the paper. 

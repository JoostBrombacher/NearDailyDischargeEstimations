iter.fun = function(start,end,areas,choices,masks,period,images,init,purpose,overwrite){
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  
  if(overwrite == TRUE){
    for(q in 1:length(areas)){
      # Estimate discharge for clean Sentinel-1 images
      if(init == TRUE){
        # Load Polygons and Projection Settings
        studyarea <- studyarea.selection(areas[q])
        riverlength <- riverlength(areas[q])
        
        print(paste("07-Iterations: Clean",areas[q],sep=" "))
        name <- "Clean"
        if(purpose == "Validation"){
          # Calculate the effective width
          We <- We.fun(start,end,MNDWI,images,studyarea,riverlength,period,areas[q],name,TRUE)
          
          # Bootstrap the rating curves
          boot <- bootstrap.fun(areas[q],name,period)
          
          # Estimate the discharge for the validation period
          results <- Qestval.fun(areas[q],name,period)
        }
        else{
          # Calculate the effective width
          We <- We.fun(start,end,MNDWI,images,studyarea,riverlength,ts.period,areas[q],name,TRUE)
          
          # Estimate the discharge
          results <- Qestlong.fun(areas[q],name,period,ts.period)
        }
      }
      
      # Estimate discharge for masked Sentinel-1 images
      if(length(masks) > 0){
        for(j in 1:length(choices)){
          for(k in 1:length(masks)){
            if(choices[j] == "Edge" & masks[k] == "NDWI"){
              mask <- NDWI.Edge
            }
            if(choices[j] == "Edge" & masks[k] == "MNDWI"){
              mask <- MNDWI.Edge
            }
            if(choices[j] == "Mean" & masks[k] == "NDWI"){
              mask <- NDWI.Mean
            }
            if(choices[j] == "Mean" & masks[k] == "MNDWI"){
              mask <- MNDWI.Mean
            }
            if(choices[j] == "Peak" & masks[k] == "NDWI"){
              mask <- NDWI.Peak
            }
            if(choices[j] == "Peak" & masks[k] == "MNDWI"){
              mask <- MNDWI.Peak
            }
            
            # Load Polygons and Projection Settings
            studyarea <- studyarea.selection(areas[q])
            riverlength <- riverlength(areas[q])
            
            # Name the specific mask
            name <- paste(masks[k],"mask-",choices[j],sep="")
            
            print(paste("07-Iterations:",name,areas[q],sep=" "))
            if(purpose == "Validation"){
              # Calculate the effective width
              We <- We.fun(start,end,mask,images,studyarea,riverlength,period,areas[q],name,FALSE)
              
              # Bootstrap the rating curves
              boot <- bootstrap.fun(areas[q],name,period)
              
              # Estimate the discharge for the validation period
              results <- Qestval.fun(areas[q],name,period)
            }
            else{
              # Calculate the effective width
              We <- We.fun(start,end,mask,images,studyarea,riverlength,ts.period,areas[q],name,FALSE)
              
              # Estimate the discharge
              results <- Qestlong.fun(areas[q],name,period,ts.period)
            }
          }
        }
      }
    }
  }
}

periodname.fun <- function(name,start,end){
  period <- paste(name,"(",as.POSIXct(start),"-",as.POSIXct(end),")",sep="")
  return(period)
}

class.fun <- function(start,end,model,modelname,period,season,overwrite){
  
  # Load the different metrics used for the Random Forest classifier
  setwd(default)
  start.run <- Sys.time()
  bands <- c("VV","VH","VVVH1","VVVH2","VVVH3","VVmean","VHmean","sdVV","sdVH")

  layers = c()
  for(i in 1:length(bands)){
    if(bands[i] == "VV"){
      layers <- c(layers,1)
    }
    if(bands[i] == "VH"){
      layers <- c(layers,2)
    }
    if(bands[i] == "VVVH1"){
      layers <- c(layers,3)
    }
    if(bands[i] == "VVVH2"){
      layers <- c(layers,4)
    }
    if(bands[i] == "VVVH3"){
      layers <- c(layers,5)
    }
    if(bands[i] == "VVmean"){
      layers <- c(layers,6)
    }
    if(bands[i] == "VHmean"){
      layers <- c(layers,7)
    }
    if(bands[i] == "sdVV"){
      layers <- c(layers,8)
    }
    if(bands[i] == "sdVH"){
      layers <- c(layers,9)
    }
  }
  
  # Check if results are present
  setwd(paste(default,"/Data/Output/05-Classification/",modelname,"/",season,"/",period,sep=""))
  checkfiles <- length(list.files(pattern="*.grd", full.names=T))
  
  # Load the Sentinel-1 images
  if(period == "ValidationTraining"){
    setwd(paste(default,"/Data/Input/Sentinel-1/",period,"/",modelname,sep=""))
    files <- list.files(pattern="*.tif", full.names=T)

    datelist = c()
    for(i in 1:length(files)){
      date <- substr(files[i],10,18)
      Y <- substr(date,1,4)
      M <- substr(date,5,6)
      D <- substr(date,7,8)
      date <- paste(Y,'-',M,'-',D,sep="")
      datelist <- c(datelist,date)
    }
  }
  
  else{
    setwd(paste(default,"/Data/Input/Sentinel-1/Images/",modelname,sep=""))
    files <- list.files(pattern="*.tif", full.names=T)
    
    datelist = c()
    for(i in 1:length(files)){
      date = substr(files[i],10,23)
      Y = substr(date,1,4)
      M = substr(date,5,6)
      D = substr(date,7,8)
      h = substr(date,9,10)
      m = substr(date,11,12)
      s = substr(date,13,14)
      date = paste(Y,'-',M,'-',D,' ',h,':',m,':',s,sep="")
      datelist = c(datelist,date)
    }
  }
  
  # Subset the image collection
  files.subset = c()
  for(i in 1:length(files)){
    if(as.POSIXct(datelist[i]) >= start && as.POSIXct(datelist[i]) <= end){
      files.subset <- c(files.subset,files[i])
    }
  }
  
  # Classify the Sentinel-1 images
  if(overwrite == TRUE || checkfiles == 0){
    classlist <- c()
    for(i in 1:length(files.subset)){
      start.time <- Sys.time()
      print(paste("05-Classification: Load and Classify Image",i,"of",length(files.subset),sep=" "))
      
      image <- brick(files.subset[i])
      image <- subset(image,layers)
      names(image) <- bands
      class <- predict(image, model=model, na.rm=TRUE)
      names(class) <- "class"
      class <- projectRaster(class,crs=crs,res=res)
      class[class < 1.5] <- 1
      class[class > 1] <- 2
      class[class == 0] <- NA
      
      if(period == "ValidationTraining"){
        image.name <- substr(files.subset[i],3,17)
      }
      else{
        image.name <- substr(files.subset[i],3,23)
      }
      
      plot(class,main=image.name)
      
      # Create Working Directory
      mainDir <- paste(default,"/Data/Output/05-Classification",sep="")
      path <- file.path(mainDir,modelname)
      if(dir.exists(path) == FALSE){
        dir.create(path=path)
      }
      path <- file.path(path,season)
      if(dir.exists(path) == FALSE){
        dir.create(path=path)
      }
      path <- file.path(path,period)
      if(dir.exists(path) == FALSE){
        dir.create(path=path)
      }
      
      # Output Results
      if(period == "ValidationTraining"){
        writeRaster(class,filename=paste(default,"/Data/Output/05-Classification/",modelname,"/",season,"/",period,"/",image.name,".grd",sep=""), format="raster", overwrite=TRUE)
        classlist <- class
      }
      
      else{
        writeRaster(class,filename=paste(default,"/Data/Output/05-Classification/",modelname,"/",season,"/",period,"/",image.name,".grd",sep=""), format="raster", overwrite=TRUE)
        classlist <- c(classlist,class)
      }
      
      end.time <- Sys.time()
      time.taken <- difftime(end.time, start.time, units = c("secs"))
      print(paste("05-Classification: Computation Time =",round(time.taken,digit=1),"seconds",sep=" "))
    }
    end.run <- Sys.time()
    time.run <- difftime(end.run, start.run, units = c("mins"))
    print(paste("05-Classification: Total Computation Time =",round(time.run,1),"minutes",sep=" "))
  }
  
  else{
    setwd(paste(default,"/Data/Output/05-Classification/",modelname,"/",season,"/",period,sep=""))
    files <- list.files(pattern="*.grd", full.names=T)
    
    # Create datelist
    if(period == "ValidationTraining"){
      datelist = c()
      for(i in 1:length(files)){
        date <- substr(files[i],10,18)
        Y <- substr(date,1,4)
        M <- substr(date,5,6)
        D <- substr(date,7,8)
        date <- paste(Y,'-',M,'-',D,sep="")
        datelist <- c(datelist,date)
      }
    }
    else{
      datelist = c()
      for(i in 1:length(files)){
        date = substr(files[i],10,23)
        Y = substr(date,1,4)
        M = substr(date,5,6)
        D = substr(date,7,8)
        h = substr(date,9,10)
        m = substr(date,11,12)
        s = substr(date,13,14)
        date = paste(Y,'-',M,'-',D,' ',h,':',m,':',s,sep="")
        datelist = c(datelist,date)
      }
    }
    
    # Subset files
    files.subset = c()
    for(i in 1:length(files)){
      if(as.POSIXct(datelist[i]) >= start && as.POSIXct(datelist[i]) <= end){
        files.subset <- c(files.subset,files[i])
      }
    }
    
    classlist <- c()
    print(paste("05-Classification: Load",length(files.subset),"Classified Images",sep=" "))
    for(i in 1:length(files.subset)){
      class <- raster(files.subset[i])
      names(class) <- "class"
      classlist <- c(classlist,class)
    }
  }
  
  setwd(default)
  return(classlist)
}
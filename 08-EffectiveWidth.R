We.fun <- function(start,end,maskraster,images,shape,l.river,period,choice,name,clean){
  
  # Load the desired classified Sentinel-1 images
  setwd(paste(default,"/Data/Output/05-Classification/",modelname,"/",season,"/Validation",sep=""))
  files <- list.files(pattern="*.grd", full.names=T)
  
  # Create a datelist
  datelist <- c()
  for(i in 1:length(files)){
    date <- substr(files[i],10,23)
    Y <- substr(date,1,4)
    M <- substr(date,5,6)
    D <- substr(date,7,8)
    h <- substr(date,9,10)
    m <- substr(date,11,12)
    s <- substr(date,13,14)
    date <- paste(Y,'-',M,'-',D,' ',h,':',m,':',s,sep="")
    datelist <- c(datelist,date)
  }

  # Subset the image collection to the desired dates
  files.subset <- c()
  for(i in 1:length(files)){
    if(as.POSIXct(datelist[i]) >= start && as.POSIXct(datelist[i]) <= end){
      files.subset <- c(files.subset,files[i])
    }
  }
  
  # Load the observed discharge data
  discharge <- read.table(paste(default,"/Data/Input/Discharge/VHM30-2015-2018.txt",sep=""),sep=";",header=TRUE,row.names = NULL)
  
  # Load the MNDWI-mask
  maskraster <- projectRaster(maskraster,crs=crs,res=res)
  maskraster <- crop(maskraster,extent(shape))
  maskraster <- mask(maskraster,shape)
  values(maskraster)[values(maskraster) >= 2.5] <- NA
  maskraster <- na.omit(maskraster)
  values(maskraster)[values(maskraster) <= 1.5] <- 1
  values(maskraster)[values(maskraster) > 1.5 & values(maskraster) < 2.5] <- 2
  
  # Load the shape of the area
  area <- area(shape)
  
  # Calculate the Effective Width of each classified Sentinel-1 image
  statList <- list()
  for(i in 1:length(files.subset)){
    print(paste("08-EffectiveWidth: Calculate Surface Area Image ",i," of ",length(files.subset)," (",name," ",choice,")",sep=""))
    
    # Load the classified image
    temp <- raster(files.subset[i])
    temp <- projectRaster(temp,crs=crs,res=res)
    temp <- resample(temp,maskraster)
    
    # Add the MNDWI-mask
    if(clean == FALSE){
      temp <- cover(maskraster, temp)
    }
    
    # Crop the image to the given sub-area
    temp <- crop(temp, extent(shape))
    if(choice != "SA"){
      temp <- mask(temp, shape)
    }
    
    # Reproject the raster according to the crs and res of the reference image
    temp <- projectRaster(temp,crs=crs,res=res)
    
    # Assign specific values to water (1) or land (2)
    values(temp)[values(temp) < 1.5] = 1
    values(temp)[values(temp) > 1] = 2
    
    # Extract the date of the classified image
    time <- substring(files.subset[i],10,nchar(files.subset[i])-4)
    Y <- substr(time,1,4)
    M <- substr(time,5,6)
    D <- substr(time,7,8)
    h <- substr(time,9,10)
    m <- substr(time,11,12)
    s <- substr(time,13,14)
    date <- paste(Y,'-',M,'-',D,' ',h,':00:00',sep="")
    date <- as.POSIXlt(date)+60*60
    
    # Plot the classified images with the implemented MNDWI-mask
    plot(temp,main=date)
    
    # Calculate the water and land surface area and convert to the effective width
    w.area <- raster::cellStats((match(temp,1)), 'sum')
    l.area <- raster::cellStats((match(temp,2)), 'sum')
    pixels <- w.area+l.area
    pixel.area <- area/pixels
    We <- (w.area*pixel.area)/l.river
    
    # Load the observed discharge 
    Q <- discharge$Q[as.POSIXlt(discharge$date) == date]
    
    # Add observed discharge and effective width to a data.frame
    statList[[i]] <- data.frame(date=date, We=We, Q=Q) # create a data.frame to save statistics
  }
  
  # Create new output directories
  setwd(default)
  mainDir <- paste(default,"/Data/Output/08-EffectiveWidth",sep="")
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
  setwd(path)

  # Create the data.frame of all the discharge observations and estimations and name the output file
  data <- do.call(rbind.data.frame,statList) # final data.frame with all statistics
  dataname <- paste('01_Data_We_',name,'_',choice,'.txt',sep='')
  
  write.table(data,dataname,row.names=FALSE,sep=';')
}

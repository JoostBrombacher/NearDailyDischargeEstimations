RandomForest.fun = function(modelname,season,overwrite){
  
  setwd(paste(default,"/Data/Output/04-RandomForest",sep=""))
  loadmodel <- paste("01-RandomForest_",modelname,"-",season,".rds",sep="")
  if(!file.exists(loadmodel) == TRUE || overwrite == TRUE){
    if(!file.exists(loadmodel) == TRUE){
      print("04-RandomForest: Output not found, create new output...")
    }
    
    # Load the different metrics used for the Random Forest classifier
    setwd(default)
    bands <- c("VV","VH","VVVH1","VVVH2","VVVH3","VVmean","VHmean","sdVV","sdVH")
    
    layers = c()
    for(i in 1:length(bands)){
      if(bands[i] == "VV"){
        layers = c(layers,1)
      }
      if(bands[i] == "VH"){
        layers = c(layers,2)
      }
      if(bands[i] == "VVVH1"){
        layers = c(layers,3)
      }
      if(bands[i] == "VVVH2"){
        layers = c(layers,4)
      }
      if(bands[i] == "VVVH3"){
        layers = c(layers,5)
      }
      if(bands[i] == "VVmean"){
        layers = c(layers,6)
      }
      if(bands[i] == "VHmean"){
        layers = c(layers,7)
      }
      if(bands[i] == "sdVV"){
        layers = c(layers,8)
      }
      if(bands[i] == "sdVH"){
        layers = c(layers,9)
      }
    }
    
    # Set-up the training data for the three Random Forest models
    print("04-RandomForest: Extract Training Points")
    if(season == "Winter"){
      dates <- c("20160210","20180308")
    }
    if(season == "Summer"){
      dates <- c("20160927","20170726","20170830","20171101")
    }
    if(season == "Complete"){
      dates <- c("20160210","20160927","20170726","20170830","20171101","20180308")
    }
    
    # Load the training points and merge these points with the training images to create a Random Forest classifier
    valuetables <- c()
    for(i in 1:length(dates)){
      print(paste("04-RandomForest: Training Points Sentinel-1 ",dates[i]," (",i," of ",length(dates),")",sep=""))
      t.w <- readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/Points/Points-",dates[i],"-Water.shp",sep=""),verbose=FALSE)
      t.w@data$id <- as.factor("Water")
      t.l <- readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/Points/Points-",dates[i],"-Land.shp",sep=""),verbose=FALSE)
      t.l@data$id <- as.factor("Land")
      t <- rbind(t.w,t.l)
      names(t) <- "class"
      t$code <- t$class
      t@data$code <- as.integer(t@data$code)

      S1 <- brick(paste("Data/Input/Sentinel-1/Training/",modelname,"/Pjorsa-",dates[i],"-Sen1.tif",sep=""))
      S1 <- subset(S1,layers) 
      names(S1) <- bands
      S1 <- projectRaster(S1,crs=crs(t))
      
      classes <- rasterize(t, S1, field='code')
      S1masked <- mask(S1, classes)
      names(classes) <- "class"
      
      trainingbrick <- addLayer(S1masked, classes)
      
      valuetable <- na.omit(getValues(trainingbrick))
      valuetable <- as.data.frame(valuetable)
      valuetable$class <- factor(valuetable$class, levels = c(1:2))
      valuetables <- rbind(valuetables,valuetable)
    }
    
    print("04-RandomForest: Random Forest Model")
    
    x <- valuetables[,c(1:length(bands))]
    y <- valuetables$class
    
    modelRF <- tuneRF(x, y, stepFactor=1.5,improve=0.000001,mtryStart=3,doBest=TRUE,importance=TRUE,ntree=500)
    
    setwd(paste(default,"/Data/Output/04-RandomForest",sep=""))
    saveRDS(modelRF, paste("01-RandomForest_",modelname,"-",season,".rds",sep=""))
  }
  
  else{
    setwd(paste(default,"/Data/Output/04-RandomForest",sep=""))
    modelRF <- readRDS(paste("01-RandomForest_",modelname,"-",season,".rds",sep=""))
  }
  
  setwd(paste(default,"/Data/Output/04-RandomForest",sep=""))
  plotname <- paste("02_Importance_RandomForest_",modelname,"-",season,".png",sep="")
  png(filename=plotname,width=9,height=5,units='in',res=300)
  varImpPlot(modelRF,main=paste(modelname,"-",season,sep=""))
  dev.off()
  
  setwd(default)
  return(modelRF)
}

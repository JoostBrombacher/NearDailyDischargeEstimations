valpoints.fun <- function(image,overwrite){
  
  # Create randomly selected validation points
  if(overwrite == TRUE){ 
    print("06-Validation: Create Validation Points")
    
    # Calculate the area of water and land
    Areawater <- raster::cellStats((match(image, 1)), 'sum')
    Arealand <- raster::cellStats((match(image, 2)), 'sum')
    Totalarea <- Areawater+Arealand
    
    # Devide 1500 over the fractions of water and land
    n <- 1500
    fwater <- Areawater/Totalarea
    fland <- Arealand/Totalarea
    nwater <- round(n*fwater, digits = 0)
    nland <- round(n*fland, digits = 0)
    
    # Add classes to classified image with water == 1 and land == 0
    w <- image
    l <- image
    
    w[w == 2] <- NA
    w[w == 0] <- NA
    l[l == 1] <- NA
    l[l == 0] <- NA
    
    # convert to SpatialGridDataFrame 
    w.spgrd <- as(w,"SpatialGridDataFrame") 
    w.spgrd$constant <- ifelse(is.na(w.spgrd[[1]]),NA,1) 
    l.spgrd <- as(l,"SpatialGridDataFrame") 
    l.spgrd$constant <- ifelse(is.na(l.spgrd[[1]]),NA,1) 
    
    # convert to im 
    w.im <- as.im(w.spgrd["constant"]) 
    l.im <- as.im(l.spgrd["constant"]) 
    
    # sample points according to constant 
    w.points <- rpoint(nwater,w.im) 
    l.points <- rpoint(nland,l.im) 
    
    # Add water and land points to data.frame
    dw <- data.frame(w.points)
    dl <- data.frame(l.points)
    
    dw$class <- rep('w',nwater)
    dl$class <- rep('l',nland)
    
    d <- rbind(dw,dl)
    names(d)[1] <- paste('Lon')
    names(d)[2] <- paste('Lat')
    
    # Write validation points to table
    write.table(d, paste(default,"/Data/Output/01-Validation/Points/Unchecked/ValidationPoints.txt", sep=""),row.names=FALSE)
    # ADJUST NAME OF CREATED FILE TO ValidationPointsChecked.txt TO PREVENT OVERWRITING
    
    print("06-Validation: COPY ValidationPoints.txt TO 'Checked' FOLDER AS ValidationPointsChecked.txt")
  }
}

valcheck.fun <- function(image,overwrite){
  
  # Manually check the validation points
  if(overwrite == TRUE){
    print("06-Validation: Correct Validation Points")
    
    # Load the reference images
    sen2 <- brick(paste(default,"/Data/Input/Sentinel-2/Validation/Pjorsa-20170820-Sen2.tif",sep=""))
    NDWI <- (Sen2[[3]]-sen2[[5]])/(sen2[[3]]+sen2[[5]])
    planet <- brick(paste(default,"/Data/Input/Planet/Validation/20170820_114628_Planet.tif",sep=""))
    NDWIplanet <- (planet[[2]]-planet[[4]])/(planet[[2]]+planet[[4]])
    
    # Load table from the checked validationpoints
    d <- data.frame(read.table(paste(default,"/Data/Output/01-Validation/Points/Checked/ValidationPointsChecked.txt",sep=""),header=T))
    
    # Create list of 1500 NA's
    n <- 1500
    ref <- rep(NA,n)
  
    # For every step the correct land cover should be noted as w or l for water and land respectively
    for(i in 1:n){
      par(mai=c(0.8,0.8,0.5,0.3))
      layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  
      new.extent <- c(d$Lon[i]-0.014,d$Lon[i]+0.014,d$Lat[i]-0.006,d$Lat[i]+0.006)
  
      plot(image,xlim=c(new.extent[1],new.extent[2]),ylim=c(new.extent[3],new.extent[4]),main="Classification")
      points(d$Lon[i],d$Lat[i],pch='x')
  
      plot(NDWI,xlim=c(new.extent[1],new.extent[2]),ylim=c(new.extent[3],new.extent[4]),main="NDWI Sentinel-2")
      points(d$Lon[i],d$Lat[i],pch='x')
  
      new.extent <- c(d$Lon[i]-0.0023,d$Lon[i]+0.0023,d$Lat[i]-0.001,d$Lat[i]+0.001)
      new.planet <- crop(planet,extent(new.extent))
      
      plotRGB(new.planet,r=3,g=2,b=1,stretch="lin",scale=3000,axes=TRUE,main="Planet (R,G,B)")
      points(d$Lon[i],d$Lat[i],pch='x')
  
      plotRGB(new.planet,r=4,g=2,b=1,stretch="lin",scale=1000,axes=TRUE,main="Planet (NIR,G,B)")
      points(d$Lon[i],d$Lat[i],pch='x')
  
      writeClipboard(as.character(paste(d$Lat[i],',',d$Lon[i],sep="")))
      refchoice <- readline(prompt=paste('i: ',i,', Coordinates: ',d$Lat[i],',',d$Lon[i],', Truth: ',sep=""))
      
      ref[i] = refchoice
    }
    
    # Backup the data
    d_refbackup = ref
    d_ref = data.frame(ref)
    d$class = d_ref$ref
    names(d)[3] = paste('ref')
    
    # Write Data.frame to table
    write.table(d, paste(default,"/Data/Output/06-Validation/Points/Unchecked/ReferencePoints.txt",sep=""),row.names=FALSE)
    # ADJUST NAME OF CREATED FILE TO ReferencePointsChecked.txt TO PREVENT OVERWRITING
    print("06-Validation: COPY ReferencePoints.txt TO 'Checked' FOLDER AS ReferencePointsChecked.txt")
    return(d)
  }
  else{
    d <- data.frame(read.table(paste(default,"/Data/Output/06-Validation/Points/Checked/ReferencePointsChecked.txt",sep=""),header=T))
    return(d)
  }
}

val.fun <- function(d,masks,season,modelname,overwrite){
  
  if(overwrite==TRUE){
    # Validate the validation image and calculate the Overall Accuracy
    print("06-Validation: Validate Results")
    
    # Load the classified validation image
    class <- raster(paste(default,"/Data/Output/05-Classification/",modelname,"/",season,"/ValidationTraining/Pjorsa-20170819075728.grd",sep=""))
    class <- projectRaster(class,crs=crs,res=res)
    class <- resample(class,r.ref)
    
    accuracy <- function(c){
      c <- projectRaster(c,crs=crsWGS)
      points <- data.frame(d$Lon,d$Lat)
      c <- extract(c,SpatialPoints(points))
      
      c[c < 1.5] <- 1 
      c[c > 1] <- 2
      c[c == 0] <- NA
      c[c == 2] <- "l"
      c[c == 1] <- "w"
      
      d <- cbind(points,d$ref,data.frame(as.factor(c)))
      names(d)[1] <- paste('Lon')
      names(d)[2] <- paste('Lat')
      names(d)[3] <- paste('ref')
      names(d)[4] <- paste('class')
      
      cmatrix <- confusionMatrix(d$class, d$ref, positive=levels(d$class))
      
      # Overall accuracy
      overall <- cmatrix$overall[1]
      
      # Users and Producers accuracy
      cmatrix <- as.table(cmatrix)
      users <- cmatrix[2,2] /sum(cmatrix[2,])
      producers <- cmatrix[2,2] /sum(cmatrix[,2])
      
      accuracy <- c(overall,users,producers)
      return(accuracy)
    }
    
    setwd(default)
    mainDir <- paste(default,"/Data/Output/06-Validation/Validations",sep="")
    path <- file.path(mainDir,modelname)
    if(dir.exists(path) == FALSE){
      dir.create(path=path)
    }
    path <- file.path(path,season)
    if(dir.exists(path) == FALSE){
      dir.create(path=path)
    }
    setwd(path)
    
    # Export the validation results
    setwd(paste(default,"/Data/Output/06-Validation/Validations/",modelname,"/",season,sep=""))
    name <- paste("01-ValidationResult_Clean.tif",sep="")
    writeRaster(class,filename=name,format="GTiff",overwrite=TRUE)
    
    print("06-Validation: Validate Image 1 of 4")
    
    class <- resample(class,r.ref)
    class <- projectRaster(class,crs=crs,res=res)
    values(class)[values(class) < 1.5] = 1
    values(class)[values(class) > 1] = 2
    plot(class,main="Clean")
    
    overall.list <- c(accuracy(class)[1])
    users.list <- c(accuracy(class)[2])
    producers.list <- c(accuracy(class)[3])
    name.list <- c("Clean")
    for(i in 1:length(masks)){
      print(paste("06-Validation: Validate Image ",i+1," of ",length(masks)+1,sep=""))
      
      mask <- masks[[i]]
      mask <- resample(mask,r.ref,resample="bilinear")
      mask <- projectRaster(mask,crs=crs,res=res)
      
      values(mask)[values(mask) >= 2.5] <- NA
      mask <- na.omit(mask)
      values(mask)[values(mask) <= 1.5] <- 1
      values(mask)[values(mask) > 1.5 & values(mask) < 2.5] <- 2
      
      class.masked <- cover(mask,class)
      values(class.masked)[values(class.masked) < 1.5] = 1
      values(class.masked)[values(class.masked) > 1] = 2
      
      
      name <- names(mask)
      if(substring(name,1,1)=="X"){
        name <- substring(names(mask),5,nchar(names(mask)))
      }
      
      plot(class.masked,main=name)
      
      filename <- paste("01-ValidationResult_",name,".tif",sep="")
      writeRaster(class.masked,filename=filename,format="GTiff",overwrite=TRUE)
      
      overall.list <- c(overall.list,accuracy(class.masked)[1])
      users.list <- c(users.list,accuracy(class.masked)[2])
      producers.list <- c(producers.list,accuracy(class.masked)[3])
      name.list <- c(name.list,name)
    }
  
    d <- data.frame(Type=name.list,Overall=overall.list,Users=users.list,Producers=producers.list)
    
    dataname <- paste('02_Data_Validation.txt',sep='')
    write.table(d,dataname,row.names=FALSE,sep=';')
  
    setwd(default)
    return(d)
  }
}




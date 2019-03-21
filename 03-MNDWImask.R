maskraster.function <- function(G,IR,index){
  # (M)NDWImask function
  setwd(paste(default,"/Data/Input/Sentinel-2/MNDWI",sep=""))
  files <- list.files(pattern="*Sen2.tif", full.names=T)
  NDWI.list <- c()
  for(i in 1:length(files)){
    print(paste("03-MNDWImask:",index,"Image",i,"of",length(files),sep=" "))
    if(i<2){
      sen2 <- stack(files[i])
      NDWI <- (sen2[[G]]-sen2[[IR]])/(sen2[[G]]+sen2[[IR]])
      NDWI.list <- NDWI
    }
    else{
      sen2 <- stack(files[i])
      NDWI <- (sen2[[G]]-sen2[[IR]])/(sen2[[G]]+sen2[[IR]])
      NDWI.list <- stack(NDWI.list,NDWI)
    }
  }
  print(paste("03-MNDWImask: Calculate Mean",index,sep=" "))
  meanNDWI <- calc(NDWI.list,mean,na.rm=TRUE)
  return(meanNDWI)
}

index.fun <- function(index){
  setwd(paste(default,"/Data/Output/03-MNDWImask/",sep=""))
  if(!file.exists(paste("01-",index,".tif",sep=""))==TRUE){
    print("03-MNDWImask: Output not found, create new output...")
    print("03-MNDWImask: Initialisation")
    if(index == "NDWI"){
      maskraster <- maskraster.function(3,4,index)
    }
    if(index == "MNDWI"){
      maskraster <- maskraster.function(3,5,index)
    }    
    
    # Project and crop maskraster for Study Area
    print("03-MNDWImask: Projecting")
    maskraster <- projectRaster(maskraster,crs=crs,res=res)
    maskraster <- resample(maskraster,r.ref,resample='bilinear')
    names(maskraster)[1] <- paste(index)
    writeRaster(maskraster, filename=paste(index,".tif",sep=""), format="GTiff", overwrite=TRUE)
    setwd(paste(default,"/Data/Output/03-MNDWImask/",sep=""))
    writeRaster(maskraster, filename=paste("01-",index,".tif",sep=""), format="GTiff", overwrite=TRUE)
  }
  else{
    print(paste("03-MNDWImask: Load ",index,sep=""))
    setwd(paste(default,"/Data/Output/03-MNDWImask/",sep=""))
    maskraster <- raster(paste("01-",index,".tif",sep=""))
    names(maskraster)[1] <- paste(index)
  }
  return(maskraster)
}

mask.fun <- function(maskraster,index,choice,overwrite){

  setwd(paste(default,"/Data/Output/03-MNDWImask/",sep=""))
  if(!file.exists(paste("02-",index,"mask-",choice,".tif",sep=""))==TRUE){
    print("03-MNDWImask: Output not found, create new output...")
    
    # Kmeans analysis for 3 clusters
    print("03-MNDWImask: Processing")
    
    df <- data.frame(data=values(maskraster))
    dh <- data.frame(data=values(maskraster))
    df <- na.omit(df)
    km <- kmeans(df$data,centers=3,iter.max = 10000)
    df$clust <- as.integer(km$cluster)
    
    # Create density curve
    d <- density(df$data)
    ts_y <- ts(d$y)
    tp <- turnpoints(ts_y)
    dtp <- data.frame(dx=d$x[tp$tppos],dy=d$y[tp$tppos])
    
    # Find locations of local maxima
    peaks <- rbind(dtp[order(-dtp$dy),][1,],dtp[order(-dtp$dy),][2,],dtp[order(-dtp$dy),][3,])
    peak.land <- peaks[order(peaks$dx),][1,]
    peak.mixed <- peaks[order(peaks$dx),][2,]
    peak.water <- peaks[order(peaks$dx),][3,]
    
    # Create separate data.frames for different clusters
    c1 <- df[df$clust == 1,]
    mean.c1 <- mean(c1$data)
    c2 <- df[df$clust == 2,]
    mean.c2 <- mean(c2$data)
    c3 <- df[df$clust == 3,]
    mean.c3 <- mean(c3$data)
    
    dc <- data.frame(cluster=c(1,2,3),mean=c(mean.c1,mean.c2,mean.c3))
    dc <- dc[order(dc$mean),]
    
    c.land <- dc$cluster[1]
    c.mixed <- dc$cluster[2]
    c.water <- dc$cluster[3]
    
    df.land <- df[(as.factor(df$clust) == c.land),]
    df.mixed <- df[(as.factor(df$clust) == c.mixed),]
    df.water <- df[(as.factor(df$clust) == c.water),]
    
    dens <- data.frame(x=d$x,y=d$y)
    d.land <- dens[(dens$x < max(df.land$data)),]
    d.land <- rbind(d.land,c(tail(d.land$x,n=1),0))
    d.mixed <- dens[(dens$x < max(df.mixed$data)),]
    d.mixed <- dens[(d.mixed$x > min(df.mixed$data)),]
    d.mixed <- rbind(c(head(d.mixed$x,n=1),0),d.mixed,c(tail(d.mixed$x,n=1),0))
    d.water <- dens[(dens$x > min(df.water$data)),]
    d.water <- rbind(d.water,c(head(d.water$x,n=1),0))
    
    # Set land and water thresholds for boundaries of clusters, peaks or means of clusters
    landthreshold <- min(df.mixed$data)
    
    if(choice == "Mean"){
      waterthreshold <- mean(df.water$data)
    }
    if(choice == "Edge"){
      waterthreshold <- min(df.water$data)
    }
    if(choice == "Peak"){
      waterthreshold <- peak.water$dx
    }
    
    print(paste("03-MNDWImask: Landthreshold = ",round(landthreshold,digit=2),sep=""))
    print(paste("03-MNDWImask: Waterthreshold = ",round(waterthreshold,digit=2),sep=""))
    
    # Crop the maskraster for the desired Study Area
    setwd(default)
    
    
    # Create a backup maskraster
    maskraster2 <- maskraster
    
    # Create mask for fixed land and water areas
    values(maskraster)[values(maskraster) > landthreshold & values(maskraster) < waterthreshold] = NA
    values(maskraster)[values(maskraster) >= waterthreshold] = 1
    values(maskraster)[values(maskraster) <= landthreshold] = 2
    values(maskraster)[is.na(values(maskraster))] <- 3 
    
    shape <- studyarea.selection("SA")
    maskraster <- crop(maskraster,extent(shape))
    maskraster <- mask(maskraster,shape)
    
    # Visualize maskraster
    values(maskraster2)[values(maskraster2) >= waterthreshold] = 3
    values(maskraster2)[values(maskraster2) <= landthreshold] = -3
    
    # Plot the Results
    data <- data.frame(x=d$x,y=d$y)
    data$type <- rep(0,length(data$x))
    
    data$type[data$x <= max(d.land$x)] = "Land"
    data$type[data$x > max(d.land$x) & data$x <= max(d.mixed$x)] = "Mixed"
    data$type[data$x > max(d.mixed$x) & data$x <= max(d.water$x)] = "Water"
    
    l <- which.min(abs(data$x - landthreshold)) 
    if(index=="NDWI"){
      x.l1 <- data$x[l+2]
      y.l1 <- data$y[l+2]
      lwd.l <- 1.8
      col.l <- "#f9ecdc"
    }
    if(index=="MNDWI"){
      x.l1 <- data$x[l-1]
      y.l1 <- data$y[l-1]
      lwd.l <- 1
      col.l <- "#dcab5e"
    }
    x.l <- data$x[l]
    y.l <- data$y[l]
    
    m <- which.min(abs(data$x - max(d.mixed$x))) 
    if(index=="NDWI"){
      x.m <- data$x[m]
      y.m <- data$y[m]
      lwd.m <- 1
    }
    if(index=="MNDWI"){
      x.m <- data$x[m-1]
      y.m <- data$y[m-1]
      lwd.m <- 1.5
    }
    
    w <- which.min(abs(data$x - waterthreshold)) 
    x.w <- data$x[w]
    y.w <- data$y[w]
    
    p1 <- ggplot(data=data,aes(x,y)) +
      geom_area(aes(fill=type)) +
      geom_line(aes(rep(x.l1,length(data$x)),seq(0,y.l1,y.l1/(length(data$x)-1))),col=col.l,lwd=lwd.l) +
      geom_line(aes(rep(x.m,length(data$x)),seq(0,y.m,y.m/(length(data$x)-1))),col="#f9ecdc",lwd=lwd.m) +
      geom_line(aes(rep(x.l,length(data$x)),seq(0,y.l,y.l/(length(data$x)-1))),linetype="twodash") +
      geom_line(aes(rep(x.w,length(data$x)),seq(0,y.w,y.w/(length(data$x)-1))),linetype="twodash") +
      geom_line(aes(seq(min(data$x),landthreshold,(landthreshold-min(data$x))/(length(data$x)-1)),rep(0,length(data$y)))) +
      geom_line(aes(seq(landthreshold,waterthreshold,(waterthreshold-landthreshold)/(length(data$x)-1)),rep(0,length(data$y))),linetype="twodash") +
      geom_line(aes(seq(waterthreshold,max(data$x),(max(data$x)-waterthreshold)/(length(data$x)-1)),rep(0,length(data$y)))) +
      geom_point(aes(x=x.l, y=y.l), colour="black",lwd=1.5) +
      geom_point(aes(x=x.w, y=y.w), colour="black",lwd=1.5) +
      geom_line(aes(x,y)) +
      scale_fill_manual(values=c("#dcab5e","#f9ecdc","#1a95c2"),name = "Land Cover") +
      ylim(c(0,max(data$y))) +
      ylab("Density\n") +
      xlab(paste("\n",index,sep="")) +
      theme_stata() +
      theme(legend.position = c(0.9,0.804),
            text = element_text(size=15),
            axis.title=element_text(size=16),
            legend.title = element_text(colour="black",size=14,face="bold"),
            legend.background = element_rect(color = "grey20",fill = "grey100"),
            legend.key = element_rect(color = 'black',fill='grey100'),
            plot.background = element_rect(fill = "white",colour="black"))
    
    setwd(paste(default,"/Data/Output/03-MNDWImask/",sep=""))
    png(filename = paste("03-DensityPlot-",index,"mask-",choice,".png",sep=""),width=8,height=5,units = 'in',res = 300)
    plot(p1)
    dev.off()
    
    names(maskraster) <- paste(index,"mask-",choice,sep="")
    writeRaster(maskraster, filename=paste("02-",index,"mask-",choice,".tif",sep=""), format="GTiff", overwrite=TRUE)
  }
  
  else{
    print(paste("03-MNDWImask: Load ",index,"-mask",sep=""))
    maskraster <- raster(paste(default,"/Data/Output/03-MNDWImask/02-",index,"mask-",choice,".tif",sep=""))
  }
  
  setwd(default)
  return(maskraster)
}

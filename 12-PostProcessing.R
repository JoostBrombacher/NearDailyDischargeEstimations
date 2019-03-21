MDG.fun <- function(){
  
  # Load the three different Random Forest models
  print("12-PostProcessing: Visualise Parameter Importance")
  completeRF <- RandomForest.fun(modelname,"Complete",FALSE)
  summerRF <- RandomForest.fun(modelname,"Summer",FALSE)
  winterRF <- RandomForest.fun(modelname,"Winter",FALSE)
  
  # Extract variable importance
  a.c <- completeRF$importance[,4]
  a.s <- summerRF$importance[,4]
  a.w <- winterRF$importance[,4]
  
  # Set proper metric names
  names <- names(a.c)
  new.names <- c()
  for(i in 1:length(names)){
    name <- names[i]
    if(grepl("sdVV",name)==TRUE){
      name <- "\u03C3VV"
    }
    if(grepl("sdVH",name)==TRUE){
      name <- "\u03C3VH"
    }
    if(grepl("VVVH1",name)==TRUE){
      name <- "VV-VH"
    }
    if(grepl("VVVH2",name)==TRUE){
      name <- "VV+VH"
    }
    if(grepl("VVVH3",name)==TRUE){
      name <- "VV/VH"
    }
    if(grepl("VVmean",name)==TRUE){
      name <- "bar(VV"
    }
    if(grepl("VHmean",name)==TRUE){
      name <- "bar(VH"
    }
    new.names <- c(new.names,name)
  }
  
  # Create a data.frame
  dg <- data.frame(metric=new.names,MDG_Complete=as.numeric(a.c),MDG_Summer=as.numeric(a.s),MDG_Winter=as.numeric(a.w))
  dg <- dg[order(dg$MDG_Complete),] 
  
  # Plot the results
  bRF <- ggplot(data = dg,aes(x=seq(1,9,1))) + 
    geom_point(aes(y = MDG_Complete,shape=" Complete  ",colour=" Complete  "),size=3) +
    geom_point(aes(y = MDG_Summer,shape=" Summer  ",colour=" Summer  "),size=3) +
    geom_point(aes(y = MDG_Winter,shape=" Winter  ",colour=" Winter  "),size=3) +
    scale_x_continuous(breaks = seq(1,9,1), labels=dg$metric) +
    scale_shape_manual("",values=c(15,17,16)) +
    scale_color_brewer("",palette="Set1") +
    xlab("Metrics") +
    ylab("\nMean Decrease Gini") +
    theme_stata() + 
    theme(legend.position = "top",
          legend.title = element_blank(),
          text = element_text(size=18),
          axis.title=element_text(size=18),
          axis.text.y = element_text(angle = 0, hjust = 1),
          legend.background = element_rect(color = "grey20",fill = "grey100"),
          legend.key = element_rect(color = 'black',fill='grey100'),
          plot.background = element_rect(fill = "white",colour="black")) +
    coord_flip()
  
  # Export the plot
  setwd(paste(default,"/Data/Output/12-PostProcessing/",sep=""))
  png(filename="01-MeanDecreaseAccuracyGini.png",width=7,height=5,units='in',res=300)
  plot(bRF)
  dev.off()
  
  setwd(default)
  return(dg)
}

stdev.fun <- function(start,end,modelname,season,maskraster,choice){
  
  # Load the classified images during the validation period
  print("12-PostProcessing: Calculate Standard Deviation")
  
  setwd(paste(default,"/Data/Output/05-Classification/",modelname,"/",season,"/Validation",sep=""))
  files <- list.files(pattern="*.grd", full.names=T)
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
  
  # Select the study area
  setwd(default)
  area <- studyarea.selection("SA")
  
  # Create a rasterlist to be able to calculate the stdev
  setwd(paste(default,"/Data/Output/05-Classification/",modelname,"/",season,"/Validation",sep=""))
  files.subset <- c()
  
  templist <- c()
  for(j in 1:length(files)){
    if(as.POSIXct(datelist[j]) >= start && as.POSIXct(datelist[j]) <= end){
      temp <- raster(files[j])
      templist <- c(templist,resample(temp,r.ref))
    }
  }
  
  # Calculate the stdev
  temp_stack <- stack(templist)
  SD <- calc(temp_stack,sd)

  # Export the results
  setwd(paste(default,"/Data/Output/12-PostProcessing/",sep=""))
  name <- paste("02-StandardDeviation_MNDWI-",choice,".tif",sep="")
  writeRaster(SD,name,format="raster", overwrite=TRUE)
  return(SD)
}




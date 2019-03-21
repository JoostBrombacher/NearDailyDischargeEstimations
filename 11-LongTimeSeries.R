Qestlong.fun <- function(area,name,period,ts.period){
  
  # Create a new output directory
  print("11-LongTimeSeries: Estimate Discharge and Create Plot")
  mainDir <- paste(default,"/Data/Output/11-LongTimeSeries/",sep="")
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

  # Load the previously generated data
  data <-read.table(paste(default,"/Data/Output/08-EffectiveWidth/",modelname,"/",season,"/",ts.period,"/01_Data_We_",name,"_",area,".txt",sep=''),sep=";",header=TRUE,check.names=FALSE)
  coefficients <- read.table(paste(default,"/Data/Output/09-Bootstrap/",modelname,"/",season,"/",period,"/01_Data_Bootstrap_",name,"_",area,".txt",sep=''),sep=";",header=TRUE,check.names=FALSE)
  
  # Estimate the discharge using the bootstrapped coefficients
  d.est <- data
  for(i in 1:length(coefficients[,1])){
    a <- coefficients[i,1]
    b <- coefficients[i,2]
    est <- (data$We/a)^(1/b)
    d.est <- cbind(d.est,est)
    names(d.est)[i+3] <- paste("est",i,sep="")
  }
  
  # Calculate median and the 90% Confidence Intervals
  est.median.list <- c()
  est.Q95.list <- c()
  est.Q05.list <- c()
  for(i in 1:length(d.est[,1])){
    temp.est <- as.numeric(d.est[i,2:(length(d.est[1,]))])
    
    median.est <- median(temp.est,na.rm=TRUE)
    Q95.est <- quantile(temp.est, 0.95,na.rm=TRUE)
    Q05.est <- quantile(temp.est, 0.05,na.rm=TRUE)
    
    est.median.list <- c(est.median.list,median.est)
    est.Q95.list <- c(est.Q95.list,Q95.est)
    est.Q05.list <- c(est.Q05.list,Q05.est)
  }
  
  # Load the original coefficients 
  a.rating <- coefficients[1,1]
  b.rating <- coefficients[1,2]
  
  # Calculate the estimated discharge for the original coefficients
  est.rating <- (data$We/a.rating)^(1/b.rating)
  
  # Add the estimated discharge and the 90% Confidence Intervals to the data.frame
  data$date <- as.POSIXct(data$date)
  data$est.median <- est.median.list
  data$est.rating <- est.rating
  data$est.Q95 <- est.Q95.list
  data$est.Q05 <- est.Q05.list
  
  # Set the start and end times of the winter period
  pre.start <- as.POSIXct("2017-04-01 00:00:00")
  start <- as.POSIXct("2017-11-01 00:00:00")
  end <- as.POSIXct("2018-04-01 00:00:00")
  
  # Subset the data.frame for the winter period
  winter <- subset(data, date > start)
  winter <- subset(winter, date < end)
  
  # Subset the data.frame by excluding the winter period
  nowinter <- data
  nowinter <- nowinter[!(nowinter$date > start & nowinter$date < end),]
  nowinter <- nowinter[!(nowinter$date < pre.start),]
  
  # Calculate the RMSE and KGE
  RMSE <- formatC(signif(rmse(data$Q,data$est.rating),digits=3), digits=3,format="fg", flag="#")
  KGE <- formatC(round(KGE(data$Q,data$est.rating),digits=3), digits=3,format="fg", flag="#")
  RMSEw <- formatC(signif(rmse(winter$Q,winter$est.rating),digits=3), digits=3,format="fg", flag="#")
  KGEw <- formatC(signif(KGE(winter$Q,winter$est.rating),digits=3), digits=3,format="fg", flag="#")
  RMSEnw <- formatC(signif(rmse(nowinter$Q,nowinter$est.rating),digits=3), digits=3,format="fg", flag="#")
  KGEnw <- formatC(signif(KGE(nowinter$Q,nowinter$est.rating),digits=3), digits=3,format="fg", flag="#")
  
  print(paste("No Winter RMSE: ",RMSEnw,sep=""))
  print(paste("No Winter KGE: ",KGEnw,sep=""))
  
  # Create Plots
  p1 <- ggplot(data, aes(date)) +
    geom_line(aes(y=Q, colour = "  Observed"),size=0.75) +
    geom_line(aes(y=est.Q95,colour = "  90% CI"),size=0.75,alpha=0.5) +
    geom_line(aes(y=est.Q05,colour = "  90% CI"),size=0.75,alpha=0.5) +
    geom_ribbon(aes(ymin=est.Q05,ymax=est.Q95), fill="grey", alpha=0.4) +
    geom_line(aes(y=est.rating, colour = "  Estimated"),size=0.75) +
    geom_line(aes(y=Q, colour = "  Observed"),size=0.75) + 
    scale_y_continuous(breaks = seq(0, 10000, by = 100),limits=c(min(data$est.Q05,na.rm=TRUE)-50,max(data$est.Q95,na.rm=TRUE)))+
    labs(y = expression(atop(paste("Discharge (", m^3, "/", s,")",sep=""),"")),x='\n Date') +
    scale_color_manual(values=c("grey","firebrick2","blue")) +
    annotate("text", x = min(data$date)+0.9*(max(data$date)-min(data$date)),y = min(data$est.Q05,na.rm=TRUE)+0.925*(max(data$est.Q95,na.rm=TRUE)-min(data$est.Q05,na.rm=TRUE)+50),
             label = paste("paste(KGE, \" = ",KGE,"\")",sep=""),
             parse = TRUE,size=4.5) +
    annotate("text", x = min(data$date)+0.9*(max(data$date)-min(data$date)),y = min(data$est.Q05,na.rm=TRUE)+0.875*(max(data$est.Q95,na.rm=TRUE)-min(data$est.Q05,na.rm=TRUE)+50),
             label = paste("paste(RMSE, \" = ",RMSE,"\")",sep=""),
             parse = TRUE,size=4.5) +
    theme_stata() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          text = element_text(size=13),
          axis.title=element_text(size=13),
          legend.background = element_rect(color = "grey20",fill = "grey100"),
          legend.key = element_rect(color = 'black',fill='grey100'),
          plot.background = element_rect(fill = "white",colour="black"))

  
  p1w <- ggplot(winter, aes(date)) +
    geom_line(aes(y=Q, colour = "  Observed"),size=0.75) +
    geom_line(aes(y=est.Q95,colour = "  90% CI"),size=0.75,alpha=0.5) +
    geom_line(aes(y=est.Q05,colour = "  90% CI"),size=0.75,alpha=0.5) +
    geom_ribbon(aes(ymin=est.Q05,ymax=est.Q95), fill="grey", alpha=0.4) +
    geom_line(aes(y=est.median, colour = "  Estimated"),size=0.75) +
    geom_line(aes(y=Q, colour = "  Observed"),size=0.75) + 
    labs(y = expression(atop(paste("Discharge (", m^3, "/", s,")",sep=""),"")),x='\n Date') +
    scale_color_manual(values=c("grey","firebrick2","blue")) +
    annotate("text", x = min(winter$date)+0.15*(max(winter$date)-min(winter$date)),y = min(winter$Q,na.rm=TRUE)+0.92*(max(winter$Q,na.rm=TRUE)-min(winter$Q,na.rm=TRUE)),
             label = paste("paste(KGE, \" = ",KGEw,"\")",sep=""),
             parse = TRUE,size=4.5) +
    annotate("text", x = min(winter$date)+0.15*(max(winter$date)-min(winter$date)),y = min(winter$Q,na.rm=TRUE)+0.84*(max(winter$Q,na.rm=TRUE)-min(winter$Q,na.rm=TRUE)),
             label = paste("paste(RMSE, \" = ",RMSEw,"\")",sep=""),
             parse = TRUE,size=4.5) +
    theme_stata() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          text = element_text(size=13),
          axis.title=element_text(size=13),
          legend.background = element_rect(color = "grey20",fill = "grey100"),
          legend.key = element_rect(color = 'black',fill='grey100'),
          plot.background = element_rect(fill = "white",colour="black"))

  start.zoom <- as.POSIXct("2017-12-15 00:00:00")
  end.zoom <- as.POSIXct("2018-01-15 00:00:00")
  
  observed <- subset(data.Q.obs, date > start.zoom)
  observed <- subset(observed, date < end.zoom)

  p2 <- ggplot(observed, aes(date)) +
    geom_line(aes(y=Q, colour = "  Observed"),size=0.75) +
    labs(y = expression(atop(paste("Discharge (", m^3, "/", s,")",sep=""),"")),x='\n Date') +
    scale_color_manual(values=c("blue")) +
    theme_stata() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          text = element_text(size=13),
          axis.title=element_text(size=13),
          legend.background = element_rect(color = "grey20",fill = "grey100"),
          legend.key = element_rect(color = 'black',fill='grey100'),
          plot.background = element_rect(fill = "white",colour="black"))
 
  pw <- ggarrange(p1w,p2,ncol=1,nrow=2) 
  
  data$res <- data$est.rating-data$Q

  pres <- ggplot(data, aes(date,res)) +
    stat_smooth(method="loess", span=0.15,level=0.90, se=TRUE, aes(fill=res), alpha=0.3,col="firebrick2") +
    geom_line(aes(y=seq(0,0,length(date))),size=1,col="blue") +
    geom_segment(aes(xend = date, yend = 0),alpha=0.3,linetype="dashed") +
    geom_point(aes(y=res, colour = " Observe"),col="black") +
    scale_y_continuous(breaks = seq(-1000, 1000, by = 200),limits=c(min(data$res,na.rm=TRUE),max(data$res,na.rm=TRUE)))+
    labs(y = expression(atop(paste("Residuals (", m^3, "/", s,")",sep=""),"")),x='\nDate') +
    #scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_stata() +
    theme(legend.position = c(0.9,0.85),
          legend.title = element_blank(),
          text = element_text(size=13),
          axis.title=element_text(size=13),
          legend.background = element_rect(color = "grey20",fill = "grey100"),
          legend.key = element_rect(color = 'black',fill='grey100'),
          plot.background = element_rect(fill = "white",colour="black"))
  

  plotname <- paste("02_Plot_Estimates_",name,"_",area,".png",sep='')
  png(plotname, width = 3500, height = 1600, units='px', res=300, pointsize=10,bg="white")
  plot(p1)
  dev.off()
  
  plotname <- paste("03_Plot_WinterEstimates_",name,"_",area,".png",sep='')
  png(plotname, width = 1750, height = 2200, units='px', res=300, pointsize=10,bg="white")
  plot(pw)
  dev.off()
  
  plotname <- paste("04_Plot_Residuals_",name,"_",area,".png",sep='')
  png(plotname, width = 3500, height = 1000, units='px', res=300, pointsize=10,bg="white")
  plot(pres)
  dev.off()
  
  setwd(default)
}


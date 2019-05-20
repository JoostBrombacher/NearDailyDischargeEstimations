Qestval.fun <- function(area,name,period,overwrite){
  
  # Load the previously generated data
  print("10-DischargeEstimations: Estimate Discharge and Create Plots")
  data <-read.table(paste(default,"/Data/Output/08-EffectiveWidth/",modelname,"/",season,"/",period,"/01_Data_We_",name,"_",area,".txt",sep=''),sep=";",header=TRUE,check.names=FALSE)
  coefficients <- read.table(paste(default,"/Data/Output/09-Bootstrap/",modelname,"/",season,"/",period,"/01_Data_Bootstrap_",name,"_",area,".txt",sep=''),sep=";",header=TRUE,check.names=FALSE)

  # Create a discharge timeseries in increasing order of magnitude
  q <- seq(from=min(data$Q,na.rm=TRUE), to=max(data$Q,na.rm=TRUE), by=(max(data$Q,na.rm=TRUE)-min(data$Q,na.rm=TRUE))/(length(data$We)-1))
  
  # Estimate the discharge from the bootstrapped coefficients
  d.fit <- data.frame(q=q)
  d.est <- data
  for(i in 1:length(coefficients[,1])){
    a <- coefficients[i,1]
    b <- coefficients[i,2]
    
    fit <- a*q^b
    est <- (data$We/a)^(1/b)
    
    d.fit <- cbind(d.fit,fit)
    d.est <- cbind(d.est,est)
    names(d.fit)[i+1] <- paste("fit",i,sep="")
    names(d.est)[i+3] <- paste("est",i,sep="")
  }
  
  # Calculate the median and 90% Confidence Intervals
  est.median.list <- c()
  est.Q95.list <- c()
  est.Q05.list <- c()
  scat.median.list <- c()
  scat.Q95.list <- c()
  scat.Q05.list <- c()
  for(i in 1:length(d.fit[,1])){
    temp.est <- as.numeric(d.est[i,2:(length(d.est[1,]))])
    temp.fit <- as.numeric(d.fit[i,2:(length(d.fit[1,]))])
    
    median.est <- median(temp.est,na.rm=TRUE)
    Q95.est <- quantile(temp.est, 0.95,na.rm=TRUE)
    Q05.est <- quantile(temp.est, 0.05,na.rm=TRUE)
    
    median.fit <- median(temp.fit,na.rm=TRUE)
    Q95.fit <- quantile(temp.fit, 0.95,na.rm=TRUE)
    Q05.fit <- quantile(temp.fit, 0.05,na.rm=TRUE)
    
    est.median.list <- c(est.median.list,median.est)
    est.Q95.list <- c(est.Q95.list,Q95.est)
    est.Q05.list <- c(est.Q05.list,Q05.est)
    
    scat.median.list <- c(scat.median.list,median.fit)
    scat.Q95.list <- c(scat.Q95.list,Q95.fit)
    scat.Q05.list <- c(scat.Q05.list,Q05.fit)
  }
  
  # Load the original coefficients
  a.rating <- coefficients[1,1]
  b.rating <- coefficients[1,2]
  
  # Estimate the discharge using the original coefficients
  fit.rating <- a.rating*q^b.rating
  est.rating <- (data$We/a.rating)^(1/b.rating)
  
  # Create scatterplot data.frame
  scat <- data.frame(median=scat.median.list,rating=fit.rating,Q95=scat.Q95.list,Q05=scat.Q05.list)

  # Add estimated discharge and Confidence Intervals to data.frame
  data$date <- as.POSIXct(data$date)
  data$est.median <- est.median.list
  data$est.rating <- est.rating
  data$est.Q95 <- est.Q95.list
  data$est.Q05 <- est.Q05.list
  
  # Calculate the RMSE and KGE
  RMSE.scat <- rmse(data$We,fit.rating)
  RMSE.est <- rmse(data$Q,data$est.rating)
  KGE <- KGE(data$Q,data$est.rating)
  
  # Create new output directory
  mainDir <- paste(default,"/Data/Output/10-DischargeEstimations",sep="")
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
  
  # Plot all the results
  plotname <- paste("02_Plot_Estimations_",name,"_",area,".png",sep="")
  s1 <- ggplot(data, aes(date),theme(plot.background = element_rect(fill = "grey90"))) + 
    geom_line(aes(y = Q, colour = "  Discharge"),size=0.75) + 
    labs(y = expression(atop(paste("Discharge (", m^3, "/", s,")", sep=""),"")),x='\nDate') +
    scale_color_manual(values=c("blue"))+
    theme_stata() +
    theme(legend.position = "none",
          text = element_text(size=18),
          plot.background = element_rect(fill = "white",colour="black"))
  
  s2 <- ggplot(data, aes(date),theme(plot.background = element_rect(fill = "grey90"))) + 
    geom_line(aes(y = We, colour = "  Effective Width"),size=0.75) + 
    labs(y = 'Effective Width (m)\n',x='\nDate') +
    scale_color_manual(values=c("firebrick2"))+
    theme_stata() +
    theme(legend.position = "none",
          text = element_text(size=18),
          plot.background = element_rect(fill = "white",colour="black"))
  
  minWe <- round(min(data$We,na.rm=TRUE),0)
  maxWe <- round(max(data$We,na.rm=TRUE),0)
  diffWe <- round((maxWe-minWe)/4,-1)
  
  s3 <- ggplot(data, aes(Q,We),theme(plot.background = element_rect(fill = "grey90"))) +
    geom_line(aes(x=q,y=scat$Q95),colour="grey",size=0.5,alpha=0.5) + 
    geom_line(aes(x=q,y=scat$Q05),colour="grey",size=0.5,alpha=0.5) +
    geom_ribbon(aes(x=q,ymin=scat$Q05,ymax=scat$Q95), fill="grey", alpha=0.4) +
    geom_line(aes(x=q,y=scat$rating, colour = "  Estimated"),size=0.75) +
    geom_point(aes(data$Q,data$We), colour = 'black')+
    labs(y = 'Effective Width (m)\n',x = expression(atop("",paste("Discharge (", m^3, "/", s,")", sep="")))) +
    #scale_y_continuous(breaks = seq(round(minWe,-1), round(maxWe,-1), by = diffWe),limits=c(minWe-0.05*minWe,maxWe+0.05*minWe)) +
    scale_color_manual(values=c("red"), 
                       labels=c(bquote(" "~ W[e] == .(signif(a.rating,2))~Q^~.(formatC(signif(b.rating,2),digits=2,format="fg",flag="#"))~" "))) +
    annotate("text", x= min(data$Q)+0.15*(max(data$Q)-min(data$Q)), y=minWe+0.90*(maxWe-minWe),
             label = paste("paste(RMSE, \" = ",signif(RMSE.scat,2),"\")",sep=""),
             parse = TRUE,size=5.5) +
    theme_stata() +
    theme(legend.position = c(0.770,0.15),
          legend.title = element_blank(),
          text = element_text(size=18),
          axis.title=element_text(size=18),
          legend.background = element_rect(color = "grey20",fill = "grey100"),
          legend.key = element_rect(color = 'black',fill='grey100'),
          plot.background = element_rect(fill = "white",colour="black"))
  
  data$date <- as.Date(data$date)
  
  s4 <- ggplot(data, aes(date)) +
    geom_line(aes(y=Q, colour = "  Observed"),size=0.75) +
    geom_line(aes(y=est.Q95,colour = "  90% CI"),size=0.5,alpha=0.5) +
    geom_line(aes(y=est.Q05,colour = "  90% CI"),size=0.5,alpha=0.5) +
    geom_ribbon(aes(ymin=est.Q05,ymax=est.Q95), fill="grey", alpha=0.4) +
    geom_line(aes(y=est.rating, colour = "  Estimated"),size=0.75) +
    geom_line(aes(y=Q, colour = "  Observed"),size=0.75) + 
    coord_cartesian(xlim = c(min(data$date)-0.03*(max(data$date)-min(data$date)),max(data$date)+0.03*(max(data$date)-min(data$date))), ylim = c(100,1200), expand = FALSE)+
    scale_y_continuous(breaks = seq(300, 1100, by = 200))+
    labs(y = expression(atop(paste("Discharge (", m^3, "/", s,")",sep=""),"")),x='\n Date') +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    scale_color_manual(values=c("grey","firebrick2","blue")) +
    annotate("text", x= min(data$date)+0.8*(max(data$date)-min(data$date)), y=1050,
             label = paste("paste(KGE, \" = ",format(round(KGE,3),nsmall=3) ,"\")",sep=""),
             parse = TRUE,size=5.5) +
    annotate("text", x= min(data$date)+0.8*(max(data$date)-min(data$date)), y=950,
             label = paste("paste(RMSE, \" = ",signif(RMSE.est,3),"\")",sep=""),
             parse = TRUE,size=5.5) +
    theme_stata() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          text = element_text(size=18),
          axis.title=element_text(size=18),
          legend.background = element_rect(color = "grey20",fill = "grey100"),
          legend.key = element_rect(color = 'black',fill='grey100'),
          plot.background = element_rect(fill = "white",colour="black"))

  png(filename=plotname,width=12,height=8,units='in',res=300)
  plot(ggarrange(s1, s2, s3, s4,
                 labels = c("A", "B", "C","D"),
                 ncol = 2, nrow = 2))
  dev.off()
  setwd(default)
}

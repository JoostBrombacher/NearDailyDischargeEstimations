ratingCurve.fun <- function(data){
  
  # Find a and b parameters for We=aQ^b
  fit <- try(nls(We~intercept*I(Q^power), data = data, start = list(intercept=1,power=1),trace = F,control = list(maxiter = 50000000)))
  if("try-error" %in% class(fit)){
    return(data.frame(a=NA,b=NA))
  }
  else{
    a <- coefficients(fit)[[1]]
    b <- coefficients(fit)[[2]]
    coeff <- data.frame(a=a,b=b)
    return(coeff)
  }
}

bootstrap.fun <- function(area,name,period){
  
  # Create different rating curves by bootstrapping the data
  print("09-Bootstrap: Create Bootstrap Samples")
  data <- read.table(paste(default,"/Data/Output/08-EffectiveWidth/",modelname,"/",season,"/",period,"/01_Data_We_",name,"_",area,".txt",sep=''),sep=";",header=TRUE)
  
  # Create new output directory
  mainDir <- paste(default,"/Data/Output/09-Bootstrap",sep="")
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
  
  # Create bootstrap data.frame with the fist solution for the original a and b parameters
  df.coeff <- data.frame(a=c(),b=c())
  coeff <- ratingCurve.fun(data)
  a <- coeff$a[1]
  b <- coeff$b[1]
  df.ab <- data.frame(a=a,b=b)
  df.coeff <- rbind(df.coeff,df.ab)
  
  # Bootstrap the dataset 500 times to create 500 options for a and b
  for(i in 1:500){
    b.sample <- data.frame(date=c(),We=c(),Q=c())
    for(j in 1:length(data$Q)){
      s <- data[sample(nrow(data),1),]
      b.sample <- rbind(b.sample,s)
    }
    
    coeff <- ratingCurve.fun(b.sample)
    a <- coeff$a[1]
    b <- coeff$b[1]
    df.ab <- data.frame(a=a,b=b)
    df.coeff <- rbind(df.coeff,df.ab)
  }
  
  # Export to .txt file
  dataname <- paste('01_Data_Bootstrap_',name,'_',area,'.txt',sep='')
  write.table(df.coeff,dataname,row.names=FALSE,sep=';')
  setwd(default)
}



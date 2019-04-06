## Install Packages if not yet Installed on Device
list.of.packages <- c("ggplot2", "magick","sp","raster","rgdal","hydroGOF","gridExtra","ggthemes","grid",
                      "ggsn","dplyr","rgeos","pastecs","randomForest","spatstat","caret","reshape2",
                      "maptools","ggpubr","geosphere","viridis","broom","classInt","reshape2","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load the Installed Packages 
library(ggplot2)
library(magick)
library(sp)
library(raster)
library(rgdal)
library(hydroGOF)
library(gridExtra)
library(ggthemes)
library(grid)
library(ggsn)
library(dplyr)
library(rgeos)
library(pastecs)
library(randomForest)
library(spatstat) 
library(caret)
library(reshape2)
library(maptools)
library(rasterVis) 
library(maptools)
library(ggpubr)
library(geosphere)
library(viridis)
library(broom)
library(classInt)
library(reshape2)
library(scales)

# Change the axis language of plots to English
Sys.setlocale(category="LC_ALL",locale="english")

# Load Discharge Observations (2015-2018)
data.Q.obs <- read.table("Data/Input/Discharge/VHM30-2015-2018.txt",sep=";",header=TRUE,row.names=NULL)
data.Q.obs <- data.frame(date=as.POSIXct(data.Q.obs$date),Q=data.Q.obs$Q)
data.Q.val <- subset(data.Q.obs, date > as.POSIXlt("2018-05-01 00:00:00"))
data.Q.val <- subset(data.Q.val, date < as.POSIXlt("2018-08-01 00:00:00"))
data.Q.est <- subset(data.Q.obs, date > as.POSIXlt("2017-01-01 00:00:00"))
data.Q.est <- subset(data.Q.est, date < as.POSIXlt("2018-11-01 00:00:00"))

# Visualize the discharge observations for specific periods
obs.fun <- function(start,end,pr){
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  
  observed <- subset(data.Q.obs, date > start)
  observed <- subset(observed, date < end)

  p1 <- ggplot(data.Q.obs, aes(date)) +
    geom_line(aes(y=Q, colour = "  Observed"),size=0.2) +
    geom_vline(xintercept=start,linetype='dashed',size=0.5) +
    geom_vline(xintercept=end,linetype='dashed',size=0.5) +
    labs(y = expression(atop(paste("Discharge (", m^3, "/", s,")",sep=""),"")),x='\n Date') +
    ggtitle("Observed Discharge") +
    scale_color_manual(values=c("blue")) +
    theme_stata() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          text = element_text(size=13),
          axis.title=element_text(size=13),
          legend.background = element_rect(color = "grey20",fill = "grey100"),
          legend.key = element_rect(color = 'black',fill='grey100'),
          plot.background = element_rect(fill = "white",colour="black"))
  
  minQ <- round(min(observed$Q,na.rm=TRUE),-1)
  maxQ <- round(max(observed$Q,na.rm=TRUE),-1)
  diffQ <- round((maxQ-minQ)/4,-1)
  
  p2 <- ggplot(observed, aes(date)) +
    geom_line(aes(y=Q, colour = "  Observed"),size=0.5) +
    labs(y = expression(atop(paste("Discharge (", m^3, "/", s,")",sep=""),"")),x='\n Date') +
    scale_color_manual(values=c("blue")) +
    scale_y_continuous(breaks = seq(minQ, maxQ, by = diffQ),limits=c(minQ,maxQ)) +
    ggtitle(paste(start," | ",end,sep="")) +
    theme_stata() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          text = element_text(size=13),
          axis.title=element_text(size=13),
          legend.background = element_rect(color = "grey20",fill = "grey100"),
          legend.key = element_rect(color = 'black',fill='grey100'),
          plot.background = element_rect(fill = "white",colour="black"))
  
  plot(ggarrange(p1, p2,ncol = 1, nrow = 2))
  
  if(pr==TRUE){
    return(observed)
  }
}

# Set Projection (ISN93 / Lambert 1993) and load reference crs, extent and raster file.
crs <- "+proj=lcc +lat_1=64.25 +lat_2=65.75 +lat_0=65 +lon_0=-19 +x_0=500000 +y_0=500000 +ellps=GRS80 +units=m +no_defs"
crsWGS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
res <- c(10, 10)
extent <- c(421151.3, 430133, 381880, 392300)
setwd(paste(default,"/Data/Input/Sentinel-1/ValidationTraining/",modelname,sep=""))
files <- list.files(pattern="*.tif", full.names=T)
r.ref <- raster(files[1])
r.ref <- projectRaster(r.ref,crs=crs,res=res,extent=extent)
setwd(default)

# Load Shapefiles Selected Study Area
studyarea.selection <- function(choice){
  if(choice == "SA"){
    studyarea <- readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/StudyArea.shp",sep=""),verbose=FALSE)
  }
  if(choice == "A1"){
    studyarea <- readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/Pjorsa1.shp",sep=""),verbose=FALSE)
  }
  if(choice == "A2"){
    studyarea <- readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/Pjorsa2.shp",sep=""),verbose=FALSE)
  }
  if(choice == "A3"){
    studyarea <- readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/Pjorsa3.shp",sep=""),verbose=FALSE)
  }
  if(choice == "A4"){
    studyarea <- readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/Pjorsa4.shp",sep=""),verbose=FALSE)
  }
  if(choice == "A5"){
    studyarea <- readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/Pjorsa5.shp",sep=""),verbose=FALSE)
  }
  return(studyarea)
}

# Calculate Length River selected Study Area
riverlength <- function(choice){
  if(choice == "SA"){
    riverlength <- gLength(spTransform(readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/SA.shp",sep=""),verbose=FALSE),CRS("+proj=utm +zone=27W ellps=WGS84")),byid=FALSE)
  }
  if(choice == "A1"){
    riverlength <- gLength(spTransform(readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/S1.shp",sep=""),verbose=FALSE),CRS("+proj=utm +zone=27W ellps=WGS84")),byid=FALSE)
  }
  if(choice == "A2"){
    riverlength <- gLength(spTransform(readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/S2.shp",sep=""),verbose=FALSE),CRS("+proj=utm +zone=27W ellps=WGS84")),byid=FALSE)
  }
  if(choice == "A3"){
    riverlength <- gLength(spTransform(readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/S3.shp",sep=""),verbose=FALSE),CRS("+proj=utm +zone=27W ellps=WGS84")),byid=FALSE)
  }
  if(choice == "A4"){
    riverlength <- gLength(spTransform(readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/S4.shp",sep=""),verbose=FALSE),CRS("+proj=utm +zone=27W ellps=WGS84")),byid=FALSE)
  }
  if(choice == "A5"){
    riverlength <- gLength(spTransform(readOGR(dsn = paste("Data/Input/Shapefiles/Lambert/S5.shp",sep=""),verbose=FALSE),CRS("+proj=utm +zone=27W ellps=WGS84")),byid=FALSE)
  }
  return(riverlength)
}

# Create barplots of the amount of Sentinel-1 and 2 observations per quarter
setwd(paste(default,"/Data/Input/Sentinel-1/Images/",modelname,sep=""))
s1.files <- list.files(pattern="*.tif", full.names=T)
setwd(paste(default,"/Data/Input/Sentinel-2/MNDWI",sep=""))
s2.files <- list.files(pattern="*Sen2.tif", full.names=T)

s1.data <- data.frame(date=c(NA),quarters=c(NA))

for(i in 1:length(s1.files)){
  
  date <- substring(s1.files[i],10,23)
  Y <- substr(date,1,4)
  M <- substr(date,5,6)
  D <- substr(date,7,8)
  h <- substr(date,9,10)
  m <- substr(date,11,12)
  s <- substr(date,13,14)
  
  date <- paste(Y,'-',M,'-',D,' ',h,':00:00',sep="")
  
  if(date >= as.POSIXlt("2017-01-01 00:00:00") & date < as.POSIXlt("2017-04-01 00:00:00")){
    d <- c(date,"2017-Q1")
    s1.data <- rbind(s1.data,d)
  }
  if(date >= as.POSIXlt("2017-04-01 00:00:00") & date < as.POSIXlt("2017-07-01 00:00:00")){
    d <- c(date,"2017-Q2")
    s1.data <- rbind(s1.data,d)
  }
  if(date >= as.POSIXlt("2017-07-01 00:00:00") & date < as.POSIXlt("2017-10-01 00:00:00")){
    d <- c(date,"2017-Q3")
    s1.data <- rbind(s1.data,d)
  }
  if(date >= as.POSIXlt("2017-10-01 00:00:00") & date < as.POSIXlt("2018-01-01 00:00:00")){
    d <- c(date,"2017-Q4")
    s1.data <- rbind(s1.data,d)
  }
  if(date >= as.POSIXlt("2018-01-01 00:00:00") & date < as.POSIXlt("2018-04-01 00:00:00")){
    d <- c(date,"2018-Q1")
    s1.data <- rbind(s1.data,d)
  }
  if(date >= as.POSIXlt("2018-04-01 00:00:00") & date < as.POSIXlt("2018-07-01 00:00:00")){
    d <- c(date,"2018-Q2")
    s1.data <- rbind(s1.data,d)
  }
  if(date >= as.POSIXlt("2018-07-01 00:00:00") & date < as.POSIXlt("2018-10-01 00:00:00")){
    d <- c(date,"2018-Q3")
    s1.data <- rbind(s1.data,d)
  }
  if(date >= as.POSIXlt("2018-10-01 00:00:00") & date < as.POSIXlt("2019-01-01 00:00:00")){
    d <- c(date,"2018-Q4")
    s1.data <- rbind(s1.data,d)
  }
}

s2.data <- data.frame(date=c(NA),quarters=c(NA))

for(i in 1:length(s2.files)){
  
  date <- substring(s2.files[i],10,23)
  Y <- substr(date,1,4)
  M <- substr(date,5,6)
  D <- substr(date,7,8)
  h <- substr(date,9,10)
  m <- substr(date,11,12)
  s <- substr(date,13,14)
  
  date <- paste(Y,'-',M,'-',D,' ',h,':00:00',sep="")
  
  if(date >= as.POSIXlt("2017-01-01 00:00:00") & date < as.POSIXlt("2017-04-01 00:00:00")){
    d <- c(date,"2017-Q1")
    s2.data <- rbind(s2.data,d)
  }
  if(date >= as.POSIXlt("2017-04-01 00:00:00") & date < as.POSIXlt("2017-07-01 00:00:00")){
    d <- c(date,"2017-Q2")
    s2.data <- rbind(s2.data,d)
  }
  if(date >= as.POSIXlt("2017-07-01 00:00:00") & date < as.POSIXlt("2017-10-01 00:00:00")){
    d <- c(date,"2017-Q3")
    s2.data <- rbind(s2.data,d)
  }
  if(date >= as.POSIXlt("2017-10-01 00:00:00") & date < as.POSIXlt("2018-01-01 00:00:00")){
    d <- c(date,"2017-Q4")
    s2.data <- rbind(s2.data,d)
  }
  if(date >= as.POSIXlt("2018-01-01 00:00:00") & date < as.POSIXlt("2018-04-01 00:00:00")){
    d <- c(date,"2018-Q1")
    s2.data <- rbind(s2.data,d)
  }
  if(date >= as.POSIXlt("2018-04-01 00:00:00") & date < as.POSIXlt("2018-07-01 00:00:00")){
    d <- c(date,"2018-Q2")
    s2.data <- rbind(s2.data,d)
  }
  if(date >= as.POSIXlt("2018-07-01 00:00:00") & date < as.POSIXlt("2018-10-01 00:00:00")){
    d <- c(date,"2018-Q3")
    s2.data <- rbind(s2.data,d)
  }
  if(date >= as.POSIXlt("2018-10-01 00:00:00") & date < as.POSIXlt("2019-01-01 00:00:00")){
    d <- c(date,"2018-Q4")
    s2.data <- rbind(s2.data,d)
  }
}

s1.data <- s1.data[complete.cases(s1.data), ]
s2.data <- s2.data[complete.cases(s2.data), ]

Quarters <- c("2017-Q1","2017-Q2","2017-Q3","2017-Q4",
             "2018-Q1","2018-Q2","2018-Q3","2018-Q4")

s1.num <- c()
s2.num <- c()
for(i in 1:length(Quarters)){
  s1.num  <- c(s1.num,length(s1.data$quarters[s1.data$quarters == Quarters[i]]))
  s2.num  <- c(s2.num,length(s2.data$quarters[s2.data$quarters == Quarters[i]]))
}

dQ <- data.frame(Quarters=Quarters,"S1"=s1.num,"S2"=s2.num)

dQm <- melt(dQ[,c('Quarters','S1','S2')],id.vars = 1)
names(dQm)[2] <- "Source"

b1 <- ggplot(dQm,aes(x = Quarters,y = value, label=value)) + 
  geom_bar(stat = "identity", aes(fill = Source)) +
  scale_y_continuous(breaks = seq(0,75,10), lim=c(0,65),expand = c(0, 0)) +
  scale_fill_brewer(palette="Paired",labels=c(" Sentinel-1  "," Sentinel-2  ")) +
  xlab("\nQuarters") +
  ylab("Number of Images\n") +
  theme_stata() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size=15),
        axis.title=element_text(size=16),
        legend.background = element_rect(color = "black",fill = "white"),
        legend.key = element_rect(color = 'black',fill='white'),
        plot.background = element_rect(fill = "white",colour=NA))

setwd(paste(default,"/Data/Output/02-LoadData",sep=""))
png(filename="01_Barplot_S12.png",width=8,height=5,units='in',res=300)
plot(b1)
dev.off()

setwd(default)

Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

library(ncdf4)
library(chron)
library(dplyr)
library(subsetnc)

sdate <- "2007-01-01"
edate <- "2020-12-31"

NA_FLAG <- -9999
date.origin <- list("CCI" = c(1,1,1900), "GPM"=c(1,1,2000), "ASCAT"=c(1,1,2000))
varlon <- c("CCI" = "Longitude", "GPM" = "Longitude", "ASCAT" = "lon")
varlat <- c("CCI" = "Latitude", "GPM" = "Latitude", "ASCAT" = "lat")
vartime <- c("CCI" = "Time", "GPM" = "Time", "ASCAT" = "time")
varprcp <- c("CCI" = "Rainfall", "GPM" = "Rainfall", "ASCAT" = "rainfall")

stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
num_stations <- dim(stations)[[1]]

bbox_lon_min <- -6
bbox_lon_max <- 3
bbox_lat_min <- 8
bbox_lat_max <- 16

slat <- stations$Latitude
slon <- stations$Longitude

models <- names(date.origin)

for (model in models) {
  print(paste0("Processing: ",model))
  files <- list.files(path = paste0("DATA/",model), pattern = "nc")
  df <- data.frame(Date=as.Date(character()),matrix(nrow=0, ncol=num_stations))
  
  for (fname in files){
    #fname <- files[1]
    print(paste("Processing >>", fname))
    ncfile <- nc_open(paste0("DATA/",model,"/",fname))
    
    nc_lon <- ncvar_get(ncfile,varlon[model])
    nc_lat <- ncvar_get(ncfile, varlat[model])
    
    xmin <- nc_lon[which.min(abs(nc_lon - bbox_lon_min))]
    xmax <- nc_lon[which.min(abs(nc_lon - bbox_lon_max))+1]
    ymin <- nc_lat[which.min(abs(nc_lat - bbox_lat_min))]
    ymax <- nc_lat[which.min(abs(nc_lat - bbox_lat_max))+1]
    
    if (model == "ASCAT") {
      ncsub <- ncfile %>% nc_subset(lon %in% xmin:xmax,lat %in% ymin:ymax)
    } else {
      ncsub <- ncfile %>% nc_subset(Longitude %in% xmin:xmax,Latitude %in% ymin:ymax)
    }
    
    nc_close(ncfile)
    
    nc_lon <- ncvar_get(ncsub,varlon[model])
    nc_lat <- ncvar_get(ncsub, varlat[model])  
    pr <- ncvar_get(ncsub, varprcp[model])
    time <- ncvar_get(ncsub,vartime[model])
    
    sm2rain <- data.frame(chron(time, origin = date.origin[[model]]))
    colnames(sm2rain)[1] <- "Date"
    sm2rain$Date <- as.POSIXct(sm2rain$Date,format="%Y-%m-%d",tz="UTC")
    
    for (i in 1:num_stations) {
      print(paste("Reading station",stations$Name[i]))
      lon_index <- which.min(abs(nc_lon - slon[i]))
      lat_index <- which.min(abs(nc_lat - slat[i]))
      
      #ascat rainfall[lon,lat,time]
      #cci Rainfall[time,lon,lat]
      #gpm Rainfall[time,latitude,longitude]
      
      if (model == "CCI") {
        spr <- pr[,lon_index,lat_index]      
      } else if (model == "GPM") {
        spr <- pr[,lat_index,lon_index]  
      } else {
        spr <- pr[lon_index,lat_index,]  
      }
      
      spr[is.na(spr)] <- 0
      spr[spr == NA_FLAG] <- 0    
      sm2rain <- cbind(sm2rain, spr)
    }
    
    rm(pr)
    #print(paste0("Mem Used: ", mem_used())) -- from pryr, to monitor mem used
    nc_close(ncsub)
    names(sm2rain) <- c("Date", stations$Name)
    colnames(df) <- colnames(sm2rain)
    for (i in 1:nrow(sm2rain)) df[nrow(df)+1,] <- sm2rain[i,]
  }

  df$Date <- as.POSIXct(df$Date,format="%Y-%m-%d",tz="UTC")
  colnames(df) <- c("Date", as.character(stations$Code))
  
  filter_rows <- which(df$Date>=as.POSIXct(sdate,format="%Y-%m-%d",tz="UTC") & 
                         (df$Date<=as.POSIXct(edate,format="%Y-%m-%d",tz="UTC")))
  
  df <- df[filter_rows,] 
  dft <- data.frame(Date = seq(as.POSIXct(sdate,format="%Y-%m-%d",tz="UTC"),
                               as.POSIXct(edate,format="%Y-%m-%d",tz="UTC"),
                               by="day"))
  
  df <- merge(dft, df, by="Date", all.x=T)
  
  write.table(df, file=paste0("EXP/",model,"_PP.csv"), sep = ",",
              row.names = FALSE, col.names = T)  
}

print("Finished.")
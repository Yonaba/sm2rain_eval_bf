Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

library(ncdf4)
library(chron)

models <- c("arc2", "rfe2", "tamsat", "chirps")
NA_FLAG <- c("arc2" = -999, "rfe2" = -999, "tamsat" = "NaN", "chirps" = -9999)
model.origin <- matrix(c(c(1,1,1960),c(10,31,2000),c(1,1,1970), c(1,1,1970)), ncol=3, byrow=T)
rownames(model.origin) <- models
tfactor <- c("arc2" = 1, "rfe2" = 1, "tamsat" = 86400, "chirps" = 86400)

varlon <- c("arc2" = "X", "rfe2" = "X", "tamsat" = "lon", "chirps" = "longitude")
varlat <- c("arc2" = "Y", "rfe2" = "Y", "tamsat" = "lat", "chirps" = "latitude")
vartime <- c("arc2" = "T", "rfe2" = "T", "tamsat" = "time", "chirps" = "time")
varprcp <- c("arc2" = "est_prcp", "rfe2" = "est_prcp", "tamsat" = "rfe", "chirps" = "precip")

sdate <- "2007-01-01"
edate <- "2020-12-31"

stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
num_stations <- dim(stations)[[1]]
slat <- stations$Latitude
slon <- stations$Longitude

for (model in models) {
  print(paste0("Processing: ",model))
  
  ncpath <- paste0("DATA/",toupper(model),"/",model,".nc")
  ncfile <- nc_open(ncpath)
  
  nc_lon <- ncvar_get(ncfile,varlon[model])
  nc_lat <- ncvar_get(ncfile, varlat[model])
  pr <- ncvar_get(ncfile, varprcp[model])
  time <- ncvar_get(ncfile,vartime[model])/tfactor[model]
  
  df <- data.frame(matrix(nrow=length(time), ncol=num_stations+1))
  colnames(df) <- c("Date",stations$Code)
  
  df$Date <- chron(time, origin =  model.origin[model,])
  df$Date <- as.POSIXct(df$Date,format="%Y-%m-%d",tz="UTC")
  
  for (i in 1:num_stations) {
    #i <- 2
    scode <- as.character(stations[i,"Code"])
    print(paste("Reading station",stations$Name[i]))
    lon_index <- which.min(abs(nc_lon - slon[i]))
    lat_index <- which.min(abs(nc_lat - slat[i]))
    spr <- pr[lon_index,lat_index,]
    spr[is.na(spr)] <- 0
    spr[spr == NA_FLAG[model]] <- NA
    df[,scode] <- spr
  }
  
  rm(pr)
  nc_close(ncfile)
  
  filter_rows <- which(df$Date>=as.POSIXct(sdate,format="%Y-%m-%d",tz="UTC") & 
                         (df$Date<=as.POSIXct(edate,format="%Y-%m-%d",tz="UTC")))
  df <- df[filter_rows,] 
  
  write.table(df, file=paste0("EXP/",toupper(model),"_PP.csv"), sep = ",",row.names = FALSE, col.names = T)
}


print("Finished.")

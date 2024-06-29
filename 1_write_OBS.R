Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

sdate <- "2007-01-01"
edate <- "2020-12-31"
timevec <- seq(as.POSIXct(sdate,format="%Y-%m-%d",tz="UTC"),
               as.POSIXct(edate,format="%Y-%m-%d",tz="UTC"),
               by="day")

stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
num_stations <- dim(stations)[[1]]

df <- data.frame(matrix(nrow=length(timevec), ncol=num_stations+1))
colnames(df) <- c("Date",stations$Code)
df$Date <- timevec

for (scode in stations$Code) {
  #scode <- as.character(stations$Code[1])
  sdata <- read.csv(paste0("DATA/OBS/",scode,"_pr.csv"), header = TRUE, sep = ",", dec = ".")$values
  df[,as.character(scode)] <- sdata
}

write.table(df, file=paste0("EXP/OBS_PP.csv"), sep = ",",row.names = FALSE, col.names = T)

print("Finished.")

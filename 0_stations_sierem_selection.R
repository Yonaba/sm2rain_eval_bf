
outpath <- "D:/Recherche/SM2RAIN/DATA/OBS/"
f <- list.files(outpath, include.dirs = F, full.names = T, recursive = T)
file.remove(f)
setwd("C:/SIEREM/data/pr/")

library(lubridate)
library(dplyr)
library(readxl)
library(tools)
library(powerjoin)

print("============= Processing Rainfall stations =============")
prss <- list.files(pattern=".csv")

tseq <- seq(as.POSIXct("2007-01-01"), as.POSIXct("2020-12-31"), by="day")

dfl <- data.frame(matrix(nrow=0, ncol=2))
colnames(dfl) <- c("station", "na_percent")

for (prs in prss) {
  #prs <- prss[22]
  print(paste0("Processing ",file_path_sans_ext(prs)))
  
  df <- read.csv(paste0(prs),header = T, sep = ",", dec = ".")
  df$dates <- as.POSIXct(df$dates,format="%Y-%m-%d",tz="UTC")  
  
  fdf <- data.frame(dates = tseq)
  fdf <- merge(fdf, df, by = "dates", all.x = T)
  
  if (!all(is.na(fdf$values))) {
    napc <- (length(which(is.na(fdf$values)))/nrow(fdf))*100
    dfl[nrow(dfl)+1,] <- c(prs, round(napc,2))
    write.csv(fdf, file = paste0(outpath,prs), row.names = F) 
  } 
}
write.csv(dfl, file = paste0("D:/Recherche/SM2RAIN/Tables/obs_stats.csv"), row.names = F) 
print("finished.")

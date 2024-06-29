Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

library(viridis)
library(biwavelet)
library(ggplot2)
library(ggpubr)
source("0_lib_import.R")

products <- c("OBS","CCI", "GPM", "ASCAT","ARC2", "RFE2", "TAMSAT", "CHIRPS")
stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
rownames(stations) <- stations$Code
cols <- c("black","#e6194b","#9A6324","#42d4f4","#469990","#f58231","#ffe119","#bfef45") #distinctColorPalette(7)

pr <- import.rainfall(products)$lmonthly
df <- average_product(pr,1,7)

siglevel <- 0.90
minp <- 2
maxp <- 180

syear <- 2007
eyear <- 2020
years <- c(syear:eyear)
lperiods <- c(2,4,8,16,32,64)

res <- 600
factor <- res/72

png("graphs/wavelet_transform.png", units = "px", pointsize = 8,
    width = 700 * factor * 1.35, height = 380 * factor, res = res,restoreConsole = TRUE)

par(mfrow=c(2,4))
par(font.axis=2,font.lab=2)

cp <- 0
for (p in products) {
  #p <- "OBS"
  print(p)
  cp <- cp + 1
  par(mar = c(5.5,ifelse((cp %in% c(1,5)),7,3),3,ifelse((cp %in% c(4,8)),6,3)))
  pr.ts <- ts(df[,p], start=c(syear,1), end = c(eyear,12), frequency = 12)
  pr.ts <- data.frame(dates = seq(from=as.Date("2007-01-01"), to=as.Date("2020-12-01"), by="month"),
                      pr.ts)
  colnames(pr.ts) <- c("dates","pr")
  pr.ts <- pr.ts[!is.nan(pr.ts$pr),]
  xlabs <- (syear:as.numeric(format(pr.ts[nrow(pr.ts),"dates"],"%Y")))
  CWT <- wt(pr.ts,sig.level=siglevel,max.scale = maxp)

  plot(CWT, tol=siglevel,ylim=c(minp,maxp), type="power.norm", plot.cb = (cp %in% c(4,8)), plot.phase = F,
       #xlim = (syear:as.numeric(format(pr.ts[nrow(pr.ts),"dates"],"%Y"))),
       fill.cols = turbo(64, begin = 0.2),
       zlim = c(-1,4.5),
       lty.sig=1,lwd.sig=.5,lty.coi=1,lwd.coi=2, col.coi="black", xaxt = "n",yaxt = "n",
       xlab = "", ylab = ifelse((cp %in% c(1,5)),"Months\n",""), cex.lab = 1.75)
  title(paste0("(",letters[cp],") ",p), line = 1.0, adj = 0, cex.main = 2)
  axis(side = 1, at=seq(1,length(CWT$xaxis),12)+5, labels = xlabs, las = 2, cex.axis = 1.75)
  axis(side = 2, at=seq(1,length(lperiods),by=1), labels = lperiods, cex.axis = 1.75)

}

dev.off()

print("finished.")

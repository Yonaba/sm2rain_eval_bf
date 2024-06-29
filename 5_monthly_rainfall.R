Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

library(hydroGOF)
library(ggplot2)
library(ggpubr)
source("0_lib_import.R")

products <- c("OBS","CCI", "GPM", "ASCAT","ARC2", "RFE2", "TAMSAT", "CHIRPS")
stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
rownames(stations) <- stations$Code
cols <- c("black","#e6194b","#9A6324","#42d4f4","#469990","#f58231","#ffe119","#bfef45") #distinctColorPalette(7)

pr <- import.rainfall(products)$lmonthly
df <- monthly_rainfall(pr)

df$model <- ordered(df$model, levels = products)
df$ind <- ordered(df$ind, levels = stations$Name)

cp <- 0
plist <- list()

for (s in stations$Name) {
  cp <- cp+1
  s <- stations[cp,]
  sname <- s$Name
  stype <- s$Type
  slon <- format(round(s$Longitude,2),nsmall = 2)
  slat <- format(round(s$Latitude,2), nsmall = 2)
  slab <- paste0(stype," (lonlat: ",slon,", ",slat," dd)")
  
  ddf <- df[df$ind == sname,]
  ddf$mon <- rep(sprintf("%02d",1:12),n=length(products))
  
  plist[[cp]] <- ggplot(ddf, aes(x=mon, y=values, group = model)) +
    geom_line(data=ddf[ddf$model!="OBS",],aes(x=mon, y=values,colour=model), linewidth=0.6) + 
    geom_line(data=ddf[ddf$model=="OBS",],aes(x=mon,y=values,colour="OBS"),linetype="twodash",linewidth=0.6) +      
    xlab(ifelse(cp>=13,"Months","")) + 
    ylab(ifelse((cp %in% c(1,7,13)),"Monthly rainfall [mm]","")) + ylim(0,350) +
    labs(title = sname, subtitle = slab) +
    scale_color_manual(values=c(cols[-1],"black")) +
    scale_x_discrete(labels=month.abb) +
    theme_bw() +
    theme(plot.title = element_text(color = "black", size = 12),
          plot.subtitle = element_text(color = "black", size = 10, hjust=0),
          axis.text = element_text(color = "black", size = 10),
          axis.text.x = element_text(angle = 90),          
          axis.title.y = element_text(size = 10, margin = margin(r = 10)),        
          axis.title.x = element_text(size = 10, margin = margin(t = 10)),         
          legend.position = "bottom", legend.title = element_blank(),
          legend.key.size = unit(1,"cm"),
          legend.text = element_text(size = 10, color = "black")) +
    guides(colour = guide_legend(nrow = 1))
    
}

grob <- ggarrange(plotlist = plist, nrow = 3, ncol = 6, common.legend = T, legend = "bottom")
#grob

ggsave(plot = grob, "graphs/monthly_cmp.png",
       width = 52, height = 25, units = "cm", dpi = 400, scale = 1,bg="white")
print("finished.")

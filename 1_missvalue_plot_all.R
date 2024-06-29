setwd("D:/Recherche/SM2RAIN/")

library(lubridate)
library(stringr)
library(jcolors)
library(ggplot2)
library(ggpubr)
library(patchwork)

MIN_MON_MISS <- 100 #c(seq(10,30,5),40,50,60,75) #6:10 /10,15s/15,18s/20,27s/25,32s/30,33s
SEL_TH <- 5
MON_MISS <- 6:10

print("============= Processing Rainfall stations =============")
prss <- list.files(path = "DATA/OBS/", pattern=".csv")
sfiles <- data.frame(Code = str_match(prss,"^([a-z0-9]*)_pr.csv")[,2])

print(paste0("Processing case min: ",MIN_MON_MISS))
stations <- read.csv(paste0("input/stations_sierem.csv"),header = T, sep = ",", dec = ".")
stations <- stations[stations$Variable == "pr",]
stations <- merge(sfiles, stations, by="Code", all.x=T)
stations$miss <- NA
stations <- stations[order(stations$Latitude, stations$Longitude, decreasing = T),]
lstations <- stations[,c(1,3)]
rownames(lstations) <- stations$Code

df <- data.frame(matrix(nrow =0, ncol = 3))
colnames(df) <- c("id","dmon", "miss")

n <- 0
filter_rows <- c()
filter_rect <- c()
labs <- c()
for (scode in stations$Code) {
  n <- n + 1
  scode <- stations$Code[n]
  print(paste0("Processing ",scode," (",n,"/",nrow(stations),")"))
  sdata <- read.csv(paste0("DATA/OBS/",scode,"_pr.csv"),header = T, sep = ",", dec = ".")
  sdata$dates <- as.POSIXct(sdata$dates,format="%Y-%m-%d",tz="UTC")
  sdata$dmon <- strftime(sdata$dates, format = "%m", tz = "UTC")
  sdata$rvalues <- 1
  s1data <- aggregate(values ~ dmon, data=sdata, function(x) {sum(is.na(x))}, na.action = NULL)
  s2data <- aggregate(rvalues ~ dmon, data=sdata, function(x) {sum(!is.na(x))}, na.action = NULL)
  s1data$miss <- (s1data$values/s2data$rvalues)*100
  
  if (all(s1data[MON_MISS,"miss"]<=SEL_TH)) filter_rect <- append(filter_rect, n)
  if (all(s1data[MON_MISS,"miss"]<=MIN_MON_MISS)) {
    filter_rows <- append(filter_rows, n)
    smiss <- (sum(s1data[MON_MISS,"values"]) / sum(s2data[MON_MISS,"rvalues"]))*100
    stations$miss[n] <- smiss
    s1data$id <- scode
    s1data <- s1data[,c(4,1,3)]
    df <- rbind(df,s1data)
    lab <- paste0("   [",sprintf(round(min(s1data[MON_MISS,"miss"]),1),fmt='%#.1f'),"% - ",
                  sprintf(round(max(s1data[MON_MISS,"miss"]),1),fmt='%#.1f'),"%]")
    labs <- append(labs, lab)
  }
  # sdata$dmon <- as.numeric(sdata$dmon)
  # ll <- nrow(sdata)
  # sdata$dmonT <- ifelse(leap_year(sdata$dmon),rep(366,ll),rep(365,ll))
  # sdata$miss <- (sdata$values/(25*365+6))*100
  
  # sdata <- sdata[sdata$miss <= 5,]
  
}

f_stations <- stations[filter_rows,]
write.csv(f_stations, file = paste0("tables/stations_",MIN_MON_MISS,"_mon.csv"), row.names = F)
write.csv(stations[filter_rect,], file = paste0("tables/stations_selected.csv"), row.names = F)

#df$id <- as.factor(df$id)
#df$dmon <- as.factor(df$dmon)
df$sname <- as.factor(lstations[as.character(df$id),"Name"])

#df$lat <- ss[as.character(df$id),"Latitude"]
df$sname <- ordered(df$sname, levels=f_stations$Name)

ys <- filter_rect2 <- nrow(f_stations) - filter_rect
df.rect <- data.frame(
  xmin = rep(0.5,length(filter_rect2)),
  xmax = rep(12.5,length(filter_rect2)),
  ymin = ys + 0.5, ymax = ys + 1.5
)

labcol <- data.frame(name = f_stations$Name, type = f_stations$Type, col = NA)
labcol[labcol$type == "Climatological","col"] <- "maroon"
labcol[labcol$type == "Pluviometric","col"] <- "darkgreen"
labcol[labcol$type == "Agrobioclimatic","col"] <- "lightgreen"
labcol[labcol$type == "Synoptic","col"] <- "darkblue"
labcol <- setNames(labcol$col,as.character(labcol$name))

df2 <- data.frame(matrix(nrow = 0, ncol=ncol(df)))
colnames(df2) <- colnames(df)

n <- 0
ylabs <- c()
for (scode in stations$Code) {
  n <- n + 1
  scode <- stations$Code[n]
  print(paste0("Processing ",scode," (",n,"/",nrow(stations),")"))
  sdata <- read.csv(paste0("DATA/OBS/",scode,"_pr.csv"),header = T, sep = ",", dec = ".")
  sdata$dates <- as.POSIXct(sdata$dates,format="%Y-%m-%d",tz="UTC")
  sdata$dmon <- strftime(sdata$dates, format = "%Y", tz = "UTC")
  sdata$rvalues <- 1
  s1data <- aggregate(values ~ dmon, data=sdata, function(x) {sum(is.na(x))}, na.action = NULL)
  s2data <- aggregate(rvalues ~ dmon, data=sdata, function(x) {sum(!is.na(x))}, na.action = NULL)
  s1data$miss <- (s1data$values/s2data$rvalues)*100
  
  s1data$id <- scode
  s1data <- s1data[,c(4,1,3)]
  df2 <- rbind(df2,s1data)  
  lab <- paste0("   [",sprintf(round(min(s1data$miss),1),fmt='%#.1f'),"% - ",
                sprintf(round(max(s1data$miss),1),fmt='%#.1f'),"%]")
  ylabs <- append(ylabs, lab)  
}

df2$sname <- as.factor(lstations[as.character(df2$id),"Name"])
df2$sname <- ordered(df2$sname, levels=f_stations$Name)

tilew <- 1
tileh <- 1
df$lab <- NA

i <- 0
for (name in f_stations$Name) {
  i <- i + 1
  df[df$sname == name,"lab"] <- labs[i]
}

pl.mon <- ggplot() + 
  geom_tile(data = df, aes(x=dmon, y=sname, fill=miss, width = tilew, height = tileh),
            alpha = 0.3,color = "black",lwd =0.4,linetype = 2) +
  #scale_fill_jcolors_contin("pal12",bias = 3) +
  scale_fill_gradientn(colours = terrain.colors(7), limits=c(0,100)) +
  scale_x_discrete(labels=month.abb) +
  scale_y_discrete(limits=rev) + 
  theme_bw() + 
  coord_cartesian(clip = 'off') +
  #coord_fixed(ratio=0.55) +
  xlab("\nMonths (-)") + ylab("Gauge stations\n") + 
  geom_tile(data = df[(as.numeric(df$dmon) %in% MON_MISS),], aes(x=dmon, y=sname, fill=miss,
            width = tilew, height = tileh),
            alpha = 0.7,color = "black",lwd =0.4,linetype = 1.5) + 
  geom_rect(data = df.rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = NA, alpha = 1, color = "blue", lwd = 1.5) +   
  annotate("text", x = 13, y = 1:49, hjust=0, label = rev(labs), size = 4.5, colour = "maroon", fontface = "bold.italic") +
  #scale_y_discrete("lat", limits=rev) +
  labs(title=paste0("(a) Monthly gaps\n")) +
  guides(fill = guide_colourbar(barwidth = 2,barheight = 40, title = "Missing data (%)\n")) +
  theme(legend.background=element_blank(),
        legend.position = "none",
        axis.line = element_line(color = "black", linewidth = 0.8, linetype = "solid"),
        legend.title = element_text(face="bold", color="black",size=20),
        legend.text = element_text(face="bold", color="black",size=20),
        axis.ticks=element_line(linewidth=1.5),axis.ticks.length = unit(0.2, "cm"),
        plot.title = element_text(face = "bold", size = 30, color = "black"),
        plot.margin = unit(c(1,8,1,1), "lines"),
        legend.spacing.y = unit(1.0, 'in'),
        axis.title.x = element_text(color="black", size=26, face="bold"),
        axis.title.y = element_text(color="black", size=26, face="bold"),
        axis.text.x = element_text(face="bold", color="#993333",size=18, angle = 90,hjust=1,vjust=0.5),
        axis.text.y = element_text(face="bold", color=rev(labcol[f_stations$Name]),size=18))  

#pl.mon
ggsave(paste0("graphs/test.png"), 
       plot = pl.mon, width = 17, height = 25, dpi = 350, scale = 0.75)

df.rect$xmax <- df.rect$xmax + 2

pl.ann <- ggplot() + 
  geom_tile(data = df2, aes(x=dmon, y=sname, fill=miss,width = tilew, height = tileh),
            alpha = 0.3,color = "black",lwd =0.4,linetype = 2) + 
  #scale_fill_jcolors_contin("pal12",bias = 3) +
  scale_fill_gradientn(colours = terrain.colors(7), limits=c(0,100)) +
  scale_x_discrete(labels=2007:2020) +
  scale_y_discrete(limits=rev) +
  coord_cartesian(clip = 'off') +  
  theme_bw() + 
  #coord_fixed(ratio=0.55) +
  xlab("\nYears (-)") + ylab("\n") + 
  geom_tile(data = df2[(as.numeric(df2$dmon) %in% MON_MISS),], aes(x=dmon, y=sname, fill=miss),
            alpha = 0.7,color = "black",lwd = 0.4,linetype = 1.5) + 
  geom_rect(data = df.rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                width = tilew, height = tileh), 
            fill = NA, alpha = 1, color = "blue", lwd = 1.5) +  
  annotate("text", x = 15, y = 1:49, hjust=0, label = rev(ylabs), size = 4.5, colour = "maroon", fontface = "bold.italic") +
  #scale_y_discrete("lat", limits=rev) +
  labs(title=paste0("(b) Annual gaps\n")) +
  guides(fill = guide_colourbar(barwidth = 2,barheight = 40, title = "Missing \n data (%)\n")) +
  theme(legend.background=element_blank(),
        plot.title = element_text(face = "bold", size = 30, color = "black"),
        #legend.box.spacing = unit(5, "pt"),legend.margin=margin(0,0,0,5),        
        plot.margin = unit(c(1,10,1,1), "lines"),            
        axis.ticks = element_line(linewidth=1.5),
        axis.ticks.length = unit(0.2, "cm"),        
        axis.line = element_line(color = "black", linewidth = 0.8, linetype = "solid"),
        legend.title = element_text(face="bold", color="black",size=20),
        legend.text = element_text(face="bold", color="black",size=20),
        legend.box.background = element_rect(fill="white"),
        legend.spacing.x = unit(50, 'in'),
        legend.box.margin=margin(1,1,1,150),
        axis.title.x = element_text(color="black", size=26, face="bold"),
        axis.title.y = element_text(color="black", size=26, face="bold"),
        axis.text.x = element_text(face="bold", color="#993333",size=18, angle = 90,hjust=1,vjust=0.5),
        axis.text.y = element_text(face="bold", color=rev(labcol[f_stations$Name]),size=18))

#pl.ann
#ggsave(paste0("graphs/test_ann.png"),plot = pl.ann, width = 20, height = 25, dpi = 350, scale = 0.75)

#grob <- pl.mon + pl.ann & theme() plot_layout(guides = "collect", widths = c(1,2))
grob <- ggarrange(pl.mon, pl.ann, nrow = 1, ncol = 2, common.legend = T, widths = c(1,1.4), legend = "right")

ggsave(paste0("graphs/miss_val_",MIN_MON_MISS,"_mon.png"), 
       plot = grob, width = 35, height = 30, dpi = 350, scale = 0.7, bg = "white")

print("Finished.")

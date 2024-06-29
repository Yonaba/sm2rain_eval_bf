Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

library(ggpattern)
library(ggplot2)
library(ggpubr)
source("0_lib_import.R")
N_YEARS <- 2020-2007+1
MIN_TH <- 0.1

products <- c("OBS","CCI", "GPM", "ASCAT","ARC2", "RFE2", "TAMSAT", "CHIRPS")
stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
rownames(stations) <- stations$Code
cols <- c("black","#e6194b","#9A6324","#42d4f4","#469990","#f58231","#ffe119","#bfef45") #distinctColorPalette(7)

pr <- import.rainfall(products)
ldaily <- as.data.frame(flatten(pr, "ldaily", products, stations))
ldaily <-  round(ldaily, 1)

dates <- rep(x=seq(from=as.Date("2007-01-01"), to = as.Date("2020-12-31"), by="day"), times=nrow(stations))
dates <- as.numeric(format(dates, "%m", tz = "UTC"))
ldaily <- data.frame(mon = dates, ldaily)

bias.df <- data.frame(matrix(nrow=0, ncol=5))
colnames(bias.df) <- c("product","bias type","mon","sys", "rnd")

for (p in products[-1]) {
  #p <- "CCI"
  print(paste0("Decomposing biases: ",p))
  ffdiv <- ifelse(p == "CCI", 9*18,ifelse(p == "GPM", 12*18,14*18))
  #ffdiv <- 18
  
  df <- ldaily[,c("mon","OBS",p)]
  df <- df[complete.cases(df),]
  df <- df[((df[,"OBS"] >= MIN_TH) & (df[,p] >= MIN_TH) & (df[,p] != df[,"OBS"])),]
  
  odf <- df[(df[,p]>df[,"OBS"]),]
  udf <- df[(df[,p]<df[,"OBS"]),]
  
  mons <- unique(df$mon)
  mons <- mons[mons %in% 6:10]
  for (mon in mons) {
    print(paste0("Mon ",mon))
    
    tmp.odf <- odf[odf$mon==mon,]
    tmp.udf <- odf[udf$mon==mon,]
    
    fm <- paste0("log(",p,")~log(OBS)")
    lmover <- lm(as.formula(fm), data = tmp.odf)
    lmunder<- lm(as.formula(fm), data = tmp.udf)
    
    o.alpha <- as.numeric(lmover$coefficients[1])
    u.alpha <- as.numeric(lmunder$coefficients[1])
    
    o.beta <- as.numeric(lmover$coefficients[2])
    u.beta <- as.numeric(lmunder$coefficients[2])
    
    o.Rlin <- exp(as.numeric(predict(lmover, data.frame(OBS = tmp.odf$OBS))))
    u.Rlin <- exp(as.numeric(predict(lmunder, data.frame(OBS = tmp.udf$OBS))))
    
    osys <- mean(log(o.Rlin/tmp.odf$OBS)^2, na.rm=T)
    usys <- mean(log(u.Rlin/tmp.udf$OBS)^2, na.rm=T)
    ornd <- mean(log(tmp.odf[,p]/o.Rlin)^2, na.rm=T)
    urnd <- mean(log(tmp.udf[,p]/u.Rlin)^2, na.rm=T)
    
    otot <- osys + ornd
    utot <- usys + urnd
    
    osys <- fmt((osys/otot)*100,1)
    ornd <- fmt((ornd/otot)*100,1)
    
    usys <- fmt(-(usys/utot)*100,1)
    urnd <- fmt(-(urnd/utot)*100,1)
    
    bias.df[nrow(bias.df)+1,] <- c(p,"Over Hit Bias (OHB)", mon, osys, ornd)
    bias.df[nrow(bias.df)+1,] <- c(p,"Under Hit Bias (UHB)", mon, usys, urnd)   
  }
}
 
ldaily <- as.data.frame(flatten(pr, "ldaily", products, stations))
ldaily <-  round(ldaily, 1)

dates <- rep(x=seq(from=as.Date("2007-01-01"), to = as.Date("2020-12-31"), by="day"), times=nrow(stations))
dates <- as.numeric(format(dates, "%Y", tz = "UTC"))
ldaily <- data.frame(year = dates, ldaily) 

abias.df <- data.frame(matrix(nrow=0, ncol=5))
colnames(abias.df) <- c("product","bias type","year","sys", "rnd")

for (p in products[-1]) {
  #p <- "CCI"
  print(paste0("Decomposing biases: ",p))
  ffdiv <- ifelse(p == "CCI", 9*18,ifelse(p == "GPM", 12*18,14*18))
  #ffdiv <- 18
  
  df <- ldaily[,c("year","OBS",p)]
  df <- df[complete.cases(df),]
  df <- df[((df[,"OBS"] >= MIN_TH) & (df[,p] >= MIN_TH) & (df[,p] != df[,"OBS"])),]
  
  odf <- df[(df[,p]>df[,"OBS"]),]
  udf <- df[(df[,p]<df[,"OBS"]),]
  
  years <- unique(df$year)
  years <- years[years %in% 2007:2020]
  for (year in years) {
    #year <- 2007
    print(paste0("Year ",year))
    
    tmp.odf <- odf[odf$year==year,]
    tmp.udf <- odf[udf$year==year,]
    
    fm <- paste0("log(",p,")~log(OBS)")
    lmover <- lm(as.formula(fm), data = tmp.odf)
    lmunder<- lm(as.formula(fm), data = tmp.udf)
    
    o.alpha <- as.numeric(lmover$coefficients[1])
    u.alpha <- as.numeric(lmunder$coefficients[1])
    
    o.beta <- as.numeric(lmover$coefficients[2])
    u.beta <- as.numeric(lmunder$coefficients[2])
    
    o.Rlin <- exp(as.numeric(predict(lmover, data.frame(OBS = tmp.odf$OBS))))
    u.Rlin <- exp(as.numeric(predict(lmunder, data.frame(OBS = tmp.udf$OBS))))
    
    osys <- mean(log(o.Rlin/tmp.odf$OBS)^2, na.rm=T)
    usys <- mean(log(u.Rlin/tmp.udf$OBS)^2, na.rm=T)
    ornd <- mean(log(tmp.odf[,p]/o.Rlin)^2, na.rm=T)
    urnd <- mean(log(tmp.udf[,p]/u.Rlin)^2, na.rm=T)
    
    otot <- osys + ornd
    utot <- usys + urnd
    
    osys <- fmt((osys/otot)*100,1)
    ornd <- fmt((ornd/otot)*100,1)
    
    usys <- fmt(-(usys/utot)*100,1)
    urnd <- fmt(-(urnd/utot)*100,1)
    
    abias.df[nrow(abias.df)+1,] <- c(p,"Over Hit Bias (OHB)", year, osys, ornd)
    abias.df[nrow(abias.df)+1,] <- c(p,"Under Hit Bias (UHB)", year, usys, urnd)   
  }
}

bias.df$mon <- as.numeric(bias.df$mon)
abias.df$year <- as.numeric(abias.df$year)
bias.df$sys <- as.numeric(bias.df$sys)
bias.df$rnd <- as.numeric(bias.df$rnd)
abias.df$sys <- as.numeric(abias.df$sys)
abias.df$rnd <- as.numeric(abias.df$rnd)

bias.df$product <- ordered(bias.df$product, levels = products)
abias.df$product <- ordered(abias.df$product, levels = products)

cp <- 0
plist <- list()
for (p in products[-1]) {
  print(paste0("Drawing: ", p))
  #p <- products[2]
  cp <- cp + 1
  df <- bias.df[bias.df$product == p,]
  sys <- df[,c("product","bias type", "mon", "sys")]
  rnd <- df[,c("product","bias type", "mon", "rnd")]
  colnames(sys)[4] <- colnames(rnd)[4] <- "value"
  sys$err <- "sys"
  rnd$err <- "rnd"
  df <- rbind(sys, rnd)
  plist[[length(plist)+1]] <- ggplot(df,aes(x = mon, y = value, fill = factor(interaction(`bias type`, err)))) +
    geom_area() + ylab(ifelse((cp %in% c(1,4)),"Percentage (%)","")) + xlab(ifelse(cp>=4,"Months\n","\n")) +
    scale_x_continuous(breaks = 6:10, labels=month.abb[6:10]) +
    scale_fill_manual(name = "Hit bias decomposition (%)", 
                      values = c("blue3","red3", "#95D0FC", "orange"),
                      labels=c("Over Hit Bias (OHB) - Random", "Under Hit Bias (UHB) - Random",
                               "Over Hit Bias (OHB) - Systematic", "Under Hit Bias (UHB) - Systematic")) +
    labs(title = paste0("(",letters[cp],") ",p)) + 
    theme_bw() +
    theme(plot.title = element_text(color = "black", size = 20, face = "bold"),
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),        
          axis.title.x = element_text(size = 14, margin = margin(t = 10)),         
          axis.text = element_text(color = "black",size = 14),
          #axis.text.x = element_text(angle = 90),
          #legend.key.size = unit(1.5,"cm"),
          legend.position = "bottom",
          legend.title = element_text(size = 14, color = "black"),
          legend.text = element_text(size = 14, color = "black")) +
    guides(fill = guide_legend(nrow = 2))
  if (cp == 3) plist[[length(plist)+1]] <- ggplot() + theme_void()  
}

grob.bias <- ggarrange(plotlist = plist, nrow = 2, ncol = 4, common.legend = T, legend = "bottom")
ggsave(plot = grob.bias, "graphs/bias_decomp_months.png",
       width = 24, height = 14, units = "in", dpi = 400, scale = 0.7,bg="white")

cp <- 0
plist <- list()
for (p in products[-1]) {
  #p <- products[2]
  print(paste0("Drawing: ", p))
  cp <- cp + 1
  df <- abias.df[abias.df$product == p,]
  sys <- df[,c("product","bias type", "year", "sys")]
  rnd <- df[,c("product","bias type", "year", "rnd")]
  colnames(sys)[4] <- colnames(rnd)[4] <- "value"
  sys$err <- "sys"
  rnd$err <- "rnd"
  df <- rbind(sys, rnd)
  plist[[length(plist)+1]] <- ggplot(df,aes(x = year, y = value, fill = factor(interaction(`bias type`, err)))) +
    geom_area() + ylab(ifelse((cp %in% c(1,4)),"Percentage (%)","")) + xlab(ifelse(cp>=4,"Years\n","\n")) +
    scale_x_continuous(breaks = 2007:2020, labels=2007:2020) +
    scale_fill_manual(name = "Hit bias decomposition (%)", 
                      values = c("blue3","red3", "#95D0FC", "orange"),
                      labels=c("Over Hit Bias (OHB) - Random", "Under Hit Bias (UHB) - Random",
                               "Over Hit Bias (OHB) - Systematic", "Under Hit Bias (UHB) - Systematic")) +
    labs(title = paste0("(",letters[cp],") ",p)) + 
    theme_bw() +
    theme(plot.title = element_text(color = "black", size = 20, face = "bold"),
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),        
          axis.title.x = element_text(size = 14, margin = margin(t = 10)),         
          axis.text = element_text(color = "black",size = 14),
          axis.text.x = element_text(angle = 90),
          #legend.key.size = unit(1.5,"cm"),
          legend.position = "bottom",
          legend.title = element_text(size = 14, color = "black"),
          legend.text = element_text(size = 14, color = "black")) +
    guides(fill = guide_legend(nrow = 2))
  if (cp == 3) plist[[length(plist)+1]] <- ggplot() + theme_void()  
}

grob.abias <- ggarrange(plotlist = plist, nrow = 2, ncol = 4, common.legend = T, legend = "bottom")
ggsave(plot = grob.abias, "graphs/bias_decomp_ann.png",
       width = 30, height = 14, units = "in", dpi = 400, scale = 0.7,bg="white")

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

bias.df <- data.frame(matrix(nrow=0, ncol=4))
colnames(bias.df) <- c("product","bias type","average", "volume")

for (p in products[-1]) {
  #p <- "CCI"
  print(paste0("Processing biases: ",p))
  ffdiv <- ifelse(p == "CCI", 9*18,ifelse(p == "GPM", 12*18,14*18))
  #ffdiv <- 18
  
  df <- ldaily[,c("OBS",p)]
  df <- df[((df[,"OBS"] >= MIN_TH) & (df[,p] >= MIN_TH) & (df[,p] != df[,"OBS"])),]
  hbias <- sum(abs(df[,p] - df[,"OBS"]), na.rm=T)
  bias.df[nrow(bias.df)+1,] <- c(p,"Hit Bias",hbias/nrow(df), hbias/ffdiv)
  
  odf <- df[(df[,p] > df[,"OBS"]),]
  udf <- df[(df[,p] < df[,"OBS"]),]
  
  ohbias <- sum(odf[,p] - odf[,"OBS"], na.rm=T)
  uhbias <- sum(udf[,"OBS"] - udf[,p], na.rm=T)
  bias.df[nrow(bias.df)+1,] <- c(p,"Over Hit Bias",ohbias/nrow(odf), ohbias/ffdiv)
  bias.df[nrow(bias.df)+1,] <- c(p,"Under Hit Bias",uhbias/nrow(udf), uhbias/ffdiv)
  
  df <- ldaily[,c("OBS",p)]
  df <- df[((df[,"OBS"] >= MIN_TH) & (df[,p] < MIN_TH)),]
  mbias <- sum(abs(df[,p] - df[,"OBS"]), na.rm=T)
  bias.df[nrow(bias.df)+1,] <- c(p,"Miss Bias",mbias/nrow(df), mbias/ffdiv)
  
  df <- ldaily[,c("OBS",p)]
  df <- df[((df[,"OBS"] < MIN_TH) & (df[,p] >= MIN_TH)),]
  fabias <- sum(df[,p] - df[,"OBS"], na.rm=T)
  bias.df[nrow(bias.df)+1,] <- c(p,"False Bias",fabias/nrow(df), fabias/ffdiv)
  
  df <- ldaily[,c("OBS",p)]
  tbias <- abs(hbias) + abs(mbias) + abs(fabias)
  bias.df[nrow(bias.df)+1,] <- c(p,"Total Bias",tbias/nrow(df), tbias/ffdiv)
  
}

btypes <- unique(bias.df$`bias type`)

acro <- c("Hit Bias" = "(HB)",
          "Over Hit Bias" = "(OHB)",          
          "Under Hit Bias" = "(-UHB)",
          "Miss Bias" = "(-MB)",
          "False Bias" = "(FB)",
          "Total Bias" = "(TB)")

bias.df$product <- ordered(bias.df$product, levels = products)
bias.df$`bias type` <- ordered(bias.df$`bias type`, levels = btypes)
bias.df$average <- round(as.numeric(bias.df$average),1)
bias.df$volume <- round(as.numeric(bias.df$volume),1)

labels_top <- paste0("(",letters[1:length(btypes)],") Average annual ",btypes)
names(labels_top) <- btypes

ctheme <- theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
                axis.title.x = element_text(size = 14, margin = margin(t = 10)),  
                axis.title.y = element_text(size = 14, margin = margin(r = 10), colour = "blue"),  
                axis.title.y.right = element_text(size = 14, margin = margin(l = 10), colour = "red"),
                axis.line.y = element_line(colour = "blue"),
                axis.line.y.right = element_line(colour = "red"),
                axis.text = element_text(color = "black", size = 14),
                axis.text.x = element_text(angle = 90, vjust = 0.5),
                axis.text.y = element_text(colour = "blue"),
                axis.text.y.right = element_text(colour = "red"),
                axis.ticks.y.left = element_line(colour = "blue"),
                axis.ticks.y.right = element_line(colour = "red"),
                legend.position = "bottom", legend.title = element_blank(),
                #legend.key.size = unit(1.5,"cm"),
                legend.text = element_text(size = 14, color = "black"))

cp <- 0
plist <- list()
coeff <- 70
for (btype in btypes) {
  #btype <- btypes[1]
  cp <- cp + 1
  ddf <- bias.df[bias.df$`bias type`==btype,]
  #ddf <- data.frame(product = rep(ddf$product,2), stack(ddf[,c("average","volume")]))
  plist[[cp]] <- ggplot(data = ddf, aes(x=product)) +
    geom_bar(aes(y = average),fill = "blue", width = 0.4, stat = "identity", just = 1, colour = "black", size = 0.5) +
    geom_bar(aes(y = volume/coeff),fill = "red", width = 0.4, stat = "identity", just = 0, colour = "black", size = 0.5) +
    xlab("") +
    scale_y_continuous(
      name=ifelse((cp %in% c(1,4)),"Avg. event bias [mm]",""), limits=c(0,15), breaks = seq(0,15,5), labels = seq(0,15,5),
      sec.axis = sec_axis(~.*coeff,name=ifelse((cp %in% c(3,6))," Avg. annual bias [mm]",""))
    ) +
    labs(title = bquote(bold("("*.(letters[cp])*")"~.(btype)~bolditalic(.(acro[btype]))))) +
    theme_bw() + ctheme +
    guides(fill = guide_legend(nrow = 1))
}

grob <- ggarrange(plotlist = plist, nrow = 2, ncol = 3, common.legend = T, legend = "bottom")
grob

ggsave(paste0("graphs/share_biases.png"), plot = grob, 
       width = 22, height = 12, dpi = 400, scale = 0.75, bg = "white") 


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

pr <- import.rainfall(products)$ldekad
df <- average_product(pr,1,7)

cp <- 0
plist <- list()

for (p in products[-1]) {
  cp <- cp + 1
  #p <- "CCI"
  print(paste0("Processing: ",p))
  cdf <- df[,c("Date","OBS",p)]
  gofs <- gof(cdf[,p], cdf[,"OBS"])
  gofs <- gofs[c("R2","RMSE","PBIAS %", "KGE"),]
  gofs.lab <- paste0(#"R² = ",format(round(gofs[1],2), nsmall=2),
                     "RMSE = ", format(round(gofs[2],1), nsmall=1)," [mm/dekad]",
                     " - PBIAS = ", format(round(gofs[3],1), nsmall=1),"%",
                     " - KGE = ", format(round(gofs[4],2), nsmall=2))
  #cdf$Date <- ordered(cdf$Date, levels=cdf$Date)
  dplot <- ggplot(cdf, aes(x=Date, group=1)) +
    geom_line(aes(y = !!sym("OBS")),color="black",linewidth = 0.5, linetype = "solid") + 
    geom_line(aes(y = !!sym(p)),color=cols[cp+1],linewidth = 0.5, linetype = "solid") +
    xlab(ifelse(cp==7,"Dekadal dates","")) + ylab("Dekadal rainfall [mm]") + ylim(0,150) +
    scale_x_discrete(breaks = cdf$Date[seq(1, length(cdf$Date),12)], 
                     labels = cdf$Date[seq(1, length(cdf$Date),12)]) +
    labs(title = paste0("(",letters[cp],") ",p),subtitle = gofs.lab) +
    theme_bw() + 
    theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
          plot.subtitle = element_text(color = "black",, size = 12, hjust = 0, face = "bold.italic"),
          axis.title.y = element_text(size = 14, margin = margin(r = 10)),        
          axis.title.x = element_text(size = 14, margin = margin(t = 10)),         
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(angle = 90,size = 10),
          legend.position = "bottom", legend.title = element_blank(),
          #legend.key.size = unit(1.5,"cm"),
          legend.text = element_text(size = 14, color = "black"))
  
  # trace(ggpubr:::.stat_lm, edit = TRUE)
  # untrace(ggpubr:::.stat_lm)
  splot <- ggscatter(cdf, x = "OBS", y = p, size = 2, add = "reg.line", color = "black", shape = 21,
    #title = ""
    add.params = list(linetype = "dashed", color = cols[cp+1]), 
    conf.int = T, conf.int.level = 0.9, cor.method = "pearson",legend = "bottom") + 
    theme_bw() +
    theme(axis.text=element_text(colour="black", size = 14),
          axis.title=element_text(colour="black", size = 14)) +
    xlab(ifelse(cp==7,"Observed [mm/dekad]","")) + ylab("Product [mm/dekad]") +
    xlim(0,130) + ylim(0,130) +
    #stat_regline_equation(formula=y~x+0, label.y = 120,size = 12, na.rm = T) +
    stat_cor(method = "pearson", aes(label = paste(after_stat(r.label),format_pval(..p..), sep="~`,`~")), 
             label.y = 120, size = 5)
  
  plist[[length(plist)+1]] <- dplot
  plist[[length(plist)+1]] <- splot
}

grob <- ggarrange(plotlist = plist, nrow = length(products)-1, ncol = 2, widths = c(1,0.5))
#grob

ggsave(plot = grob, "graphs/ts_scatter_dekadal.png",
       width = 30, height = 65, units = "cm", dpi = 400, scale = 1,bg="white")
print("finished.")

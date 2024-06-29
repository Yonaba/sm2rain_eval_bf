Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

library(verification)
library(ggplot2)
library(ggpubr)
source("0_lib_import.R")

ths <- c(0.1,5,10, 15, 20, 25, 30)
products <- c("OBS","CCI", "GPM", "ASCAT","ARC2", "RFE2", "TAMSAT", "CHIRPS")
stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
rownames(stations) <- stations$Code
cols <- c("black","#e6194b","#9A6324","#42d4f4","#469990","#f58231","#ffe119","#bfef45") #distinctColorPalette(7)

pr <- import.rainfall(products)$ldaily

cdf <- data.frame(matrix(nrow=nrow(pr$OBS) * (ncol(pr$OBS)-1), ncol=length(products)))
colnames(cdf) <- products
for (product in names(pr)) {
  df <- pr[[product]]
  df$Date <- NULL
  df[df<min(ths)] <- 0
  df <- stack(df)$values
  cdf[,product] <- df
}

cnames <- c("POD", "FAR", "TS")
scnames <- c("model", "values", "ind", "th")
scores <- data.frame(matrix(nrow=0, ncol=length(scnames)))
colnames(scores) <- scnames

for (th in ths) {
  #th <- ths[1]
  print(paste0("Processing threshold: ",th," mm"))
  robs <- (cdf$OBS >= th)
  
  pscores <- data.frame(matrix(nrow=0, ncol=length(cnames)))
  colnames(pscores) <- cnames  
  for (product in colnames(cdf)[-1]) {
    #product <- "CCI"
    rsim <- (cdf[,product] >= th)
    ss <- table.stats(robs, rsim, fudge = 0.01, silent = FALSE) 
    vals <- c(ss$POD, ss$FAR, ss$TS)
    pscores[nrow(pscores)+1,] <- vals
  }
  
  pscores <- data.frame(model = rep(products[-1],length(cnames)),stack(pscores))
  pscores$th <- as.character(th)
  scores <- rbind(scores, pscores)
}


scores$model <- ordered(scores$model, levels = products[-1])
scores$ind <- ordered(scores$ind, levels = cnames)
scores$th <- as.numeric(scores$th)
scores$ltyp <- ifelse((scores$model %in% c("CCI","GPM","ASCAT")),"SM2RAIN","SPP")

labels_top <- paste0("(",letters[1:3],") ",cnames)
names(labels_top) <- cnames

plot <- ggplot(scores, aes(x=th, y=values, colour = model, linetype = ltyp)) +
  geom_line(linewidth = 1) + geom_point(shape=16,size=2) +
  facet_wrap(. ~ ind, labeller = as_labeller(labels_top)) +
  xlab("Rainfall thresholds") + ylab("Score\n") +
  scale_colour_manual(values = cols[-1], labels = as.character(products[-1])) +
  scale_linetype_manual(values=c("twodash", "solid")) +
  scale_x_continuous(breaks=ths, labels=ths) +
  labs(linetype = "Source type", colour = "Products") + 
  theme_bw() +
  theme(strip.text.x = element_text(size = 16, colour = "black", face = "bold", hjust = 0),
        axis.title.y = element_text(color = "black",size = 14, margin = margin(r = 10)),        
        axis.title.x = element_text(color = "black",size = 14, margin = margin(t = 10)),
        axis.text=element_text(colour="black", size = 14),
        legend.position = "bottom",
        legend.title = element_text(colour = "black",size=14, face = "italic"), 
        legend.text = element_text(colour = "black",size=14)) +
  guides(linetype = guide_legend(nrow = 1)) +
  guides(colour = guide_legend(nrow = 1))


ggsave("graphs/skill_score_products.png", plot = plot, 
       width = 17, height = 7, units = "in", dpi = 400, scale = 0.8)
print("finished.")

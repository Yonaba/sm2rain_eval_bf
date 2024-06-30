Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

library(climdex.pcic)
library(ggplot2)
library(ggpubr)
PMIN <- 0.1
source("0_lib_import.R")

products <- c("OBS","CCI", "GPM", "ASCAT","ARC2", "RFE2", "TAMSAT", "CHIRPS")
stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
rownames(stations) <- stations$Code
cols <- c("black","#e6194b","#9A6324","#42d4f4","#469990","#f58231","#ffe119","#bfef45") #distinctColorPalette(7)

pr <- import.rainfall(products)$ldaily

etccdi <- data.frame(matrix(nrow=0, ncol=4))
colnames(etccdi) <- c("product", "station", "ind", "values")

for (p in products) {
  #p <- "OBS"
  print(paste0("Processing :",p))
  df <- pr[[p]]
  df[,2:ncol(df)][df[,2:ncol(df)]<PMIN] <- 0
  ddf <- df
  ddf[!(as.numeric(format(ddf$Date, "%m")) %in% 5:10),2:ncol(ddf)] <- 1000
  for (s in stations$Name) {
    #s <- "ARIBINDA"
    print(paste0("      :",s))
    ci.ddf <- climdexInput.raw(prec = ddf[,s], prec.dates = as.PCICt(ddf$Date, cal="gregorian"), 
                              base.range = c(2007,2020), n = 14, northern.hemisphere = T,
                              prec.qtiles = c(0.9, 0.95, 0.99))
    
    ci.df <- climdexInput.raw(prec = df[,s], prec.dates = as.PCICt(df$Date, cal="gregorian"), 
                               base.range = c(2007,2020), n = 14, northern.hemisphere = T,
                               prec.qtiles = c(0.9, 0.95, 0.99))
    
    vals <- climdex.rnnmm(ci.df, PMIN)
    etccdi <- rbind(etccdi, data.frame(product = p, station = s, ind = "nP", values = vals)) 
    
    vals <- as.numeric(climdex.cdd(ci.ddf, spells.can.span.years = F))
    etccdi <- rbind(etccdi, data.frame(product = p, station = s, ind = "CDD", values = vals))
    
    vals <- as.numeric(climdex.cwd(ci.df, spells.can.span.years = F))
    etccdi <- rbind(etccdi, data.frame(product = p, station = s, ind = "CWD", values = vals))
    
    vals <- climdex.sdii(ci.df)
    etccdi <- rbind(etccdi, data.frame(product = p, station = s, ind = "SDII", values = vals))
    
    vals <- climdex.r10mm(ci.df)
    etccdi <- rbind(etccdi, data.frame(product = p, station = s, ind = "R10mm", values = vals)) 
    
    vals <- climdex.r20mm(ci.df)
    etccdi <- rbind(etccdi, data.frame(product = p, station = s, ind = "R20mm", values = vals)) 
    
    vals <- climdex.rx1day(ci.df,freq = "annual")
    etccdi <- rbind(etccdi, data.frame(product = p, station = s, ind = "RX1DAY", values = vals))
    
    vals <- climdex.rx5day(ci.df,freq = "annual")
    etccdi <- rbind(etccdi, data.frame(product = p, station = s, ind = "RX5DAY", values = vals)) 
    
    etccdi <- rbind(etccdi,
                    data.frame(product = p, station = s, ind = "p90", values = ci.df@quantiles[["prec"]][["q90"]]),
                    data.frame(product = p, station = s, ind = "p95", values = ci.df@quantiles[["prec"]][["q95"]]))
    
    vals <- climdex.r95ptot(ci.df)
    etccdi <- rbind(etccdi, data.frame(product = p, station = s, ind = "R95pTOT", values = vals))
    
    vals <- climdex.r99ptot(ci.df)
    etccdi <- rbind(etccdi, data.frame(product = p, station = s, ind = "R99pTOT", values = vals)) 
    
  }
}

ctheme.plot <-  theme(plot.title = element_text(color = "black", size = 20, face = "bold"),
                      axis.title.y = element_text(size = 14, margin = margin(r = 15)),  
                      
                      axis.text = element_text(color = "black", size = 14),
                      axis.text.x = element_text(angle = 90, vjust = 0.5),
                      legend.position = "none")

etccdi$product <- ordered(etccdi$product, levels = products)
inds <- unique(etccdi$ind)
u.inds <- c("-","days", "days", "mm/day", "days", "days", "mm", "mm", "mm", "mm", "mm", "mm")
names(u.inds) <- inds

plist <- list()
cp <- 0
for (ind in inds) {
  #ind <- "CDD"
  cp <- cp + 1
  setccdi <- etccdi[etccdi$ind == ind,]
  ylab <- paste0(ind," [",u.inds[ind],"]")
  if (cp == 4) ylab <- bquote(.(ind) ~ " [mm " * day^-1 *"]")
  plist[[cp]] <- ggplot(setccdi, aes(x=product, y=values, fill = product)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=21,outlier.size=2, outlier.fill = NA) +
    xlab("") + ylab(ylab) + 
    labs(title = paste0("(",letters[cp],") ",ind)) +
    scale_fill_manual(values = cols) + theme_bw() + ctheme.plot
}

grob <- ggarrange(plotlist = plist, nrow = 4, ncol = 3)

ggsave(plot = grob, "graphs/etccdi.png",
       width = 20, height = 20, units = "in", dpi = 400, scale = 0.8,bg="white")



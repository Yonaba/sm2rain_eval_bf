Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

source("0_lib_import.R")
library(hydroGOF)
library(randomcoloR)
library(ggplot2)
library(ggpubr)

products <- c("OBS","CCI", "GPM", "ASCAT","ARC2", "RFE2", "TAMSAT", "CHIRPS")
stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
rownames(stations) <- stations$Code
cols <- c("#e6194b","#9A6324","#42d4f4","#469990","#f58231","#ffe119","#bfef45") #distinctColorPalette(7)

pr <- import.rainfall(products)

df <- data.frame(matrix(nrow=0, ncol=4))
colnames(df) <- c("timescale","product","metric", "value")

for (timescale in names(pr)) {
  #timescale <- "dev.off"  
  print(paste0("Processing: ", timescale))
  for (product in products[-1]) {
    #timescale <- "lannual"
    #product <- "CCI"
    print(paste0("    Evaluating: ", product))
    dobs <- pr[[timescale]][["OBS"]]
    dsim <- pr[[timescale]][[product]]
    dobs$Date <- dsim$Date <- NULL
    
    dr <- rPearson(dsim, dobs, na.rm = T)
    drmse <- sqrt(mse(dsim, dobs, na.rm = T))
    dpbias <- pbias(dsim, dobs, na.rm = T)
    dkge <- as.numeric(KGE(dsim, dobs, method = "2012", na.rm = T))
    
    n <- length(dr)
    
    r.df <- cbind(rep(timescale, n), rep(product, n), rep("r", n), dr)
    rmse.df <- cbind(rep(timescale, n), rep(product, n), rep("rmse", n), drmse)
    pbias.df <- cbind(rep(timescale, n), rep(product, n), rep("pbias", n), dpbias)
    kge.df <- cbind(rep(timescale, n), rep(product, n), rep("kge", n), dkge)
    
    colnames(r.df) <-  colnames(rmse.df) <- colnames(pbias.df) <- 
      colnames(kge.df) <- colnames(df)
    
    df <- rbind(df, r.df, rmse.df, pbias.df, kge.df)
  }
}

df$product <- ordered(df$product, levels=products[-1])
df$metric <- ordered(df$metric, levels=c("r","rmse","pbias","kge"))
df$value <- as.numeric(df$value)

rmse.units <- c("ldaily" = "day",
               "ldekad" = "dekad",
               "lmonthly" = "month",
               "lannual" = "yr")

rmse.lims <- list("ldaily" = c(0,10),
               "ldekad" = c(0,30),
               "lmonthly" = c(0,70),
               "lannual" = c(0,500))

ctheme.plot <-  theme(plot.title = element_text(color = "black", size = 20, face = "bold"),
                      axis.title.y = element_text(size = 14, margin = margin(r = 15)),        
                      axis.text = element_text(color = "black", size = 14),
                      axis.text.x = element_text(angle = 90),
                      legend.position = "none", legend.title = element_blank(),
                      legend.key.size = unit(1.5,"cm"),
                      legend.text = element_text(size = 14, color = "black"))

pp <- 0
for (timescale in unique(df$timescale)) {
  pp <- pp + 1
  print(paste0("Plotting: ",timescale))
  
  sdf <- df[((df$timescale == timescale) & (df$metric == "r")),]
  r.plot <- ggplot(sdf, aes(x=product, y=value, fill = product)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=19,outlier.size=2) +
    xlab("") + ylab("r [-]") + ylim(0,1) +
    labs(title = paste0("(a) r (Pearson)")) +
    scale_fill_manual(values = cols) +
    theme_bw() + ctheme.plot +
    guides(fill = guide_legend(nrow = 1))
  
  
  sdf <- df[((df$timescale == timescale) & (df$metric == "rmse")),]
  rmse.plot <- ggplot(sdf, aes(x=product, y=value, fill = product)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=19,outlier.size=2) +
    xlab("") + 
    ylab(bquote("RMSE [mm." * .(rmse.units[timescale]) ^-1 *"]")) + 
    ylim(rmse.lims[[timescale]]) +
    labs(title = paste0("(b) RMSE")) +
    scale_fill_manual(values = cols) +
    theme_bw() + ctheme.plot +
    guides(fill = guide_legend(nrow = 1))
  
  sdf <- df[((df$timescale == timescale) & (df$metric == "pbias")),]
  pbias.plot <- ggplot(sdf, aes(x=product, y=value, fill = product)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=19,outlier.size=2) +
    xlab("") + ylab("PBIAS [%]") + ylim(-60,40) +
    labs(title = paste0("(c) PBIAS")) +
    scale_fill_manual(values = cols) +
    theme_bw() + ctheme.plot +
    guides(fill = guide_legend(nrow = 1))
  
  sdf <- df[((df$timescale == timescale) & (df$metric == "kge")),]
  kge.plot <- ggplot(sdf, aes(x=product, y=value, fill = product)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=19,outlier.size=2) +
    xlab("") + ylab("KGE [-]") + ylim(0,1) +
    labs(title = paste0("(d) KGE")) +
    scale_fill_manual(values = cols) +
    theme_bw() + ctheme.plot +
    guides(fill = guide_legend(nrow = 1))
  
  grob <- ggarrange(r.plot, rmse.plot, pbias.plot, kge.plot, ncol = 2, nrow = 2,
                    common.legend = T, legend = "bottom")
  #grob
  ggsave(paste0("graphs/",pp,"_",timescale,"_boxp_metrics.png"), plot = grob, 
         width = 20, height = 15, dpi = 400, scale = 0.6, bg = "white")  
}

write.csv(df, file = paste0("tables/products_eval_metrics.csv"), row.names = F)

print("Finished.")
Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

library(ggplot2)
library(ggpubr)
source("0_lib_import.R")
N_YEARS <- 2020-2007+1

cnv_share <- function(df) {
  models <- levels(vol.df$model)
  for (m in models) {
    #m <- "OBS"
    vdf <- df[((df$model==m)),]
    ss <- (vdf$values/sum(vdf$values))*100
    df[((df$model==m)),"values"] <- ss
  }
  df$values <- as.numeric(format(round(df$values, digits = 1), nsmall = 1))
  return (df)
}

products <- c("OBS","CCI", "GPM", "ASCAT","ARC2", "RFE2", "TAMSAT", "CHIRPS")
stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
rownames(stations) <- stations$Code
cols <- c("black","#e6194b","#9A6324","#42d4f4","#469990","#f58231","#ffe119","#bfef45") #distinctColorPalette(7)

pr <- import.rainfall(products)
ldaily <- flatten(pr, "ldaily", products, stations)

ths <- c(1,5,10,20,30,1000)
lab.ths <- c("<=1", "1-5", "5-10", "10-20", "20-30", ">30")

count.df <- eval_ths(ldaily, ths, lab.ths, N_YEARS * nrow(stations), length)
vol.df <- eval_ths(ldaily, ths, lab.ths, N_YEARS * nrow(stations), sum)
count.df$model <- ordered(count.df$model, levels = rev(products))
count.df$ind <- ordered(count.df$ind, levels = lab.ths)
vol.df$model <- ordered(vol.df$model, levels = rev(products))
vol.df$ind <- ordered(vol.df$ind, levels = lab.ths)
vol.df <- cnv_share(vol.df)

cplot <- ggplot(data=count.df, aes(x=ind, y=values, fill=model)) +
  geom_bar(stat="identity") + xlab("Rainfall classes\n") + ylab("Count [-]") +
  ylim(0,300) +
  scale_fill_manual(values=rev(cols)) +
  scale_y_continuous(transform = "identity",
                     breaks = seq(0,300,50), labels = seq(0,300,50)) + 
  labs(title = "(a) Share in annual event count") +
  guides(fill = guide_legend(nrow = 1, reverse = T)) +    
  theme_bw() +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
        axis.title.y = element_text(size = 14, margin = margin(r = 10)),        
        axis.title.x = element_text(size = 14, margin = margin(t = 10)),         
        axis.text = element_text(color = "black", size = 14),
        legend.position = "right", legend.title = element_blank(),
        #legend.key.size = unit(1.5,"cm"),
        legend.text = element_text(size = 14, color = "black"))

#cplot

vplot <- ggplot(data=vol.df, aes(x=ind, y=values, fill=model)) +
  geom_bar(stat="identity") + xlab("Rainfall classes\n") + ylab("Volume [%]") +
  scale_fill_manual(values=rev(cols)) +
  scale_y_continuous(transform = "identity") + 
  labs(title = "(b) Share in annual volume") +
  guides(fill = guide_legend(nrow = 1, reverse = T)) +  
  theme_bw() +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
        axis.title.y = element_text(size = 14, margin = margin(r = 10)),        
        axis.title.x = element_text(size = 14, margin = margin(t = 10)),         
        axis.text = element_text(color = "black", size = 14),
        legend.position = "right", legend.title = element_blank(),
        #legend.key.size = unit(1.5,"cm"),
        legend.text = element_text(size = 14, color = "black"))
#vplot

grob <- ggarrange(cplot, vplot, nrow = 1, ncol = 2, common.legend = T, legend = "bottom")
grob

ggsave(paste0("graphs/cmp_rainfall_count_volume.png"), plot = grob, 
       width = 15, height = 7, dpi = 400, scale = 0.75, bg = "white")  

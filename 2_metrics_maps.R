Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

#source("0_lib_import.R")
library(stringr)
library(ggplot2)
library(ggpubr)
library(stars)
library(sf)
library(ggsn)


products <- c("OBS","CCI", "GPM", "ASCAT","ARC2", "RFE2", "TAMSAT", "CHIRPS")
stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
rownames(stations) <- stations$Code
cols <- c("red","blue2","orange","darkgreen")
cols <- rep(cols, 7)

bfshp <- read_sf("Map/bf.shp")
crs <- st_crs("EPSG:4326")

met <- read.csv("Tables/products_eval_metrics.csv", header = TRUE, sep = ",", dec = ".")
timescales <- unique(met$timescale)
metrics <- unique(met$metric)

ulegend_title <- c("r" = "r", "rmse" = "RMSE","pbias" = "PBIAS", "kge" = "KGE")
legend_title <- c("r" = "r [-]", "rmse" = "RMSE [mm/%s]","pbias" = "PBIAS (%)", "kge" = "KGE [-]")
timescale.label <- c("ldaily" = "day", "ldekad" = "dekad", "lmonthly" = "month", "lannual" = "year")

#timescales <- c("ldaily")

for (timescale in timescales) {
  print(paste0("Processing: ",timescale))
  limits <- data.frame(r = c(0,1),rmse = c(0,10),pbias = c(-60,60),kge = c(0,1))
  breaks <- data.frame(
    r = seq(0,1, length.out = 5),rmse = seq(0,10, length.out = 5),
    pbias = seq(-60,60, length.out = 5),kge = seq(0,1, length.out = 5)
  )
  if (timescale == "ldekad") {
    limits$rmse <- c(0,30)
    breaks$rmse <- seq(0,30, length.out = 5)
  } else if (timescale == "lmonthly") {
    limits$rmse <- c(0,70)
    breaks$rmse <- seq(0,70, length.out = 5)
  } else if (timescale == "lannual") {
    limits$rmse <- c(0,500)
    breaks$rmse <- seq(0,500, length.out = 5)
  }
  
  labels <- breaks
  labels$r <- format(round(labels$r,2), nsmall = 2)
  labels$kge <- format(round(labels$kge,2), nsmall = 2)
  labels$rmse <- format(round(labels$rmse,1), nsmall = 1)
  labels$pbias <- format(round(labels$pbias,0), nsmall = 0)
  
  plist <- list()
  cp <- 0
  
  for (product in products[-1]) {
    print(paste0("    Processing: ",product))    
    for (m in metrics) {
      print(paste0("        Drawing: ",m))      
      cp <- cp + 1
      # timescale <- "ldaily"
      # product <- "GPM"
      # m <- "pbias"
      df <- met[((met$timescale == timescale) & (met$product == product) & (met$metric == m)),]
      sdata <- stations
      sdata$metric <- as.numeric(df$value)
      
      st_as_sf(sdata, crs = "EPSG:4326", coords = c("Longitude", "Latitude")) |> st_transform(crs) -> sp.df
      st_bbox(bfshp) |> st_as_stars(dx = 0.05) |> st_crop(bfshp) -> grd
      
      plist[[cp]] <- ggplot() +
        xlab(ifelse(cp>=25,"Longitude (°)","")) + ylab(ifelse((cp %in% c(1,5,9,13,17,21,25)),"Latitude (°)","")) +
        geom_sf(data = st_cast(bfshp, "MULTILINESTRING")) +
        geom_point(data = sdata, aes(x = Longitude, y = Latitude, colour = metric, size = 1.5)) +
        #geom_sf_text(data=sp.df, aes(label=Name), nudge_y=0.25, size = 2.5) + 
        scale_colour_gradient(low = "red", high = "green",
                              limits = limits[,m], breaks = breaks[,m], labels = labels[,m]) +
        guides(size = "none") +
        labs(title = paste0(product," (",ulegend_title[m],")"),
             colour = ifelse(m == "rmse",sprintf(legend_title[m],timescale.label[timescale]),legend_title[m])) + 
        theme_bw() +
        theme(plot.title = element_text(face = "bold", size = 16),
              axis.text = element_text(color = "black", size = 12),
              axis.title = element_text(color = "black", size = 12),
              axis.title.y = element_text(margin = margin(r = 10)),  
              axis.title.x = element_text(margin = margin(t = 10)),                
              legend.position = ifelse(cp>=25,"bottom","none"),
              legend.key.width = unit(1,"cm")) +
        north(data = sp.df, scale=0.2, symbol = 12, anchor = c(x=2.75,y=15)) +
        scalebar(data = sp.df, anchor = c(x=0.5,y=10),
                 model = "WGS84", dist = 150, dist_unit = "km", transform = T,
                 location = "bottomright",height = 0.05, 
                 border.size = 0.2, st.size = 4, st.dist = 0.075)
      
    }
  }
  
  r.grob <- ggarrange(plotlist = plist[seq(1,25,4)], nrow = 7, ncol = 1, common.legend = T, legend = "bottom")
  rmse.grob <- ggarrange(plotlist = plist[seq(2,26,4)], nrow = 7, ncol = 1, common.legend = T, legend = "bottom")
  pbias.grob <- ggarrange(plotlist = plist[seq(3,27,4)], nrow = 7, ncol = 1, common.legend = T, legend = "bottom")
  kge.grob <- ggarrange(plotlist = plist[seq(4,28,4)], nrow = 7, ncol = 1, common.legend = T, legend = "bottom")
  grob <- ggarrange(r.grob, rmse.grob, pbias.grob, kge.grob, nrow = 1, ncol = 4)
  
  ggsave(paste0("graphs/",timescale,"_map_metrics.png"), plot = grob, 
         width = 25, height = 30, dpi = 400, scale = 0.9, bg = "white") 
}

print("Finished.")
Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

library(tidyverse)
library(sf)
library(gstat)
library(stars)
library(ggsn)
library(ggspatial)
library(ggplot2)
library(ggpubr)
library(viridis)
source("0_lib_import.R")

products <- c("OBS","CCI", "GPM", "ASCAT","ARC2", "RFE2", "TAMSAT", "CHIRPS")
stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
rownames(stations) <- stations$Code
cols <- c("black","#e6194b","#9A6324","#42d4f4","#469990","#f58231","#ffe119","#bfef45") #distinctColorPalette(7)

pr <- import.rainfall(products)$lannual
df <- annual_normals(pr)
stations <- cbind(stations,df[,2:ncol(df)])

for (p in products[-1]) {
  stations[,p] <- stations[,p] - stations[,"OBS"]
}
stations$OBS <- NULL

bfshp <- read_sf("Map/bf.shp")
crs <- st_crs("EPSG:4326")
st_as_sf(stations, crs = "EPSG:4326", coords = c("Longitude", "Latitude")) |>
  st_transform(crs) -> sp.df
st_bbox(bfshp) |> st_as_stars(dx = 0.05) |> st_crop(bfshp) -> grd

interp <- sapply(products[-1],function(x) NULL)
for (i in names(interp)) {
  print(paste0("IDW: ",i))
  interp[[i]] <- idw(as.formula(paste0(i,"~1")), sp.df, grd, idp = 2.5)
}
scalelims <- seq(-500,200,100)

cp <- 0
plist <- list()
for (p in products[-1]) {
  #p<- "OBS"
  cp <- cp + 1
  print(paste0("p ",p))
  print(paste0("cp ",cp))
  plist[[length(plist)+1]] <- ggplot() +
    #ggplot() +
    geom_stars(data = interp[[cp]], aes(fill=var1.pred, x=x,y=y)) + 
    xlab(ifelse(cp>=4,"Longitude (°)","")) + ylab(ifelse((cp %in% c(1,4)),"Latitude (°)","")) +
    geom_sf(data = st_cast(bfshp, "MULTILINESTRING")) +
    #geom_sf(data = sp.df, aes(),size = 2.5) +
    #scale_shape_manual(values=c(1, 16)) +
    #geom_sf_text(data=sp.df, aes(label=stations$Station), nudge_y=0.25, size = 2.5) +
    scale_fill_gradientn(name = bquote("Annual rainfall [mm."*yr^-1*"]"),
                         colors=turbo(100,begin = 0,direction=-1), 
                         na.value=NA,
                         trans = "identity",
                         breaks = scalelims, labels=scalelims,
                         limits = range(scalelims)) +
    labs(title = paste0("(",letters[cp],") ",p), ) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", colour = "black", size = 20),
          axis.text = element_text(color = "black", size = 12),
          axis.title.x = element_text(size = 14, color = "black", margin = margin(t=10)),
          axis.title.y = element_text(size = 14, color = "black", margin = margin(r=10)),
          legend.position = "bottom", 
          legend.title = element_text(size = 14, color = "black", vjust = 0.8),
          legend.key.width = unit(2,"cm"),
          legend.text = element_text(angle=0, size = 12,colour = "black")) +
    north(data = sp.df, scale=0.2, symbol = 12, anchor = c(x=2.75,y=15)) +
    annotation_scale(text_cex = 1, pad_x = unit(5.5,"cm"))
    # scalebar(data = stations, anchor = c(x=0.5,y=10),
    #          model = "WGS84", dist = 150, dist_unit = " km", transform = T,
    #          location = "bottomright",height = 0.05, 
    #          border.size = 0.2, st.size = 4, st.dist = 0.075) 
  if (cp == 3) plist[[length(plist)+1]] <- ggplot() + theme_void()
}

grob <- ggarrange(plotlist = plist, nrow = 2, ncol = 4, common.legend = T, legend = "bottom")
grob

ggsave(plot = grob, "graphs/annual_rainfall_maps.png",
       width = 25, height = 10, units = "in", dpi = 400, scale = 0.8,bg="white")
print("finished.")

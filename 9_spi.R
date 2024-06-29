Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

library(hydroGOF)
library(SPEI)
library(viridis)
library(ggplot2)
library(ggpubr)
library(infotheo)
source("0_lib_import.R")

products <- c("OBS","CCI", "GPM", "ASCAT","ARC2", "RFE2", "TAMSAT", "CHIRPS")
stations <- read.csv("Tables/stations_selected.csv", header = TRUE, sep = ",", dec = ".")
rownames(stations) <- stations$Code
cols <- c("black","#e6194b","#9A6324","#42d4f4","#469990","#f58231","#ffe119","#bfef45") #distinctColorPalette(7)

pr <- import.rainfall(products)$lmonthly
df <- average_product(pr,1,7)
spi.scales <- c(1,3,6,12)

rdf <- data.frame(matrix(nrow=0, ncol=length(products)-1))
rdfd <- data.frame(matrix(nrow=0, ncol=length(products)-1))
#colnames(rdf) <- products

glist <- list()
for (s in spi.scales) {
  #s <- spi.scales[1]
  print(paste0("Processing scale: ",s))
  obs <- spi(df[,"OBS"],s, na.rm=T,verbose=F)$fitted
  obs[!is.finite(obs)] <- NA
  
  cp <- 0
  r2s <- r2sd <- c()
  for (p in products[-1]) {
    cp <- cp + 1
    #p <- products[2]
    sim <- spi(df[,p],s, na.rm=T,verbose=F)$fitted
    sim[!is.finite(sim)] <- NA
    ddf <- data.frame(dates = df$Date, OBS = obs, sim = sim)
    colnames(ddf)[3] <- p
    gofs <- cor.test(sim, obs, na.rm = T, method = "spearman")
    r2sval <- fmt(gofs$estimate,2)
    p2sval <- ifelse(gofs$p.value<0.001,"<0.001",round(gofs$p.value,3))
    
    simd <- sim
    obsd <- obs
    simd[simd<0] <- NA
    obsd[obsd<0] <- NA
    gofsd <- cor.test(simd, obsd, na.rm = T, method = "spearman")
    r2svald <- fmt(gofsd$estimate,2)
    
    r2s <- append(r2s, r2sval)
    r2sd <- append(r2sd, r2svald)
    
    gofs.text <- paste0("r = ",r2sval,", p ",p2sval)
    
    glist[[length(glist)+1]] <- ggplot(ddf, aes(x=dates, group=1)) +
      geom_line(aes(y = !!sym("OBS")),color="black",linewidth = 1, linetype = "solid") + 
      geom_line(aes(y = !!sym(p)),color=cols[cp+1],linewidth = 1, linetype = "solid") +
      xlab("") + ylab(ifelse(cp==1,paste0("SPI ",s,"-month",ifelse(s==1,"","s")),"")) + ylim(-3,3) +
      annotate("text", x=80, y=-2.75, label = gofs.text, size = 6, col = "black", hjust = 0, fontface = "italic") +
      scale_x_discrete(breaks = ddf$dates[seq(1, length(ddf$dates),12)], 
                       labels = ddf$dates[seq(1, length(ddf$dates),12)]) +
      labs(title = paste0(p," (",s,"-month",ifelse(s==1,"","s"),")")) +
      theme_bw() +
      theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
            axis.title.y = element_text(size = 14, margin = margin(r = 10)),        
            axis.title.x = element_text(size = 14, margin = margin(t = 10)),         
            axis.text = element_text(color = "black",size = 14),
            axis.text.x = element_text(angle = 90),
            legend.position = "bottom", legend.title = element_blank(),
            #legend.key.size = unit(1.5,"cm"),
            legend.text = element_text(size = 14, color = "black"))
  }
  rdf[nrow(rdf)+1,] <- r2s
  rdfd[nrow(rdfd)+1,] <- r2sd
}

rdf <- t(rdf)
rdfd <- t(rdfd)

colnames(rdf) <- colnames(rdfd) <- paste0(spi.scales,"-month")
rownames(rdf) <- rownames(rdfd) <- products[-1]

write.csv(rdf, file = paste0("tables/spi_c.csv"), row.names = T)
write.csv(rdfd, file = paste0("tables/spid_c.csv"), row.names = T)

grob <- ggarrange(plotlist = glist, nrow = length(spi.scales), ncol = length(products)-1)
#grob

ggsave(plot = grob, "graphs/spi_cmp.png",
       width = 40, height = 25, units = "in", dpi = 400, scale = 1,bg="white")

print("finished.")

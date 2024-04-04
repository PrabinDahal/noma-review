################################################################################
# Title			: Summarising Noma systematic literature review database
# Data version		: March-2023
# Analysis version	: March-2023
# Task			: Generate maps showing study location
################################################################################
#rm(list=ls())
library(pacman)
pacman::p_load(readxl,tidyverse,ggpubr,purrr,readr,stringr,patchwork,table1,doBy,
               gtools,janitor,ggbreak, RColorBrewer,wesanderson,viridis,forcats,tibble,tidyr,
               dplyr,maps,rworldmap,rworldxtra,naniar,cowplot,maptools,gridExtra,classInt, 
               car,ggpubr,ggalt,plyr,IDPmisc,rms,epitools,textclean,waffle,ggwaffle,cowplot
               )
# Working directory
setwd("C:/IDDO_Other/Noma/Data")

# Read the analysis dataset
dat_lab<-read.csv("analysis_data_20_03_2023.csv")

#===================================================
# summarise study sites
#===================================================
(n_sites <- dat_lab %>%
     dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T)
   ))

#multicentre studies
multicentre <- dat_lab %>% filter(multicentre=="Yes")

#===================================================
# Plot location of all the included study sites
#===================================================
t1 <- dat_lab %>% 
  filter(year >= 2000 & year <=2009)
  
t2 <- dat_lab %>% 
  filter(year >= 2010 & year <=2022)

setwd("C:/IDDO_Other/Noma/Results/out1_maps")

tiff(file="S1_overall_map_June2023.tiff", 
              width=24, 
              height=18, 
              units="cm", 
              pointsize="10", 
              compression = "lzw+p", 
              bg="white", 
              res=600, 
              antialias = "none")
  
  worldmap <- getMap(resolution = "high")
  w_map<- worldmap[which(worldmap$REGION %in% c("Africa","Asia","Australia","North America","South America and the Caribbean","Europe")),]               

plot(w_map, 
     lwd=1.2,
     cex.main=1.5, 
     main= ""
     #col="#F0FFF0"
)
points(dat_lab$lon,dat_lab$lat,pch=21,bg="#2c7fb2", cex=1.2)
points(t1$lon,t1$lat,bg="salmon", cex=1.2, pch=21)
points(t2$lon,t2$lat,bg="seagreen", cex=1.2, pch=21)

legend("bottomright", c("Pre-2000", "2000-2009","2010-2022"), col=c("#2c7fb2", "salmon","seagreen"),
       pch=c(19), horiz=TRUE)

dev.off()

#==============================================================
# Plot location of all the included study sites by time-period
#==============================================================
tiff(file="S1_map_time_and_space_overall.tiff", 
            width=28, height=22, units="cm", 
           pointsize="10", compression = "lzw+p", 
            bg="white", res=600, antialias = "none"
          )

# pre-1900
plot(w_map, 
     lwd=1.2,
     cex.main=1.5, 
     main= "Pre-1900"
		 #col="#F0FFF0"
		)
points(dat_lab[which(dat_lab$cutyear=="Pre-1900"),]$lon,dat_lab[which(dat_lab$cutyear=="Pre-1900"),]$lat,pch=21,
		bg="#2c7fb2", cex=1.2)

# 1900-1940
plot(w_map, 
     lwd=1.2,
     cex.main=1.5, 
     main= "1900 - 1940 "
     #col="#F0FFF0"
  )
points(dat_lab[which(dat_lab$cutyear=="1900-1940"),]$lon,dat_lab[which(dat_lab$cutyear=="1900-1940"),]$lat,pch=21,
		bg="#2c7fb2", cex=1.2)

# 1941-1999
plot(w_map, 
     lwd=1.2,
     cex.main=1.5, 
     main= "1941 - 1999 "
     #col="#F0FFF0"
  )
points(dat_lab[which(dat_lab$cutyear=="1941-1999"),]$lon,dat_lab[which(dat_lab$cutyear=="1941-1999"),]$lat,pch=21,
		bg="#2c7fb2", cex=1.2)

# 2000-2009
plot(w_map, 
     lwd=1.2,
     cex.main=1.5, 
     main= "2000 - 2009 "
     #col="#F0FFF0"
)
points(dat_lab[which(dat_lab$cutyear=="2000-2009"),]$lon,dat_lab[which(dat_lab$cutyear=="2000-2009"),]$lat,pch=21,
		bg="#2c7fb2", cex=1.2)

# 2010+
plot(w_map, 
     lwd=1.2,
     cex.main=1.5, 
     main= "2010+ "
     #col="#F0FFF0"
  )
points(dat_lab[which(dat_lab$cutyear=="Post-2010"),]$lon,dat_lab[which(dat_lab$cutyear=="Post-2010"),]$lat,pch=21,
		bg="#2c7fb2", cex=1.2)

dev.off()

#===================================================
## Export each of the time-space graphs separately
#===================================================
tiff(file="S1_map_time_and_space_pre_1900.tiff", 
            width=28, 
            height=22, units="cm", 
            pointsize="10", compression = "lzw+p", 
            bg="white", res=600, antialias = "none"
     )

# pre-1900
plot(w_map, lwd=0.3,cex.main=2, 
		main= "Pre-1900",
		xlim=c(-100,150), 
		ylim=c(-10,10)
	)
points(dat_lab[which(dat_lab$cutyear=="Pre-1900"),]$lon,dat_lab[which(dat_lab$cutyear=="Pre-1900"),]$lat,pch=21,
		bg="#2c7fb2", cex=1.2)

dev.off()

# 1900-1940
tiff(file="S1_map_time_and_space_1900-1940.tiff", 
            width=28, height=22, units="cm", 
           pointsize="10", compression = "lzw+p", 
            bg="white", res=600, antialias = "none"
     )

plot(w_map, lwd=0.3,cex.main=2, 
		main= "1900 - 1940",
		xlim=c(-100,150), 
		ylim=c(-10,10)
		)

points(dat_lab[which(dat_lab$cutyear=="1900-1940"),]$lon,dat_lab[which(dat_lab$cutyear=="1900-1940"),]$lat,pch=21,
		bg="#2c7fb2", cex=1.2)

dev.off()

# 1941-1999
tiff(file="S1_map_time_and_space_1941-1999.tiff", 
            width=28, height=22, units="cm", 
           pointsize="10", compression = "lzw+p", 
            bg="white", res=600, antialias = "none"
     )

plot(w_map, lwd=0.3,cex.main=2, 
		main= "1941 - 1999",
		xlim=c(-100,150), 
		ylim=c(-10,10)
	)
points(dat_lab[which(dat_lab$cutyear=="1941-1999"),]$lon,dat_lab[which(dat_lab$cutyear=="1941-1999"),]$lat,pch=21,
		bg="#2c7fb2", cex=1.2)
dev.off()

# 2000-2009
tiff(file="S1_map_time_and_space_2000-2009.tiff", 
            width=28, height=22, units="cm", 
           pointsize="10", compression = "lzw+p", 
            bg="white", res=600, antialias = "none"
     )

plot(w_map, lwd=0.3,cex.main=2, 
		main= "2000-2009",
		xlim=c(-100,150), 
		ylim=c(-10,10)
	)
points(dat_lab[which(dat_lab$cutyear=="2000-2009"),]$lon,dat_lab[which(dat_lab$cutyear=="2000-2009"),]$lat,pch=21,
		bg="#2c7fb2", cex=1.2)
dev.off()

# 2010+
tiff(file="S1_map_time_and_space_post_2010.tiff", 
            width=28, height=22, units="cm", 
           pointsize="10", compression = "lzw+p", 
            bg="white", res=600, antialias = "none"
     )
plot(w_map, lwd=0.3,cex.main=2, 
		main= "Post-2010",
		xlim=c(-100,150), 
		ylim=c(-10,10)
	)
points(dat_lab[which(dat_lab$cutyear=="Post-2010"),]$lon,dat_lab[which(dat_lab$cutyear=="Post-2010"),]$lat,pch=21,
		bg="#2c7fb2", cex=1.2)

dev.off()

## END

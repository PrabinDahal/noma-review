################################################################################
# Title                : Summarising Noma systematic literature review database
# Data version         : March-2023
# Analysis version     : March-2023
# Task                 : Summarise the disease staging
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

# Read analysis dataset
dat_lab<-read.csv("analysis_data_20_03_2023.csv")

#===============================================
# Summary of disease stages
#===============================================
#-------------------------------
# Total number of studies
#-------------------------------
(total_studies <- dat_lab %>%
   dplyr::summarise(
     n_studies = length(unique(row_id))
   ))
#-----------------------------------
# Number of studies by WHO stage
#-----------------------------------
(n_articles_staging <- dat_lab %>%
   dplyr::group_by(WHO_noma_stage) %>%
   dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T),
     min_year = min(year),
     max_year = max(year),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

#-----------------------------------------------
# Summary of disease stages & design
#-----------------------------------------------
(n_articles_stage_design <- dat_lab %>%
   dplyr::group_by(WHO_noma_stage,design) %>%
   dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T),
     min_year = min(year),
     max_year = max(year),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

#-----------------------------------------------
# Summary of disease stages & design
#-----------------------------------------------
(n_articles_staging <- dat_lab %>%
   filter(WHO_noma_stage=="5. Sequelae Stage") %>% 
  dplyr::group_by(design) %>%
  dplyr::summarise(
    number_of_articles = length(row_id),
    number_of_patients = sum(n_patients, na.rm=T),
    min_year = min(year),
    max_year = max(year)
    )
  )

#-----------------------------------------------
# Summary of multiple disease stages
#-----------------------------------------------
(n_articles_staging <- dat_lab %>%
   filter(WHO_noma_stage=="Multiple Stages") %>% 
   dplyr::group_by(design) %>%
   dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T),
     min_year = min(year),
     max_year = max(year)
   )
)

#===========================================================
# Stacked bar chart: WHO Noma stage distribution by region
#===========================================================
setwd("C:/IDDO_Other/Noma/Results/out2_samplesize")

(dat_lab %>%
   dplyr::group_by(region) %>%
   dplyr::summarise(
     n_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T)
   )) 

(n_articles_stage_region <- dat_lab %>%
    dplyr::group_by(WHO_noma_stage, region) %>%
    dplyr::summarise(
      n_articles = length(row_id),
      number_of_patients = sum(n_patients, na.rm=T)
    )) 

# Filter out missing regions
n_articles_stage_region <- n_articles_stage_region %>% 
  filter(region!=" ")


tiff(file="S2_noma_stages_by_region.tiff", 
     width=36, height=24, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

ggbarplot(n_articles_stage_region, 
          x = "region",
          y = "number_of_patients",
          fill = "WHO_noma_stage",
          palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
          sorting = "descending",                       # Sort value in descending order
          rotate = FALSE,                                # Rotate vertically
          ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Number of patients") +
  #scale_y_log10() +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(6)))+
  theme(legend.key.size = unit(1.8, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.1)),
        strip.text.x = element_text(size=rel(1.7)),
        strip.text.y = element_text(size=rel(1.7)))+
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "right")

dev.off()

#===================================================
# Stacked bar chart: Stage distribution by Age
#===================================================
(n_articles_stage_age <- dat_lab %>%
   dplyr::group_by(WHO_noma_stage, age) %>%
   dplyr::summarise(
     n_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T)
   )) 

tiff(file="S2_noma_stages_by_age.tiff", 
     width=32, height=24, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none")

ggbarplot(n_articles_stage_age, 
          x = "age",
          y = "number_of_patients",
          fill = "WHO_noma_stage",
          palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
          sorting = "descending",                       # Sort value in descending order
          rotate = FALSE,                                # Rotate vertically
          ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Number of patients") +
  #scale_y_log10() +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(6)))+
  theme(legend.key.size = unit(1.8, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.1)),
        strip.text.x = element_text(size=rel(1.7)),
        strip.text.y = element_text(size=rel(1.7)))+
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "right")

dev.off()

#===================================================
# Stacked bar chart: Stage distribution by Time
#===================================================
dat_lab$time_period <- fct_relevel(dat_lab$time_period , 
                                   "Pre-1900", 
                                   "1900-1924", 
                                   "1925-1949",
                                   "1950-1974",
                                   "1975-1999",
                                   "2000-2009",
                                   "Post-2010")

(dat_lab %>%
   dplyr::group_by(time_period) %>%
   dplyr::summarise(
     n_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T)
   )) 

(n_articles_stage_time <- dat_lab %>%
    dplyr::group_by(WHO_noma_stage, time_period) %>%
    dplyr::summarise(
      n_articles = length(row_id),
      number_of_patients = sum(n_patients, na.rm=T)
    )) 

# Plot number of studies and number of patients
a <- ggbarplot(n_articles_stage_time, 
          x = "time_period",
          y = "n_articles",
          fill = "WHO_noma_stage",
          palette = brewer.pal(n = 7, name = "Set2"),   
          sorting = "descending",                       
          rotate = FALSE,                               
          ggtheme = theme_pubr()                        
) +
  ylab("Number of studies") +
  #scale_y_log10() +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(4.5)))+
  theme(legend.key.size = unit(1.2, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.1)),
        strip.text.x = element_text(size=rel(1.7)),
        strip.text.y = element_text(size=rel(1.7)))+
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "right")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


b<- ggbarplot(n_articles_stage_time, 
          x = "time_period",
          y = "number_of_patients",
          fill = "WHO_noma_stage",
          palette = brewer.pal(n = 7, name = "Set2"),      
          sorting = "descending",                       
          rotate = FALSE,                               
          ggtheme = theme_pubr()                        
) +
  ylab("Number of patients") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(4.5)))+
  theme(legend.key.size = unit(1.2, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.1)),
        strip.text.x = element_text(size=rel(1.7)),
        strip.text.y = element_text(size=rel(1.7)))+
  theme(
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "right")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#----------------------------
# Export graph
#----------------------------
tiff(file="S2_n_pubs_n_patients_stages_by_stage_and_time.tiff", 
     width=44, height=18, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

plot_grid(a,b)

dev.off()

#----------------------------
# Export graph
#----------------------------
tiff(file="supplemental_fig_10.tiff", 
     width=28, height=18, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )
b
dev.off()

#=============================================================
# Total number of articles by period & noma stages
# Same plot as before; but with different color scheme
#=============================================================
dat_lab$time_period <- fct_relevel(dat_lab$time_period , 
                                   "Pre-1900",
                                   "1900-1924",
                                   "1925-1949",
                                   "1950-1974",
                                   "1975-1999",
                                   "2000-2009",
                                   "Post-2010")

# generate summary by stage and time            
(n_patients_stage <- dat_lab %>% 
    dplyr::group_by(WHO_noma_stage,time_period) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))

a <- ggbarplot(n_patients_stage, 
               x = "time_period",
               y = "number_of_articles",
               fill = "WHO_noma_stage",
               #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
               sorting = "descending",                       # Sort value in descending order
               rotate = FALSE,                                # Rotate vertically
               ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Number of studies") +
  #scale_y_log10() +
  xlab("")+
  guides(fill=guide_legend(title=""), size=30)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40))+
  theme(legend.position = "right")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

b <- ggbarplot(n_patients_stage, 
               x = "time_period",
               y = "number_of_patients",
               fill = "WHO_noma_stage",
               #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
               sorting = "descending",                       # Sort value in descending order
               rotate = FALSE,                                # Rotate vertically
               ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Number of patients") +
  #scale_y_log10() +
  xlab("")+
  guides(fill=guide_legend(title=""), size=30)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40))+
  theme(legend.position = "right")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#---------------------
# Export the plot
#---------------------
tiff(file="C:/IDDO_Other/Noma/Results/out2_samplesize/S2_noma_stages_by_time.tiff", 
     width=56, height=20, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

plot_grid(a,b)

dev.off()

## END

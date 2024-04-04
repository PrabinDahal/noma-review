################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Summarise mortality data by disease status
################################################################################
#rm(list=ls())
library(pacman)
pacman::p_load(readxl,tidyverse,ggpubr,purrr,readr,stringr,patchwork,
               table1,doBy,gtools,janitor,RColorBrewer,wesanderson,
               viridis,forcats,tibble,tidyr,dplyr,maps,rworldmap,rworldxtra,naniar,
               cowplot,maptools,gridExtra,classInt,car,ggpubr,ggalt,plyr,IDPmisc,rms,
               epitools,textclean,waffle,ggwaffle,forcats,ggbreak,binom,meta)

# Working directory
setwd("C:/IDDO_Other/Noma/Data")

# Read the original datasets
dat_lab<-read.csv("analysis_data_20_03_2023.csv")

#-------------------------------
# Total number of studies
#-------------------------------
(total_studies <- dat_lab %>%
   dplyr::summarise(
     n_studies = length(unique(row_id))
   ))

#-----------------------------------------------------------------------
# Total number of articles by risk factors reported by time-period
#-----------------------------------------------------------------------
dat_lab$n_deaths[dat_lab$n_deaths ==-99]<- NA

# Change the order of levels
dat_lab$time_period <- fct_relevel(dat_lab$time_period , 
                                   "Pre-1900", "1900-1924", "1925-1949","1950-1974",
                                   "1975-1999","2000-2009","Post-2010"
)

## Keep only study that reported deaths
dat_lab <- dat_lab %>% 
    filter(!is.na(n_deaths))
nrow(dat_lab)

# total number of studies
(total_studies <- dat_lab %>%
    dplyr::summarise(n_studies = length(unique(row_id))
    ))

# total number of studies
(total_studies <- dat_lab %>%
    filter(n_deaths==0) %>% 
    dplyr::summarise(n_studies = length(unique(row_id))
    ))

(overall_deaths <- dat_lab %>% 
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
    )) 

(overall_deaths <- dat_lab %>% 
    dplyr::group_by(design) %>%
    dplyr::summarise(
      n_patients = sum(n_patients, na.rm=T),
      n_studies = length(unique(row_id)),
      n_deaths = sum(n_deaths, na.rm=T)
      )) 

(noma <- dat_lab %>% 
    filter(year>2000) %>% 
  #  dplyr::group_by(design) %>%
    dplyr::summarise(
      n_patients = sum(n_patients, na.rm=T),
      n_studies = length(unique(row_id)),
      n_deaths = sum(n_deaths, na.rm=T)
    )) 

#============================================================================
# Total number of deaths reported by time-period & study design
#============================================================================
(death <- dat_lab %>% 
    dplyr::group_by(design) %>%
      dplyr::summarise(
        n_patients = sum(n_patients, na.rm=T),
        n_studies = length(unique(row_id)),
        n_deaths = sum(n_deaths, na.rm=T)
    )) 

#============================================================================
# Total number of deaths reported by time-period & age 
#============================================================================

(death_age <- dat_lab %>% 
   dplyr::group_by(time_period, age_group) %>%
   dplyr::summarise(
     n_patients = sum(n_patients, na.rm=T),
     n_studies = length(unique(row_id)),
     n_deaths = sum(n_deaths, na.rm=T)
   )) 
#write.csv(death_age, "I:/Prabin/Noma/Results/mortality_by_age_period.csv", row.names=FALSE)

#============================================================================
# Total number of deaths reported by time-period & Noma stage 
#============================================================================
(death_stage <- dat_lab %>% 
   dplyr::group_by(time_period, WHO_noma_stage) %>%
   dplyr::summarise(
     enrolled = sum(n_patients, na.rm=T),
     #n_studies = length(unique(row_id)),
     Death = sum(n_deaths, na.rm=T),
     Alive = enrolled-Death
   )) %>% 
  select(-enrolled)

death_stage1 <- death_stage  %>% 
  pivot_longer(
    cols = c("Death","Alive"),
    names_to = "survival",
    names_prefix = "survival",
    values_to = "total"
  )

s1 <- ggbarplot(death_stage1 %>%  filter(WHO_noma_stage=="1. Acute necrotizing gingivitis"), 
          x = "time_period",
          y = "total",
          fill = "survival",
          #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
          sorting = "descending",                       # Sort value in descending order
          rotate = FALSE,                                # Rotate vertically
          ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("Acute necrotizing gingivitis") +
  ylab("Number of patients") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(1.4, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "right")+
  theme(legend.position = c(0.2,0.7))+
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))

s2 <- ggbarplot(death_stage1 %>%  filter(WHO_noma_stage=="2. Oedema Stage"), 
                x = "time_period",
                y = "total",
                fill = "survival",
                #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("Oedema Stage") +
  ylab("") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))+
  ylim(0,30)

s3 <- ggbarplot(death_stage1 %>%  filter(WHO_noma_stage=="3. Gangrenous Stage"), 
                x = "time_period",
                y = "total",
                fill = "survival",
                #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("Gangrenous Stage") +
  ylab("") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))+
  ylim(0,30)

s4 <- ggbarplot(death_stage1 %>%  filter(WHO_noma_stage=="4. Scarring Stage"), 
                x = "time_period",
                y = "total",
                fill = "survival",
                #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("Scarring Stage") +
  ylab("Number of patients") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.29,0.7))+
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))+
  ylim(0,30)

s5 <- ggbarplot(death_stage1 %>%  filter(WHO_noma_stage=="5. Sequelae Stage"), 
                x = "time_period",
                y = "total",
                fill = "survival",
                #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("Sequelae Stage") +
  ylab("") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))

s6 <- ggbarplot(death_stage1 %>%  filter(WHO_noma_stage=="Multiple Stages"), 
                x = "time_period",
                y = "total",
                fill = "survival",
                #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("Multiple Stages") +
  ylab("") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))

s7 <- ggbarplot(death_stage1 %>%  filter(WHO_noma_stage=="Unclear"), 
                x = "time_period",
                y = "total",
                fill = "survival",
                #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("Unclear") +
  ylab("Number of patients") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))

#===================================================
# Export plot of reported deaths by disease status
#===================================================
tiff(file="I:/Prabin/Noma/Results/Surival_by_noma_stage.tiff", 
     width=50, height=36, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=600, antialias = "none")

plot_grid(s1,s2,s3,s4,s5,s6,s7)

dev.off()

#============================================================================
# Total number of deaths reported by time-period & study design & continent
#============================================================================
(death_design <- dat_lab %>% 
   dplyr::group_by(time_period, region) %>%
   dplyr::summarise(
     enrolled = sum(n_patients, na.rm=T),
     #n_studies = length(unique(row_id)),
     Death = sum(n_deaths, na.rm=T),
     Alive = enrolled-Death
   )) %>% 
  select(-enrolled)

death_design1 <- death_design  %>% 
  pivot_longer(
    cols = c("Death","Alive"),
    names_to = "survival",
    names_prefix = "survival",
    values_to = "total"
  )

s1 <- ggbarplot(death_design1 %>%  filter(region=="Africa"), 
                x = "time_period",
                y = "total",
                fill = "survival",
                #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("Africa") +
  ylab("Number of patients") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(1.4, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "right")+
  theme(legend.position = c(0.2,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))

s2 <- ggbarplot(death_design1 %>%  filter(region=="Asia"), 
                x = "time_period",
                y = "total",
                fill = "survival",
                #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("Asia") +
  ylab("") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))


s3 <- ggbarplot(death_design1 %>%  filter(region=="Europe"), 
                x = "time_period",
                y = "total",
                fill = "survival",
                #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("Europe") +
  ylab("") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))

s4 <- ggbarplot(death_design1 %>%  filter(region=="The Americas"),
                x = "time_period",
                y = "total",
                fill = "survival",
                #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("The Americas") +
  ylab("Number of patients") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))


s5 <- ggbarplot(death_design1 %>%  filter(region=="Rest") ,
                x = "time_period",
                y = "total",
                fill = "survival",
                #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ggtitle("Rest") +
  ylab("") +
  xlab("")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "none")+
  #theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=22))

#===================================================
# Export plot of reported deaths by region and time
#===================================================
tiff(file="C:/IDDO_Other/Noma/Results/out5_deaths/S5_Surival_by_region_time.tiff", 
     width=50, height=36, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=600, antialias = "none")

plot_grid(s1,s2,s3,s4,s5)

dev.off()

## END
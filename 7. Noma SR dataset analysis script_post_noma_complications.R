################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Summarising post-noma complications
################################################################################
#rm(list=ls())
library(pacman)
#devtools::install_github("thomasp85/patchwork") # needs for par for ggplot2

pacman::p_load(readxl,tidyverse, ggpubr,purrr,readr, stringr,patchwork,table1,doBy,gtools,
               janitor,RColorBrewer,wesanderson,viridis,forcats, tibble,tidyr,dplyr,
               maps,rworldmap,rworldxtra,naniar,cowplot,maptools,gridExtra,classInt, car,ggpubr,ggalt,
               plyr,IDPmisc,rms,epitools
               )

# Working directory
setwd("I:/Prabin/Noma/Data")

#---------------------------
# Read the analysis dataset
#---------------------------
dat_lab <-read.csv("analysis_data_20_03_2023.csv")

# Total number of studies
(total_studies <- dat_lab %>%
   dplyr::summarise(n_studies = length(unique(row_id))
   ))
#-----------------------------------------------
# Deaths reported
#-----------------------------------------------
dat_lab [dat_lab == -99] <- NA

(n_patients_death <- dat_lab %>% 
    dplyr::group_by(treatment_modality) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

#-----------------------
# Before or after 1928
#-----------------------
dat_lab$before1928<- ifelse(dat_lab$year <= 1928, "before1928","onorafter1928")

(n_patients_death <- dat_lab %>% 
    dplyr::group_by(before1928) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

#-----------------------
# By age-group
#-----------------------
(n_patients_death <- dat_lab %>% 
    dplyr::group_by(age_group) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

#-----------------------------------------------
# post_noma_complications
#-----------------------------------------------
(n_post_noma <- dat_lab %>% 
   dplyr::group_by(post_noma_complications) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id)),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

(n_post_noma <- dat_lab %>% 
    dplyr::group_by(WHO_noma_stage,post_noma_complications) %>%
    dplyr::summarise(
          number_of_articles = length(unique(row_id)),
          number_of_patients = sum(n_patients, na.rm=T)
    )) 

write.csv(n_post_noma, "I:/Prabin/Noma/Results/n_post_noma_stage.csv", row.names=FALSE)

#-----------------------------------------------
# studies reporting any post_noma_complications
#-----------------------------------------------
(n_post_noma <- dat_lab %>% 
    filter(post_noma_complications %in% c("Head complications","Multiple complications","Not applicable -death",
                                            "Other complications"  )) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

## END
################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Summarise patient data by disease status
################################################################################
#rm(list=ls())
library(pacman)
pacman::p_load(readxl,tidyverse, ggpubr,purrr,readr, stringr,patchwork,table1,
               doBy,gtools,janitor,RColorBrewer,wesanderson,viridis,
               forcats, tibble,tidyr,dplyr,maps,rworldmap,rworldxtra,naniar,cowplot,maptools,
               gridExtra,classInt, car,ggpubr,ggalt,plyr,IDPmisc,rms,epitools)


# Working directory
setwd("I:/Prabin/Noma/Data")

#-----------------------------
# Read the analysis dataset
#-----------------------------
dat_lab<-read.csv("analysis_data_20_03_2023.csv")

#-----------------------------
# Total number of studies
#-----------------------------
(total_studies <- dat_lab %>%
   dplyr::summarise(n_studies = length(unique(row_id))
   ))

#-----------------------------------------------
# Patient description
#-----------------------------------------------
(n_patients_age <- dat_lab %>% 
   dplyr::group_by(age_group) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id)),
     percentage = round((number_of_articles/total_studies)*100,2)
     )) %>% 
  arrange(desc(percentage))


# Disease Stage of Noma
(n_patients_stage <- dat_lab %>% 
    dplyr::group_by(WHO_noma_stage) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      percentage = round((number_of_articles/total_studies)*100,2)
      )) %>% 
  arrange(desc(percentage))

## END
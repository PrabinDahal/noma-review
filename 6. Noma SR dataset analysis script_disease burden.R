################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Summarizing disease burden measures
################################################################################
#rm(list=ls())
library(pacman)
pacman::p_load(readxl,tidyverse, ggpubr,purrr,readr, stringr,patchwork,table1,
               doBy,gtools,janitor,RColorBrewer,wesanderson,viridis,
               forcats, tibble,tidyr,dplyr,maps,rworldmap,rworldxtra,naniar,cowplot,maptools,
               gridExtra,classInt, car,ggpubr,ggalt,plyr,IDPmisc,rms,epitools
               )

# Working directory
setwd("I:/Prabin/Noma/Data")

#--------------------------
# Read the analysis dataset
#--------------------------
dat_lab<-read.csv("analysis_data_20_03_2023.csv")
dat_lab$n_deaths[dat_lab$n_deaths==-99] <- NA

# Total number of studies
(total_studies <- dat_lab %>%
   dplyr::summarise(n_studies = length(unique(row_id))
   ))

#-----------------------------------------------
# Total number of articles by burden of disease
#-----------------------------------------------
(n_patients_burden<- dat_lab %>% 
   dplyr::group_by(burden_of_disease) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id)),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

#----------------------------------------
# Burden of the disease
#----------------------------------------
(n_rf <- dat_lab %>% 
   dplyr::group_by(burden_of_disease) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id)),
     number_of_deaths = sum(n_deaths, na.rm=T),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

(n_rf <- dat_lab %>% 
    dplyr::group_by(source.of.estimate) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

#---------------------------------------------------------------
# Explore only those studies with primary, secondary or both
#---------------------------------------------------------------
dat_burden <- dat_lab %>% 
    filter(source.of.estimate %in% c("Primary ","Secondary","Both"))

(n_rf <- dat_burden %>% 
    dplyr::group_by(source.of.estimate) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))

(n_rf <- dat_burden %>% 
    dplyr::group_by(source.of.estimate,BOD_estimate) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))


## END
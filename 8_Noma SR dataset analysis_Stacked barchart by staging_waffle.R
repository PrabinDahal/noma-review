################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Stacked barcharts for disease status and mortality
################################################################################
#rm(list=ls())
library(pacman)
#devtools::install_github("thomasp85/patchwork") # needs for par for ggplot2
#devtools::install_github("liamgilbey/ggwaffle")

pacman::p_load(readxl,tidyverse, ggpubr,purrr, readr, stringr,patchwork,table1,doBy,gtools,
               janitor,RColorBrewer,wesanderson,viridis,forcats, tibble,tidyr,dplyr,
               maps,rworldmap,rworldxtra,naniar,cowplot,maptools,gridExtra,ggbreak,
               classInt, car,ggpubr,ggplot2,ggalt,plyr,IDPmisc,rms,epitools,
               waffle, ggwaffle,cowplot
               )

# Working directory
setwd("I:/Prabin/Noma/Data")

#----------------------------
# Read the analysis dataset
#----------------------------
dat_lab<-read.csv("analysis_data_20_03_2023.csv")

#====================================================
# Number of articles by design and time-period
#====================================================
dat_lab$continent[dat_lab$continent=="Middle-east"] <- "Middle-East"
dat_lab$design[dat_lab$design=="Interventional (RCT or non-randomised clinical study)"] <- "Interventional"

# 3.6 Microbiology - the document you sent didn't have an updated table. I have updated numbers based on my review of free text variables. 
# Can you please double check the table and go back to the data extraction spreadsheet instead of any data you may be working off. 
# I found errors in the guys extraction with 3 studies (25,30,70 which should have been marked Yes)
dat_lab$design[dat_lab$design=="Interventional (RCT or non-randomised clinical study)"] <- "Interventional"

# Change the order of levels
dat_lab <- dat_lab %>% 
  mutate (year_cut = case_when(
    year <1900 ~ "Pre-1900",
    year >=1900 & year <1950 ~ "1900-1950",
    year >=1950 & year <2000 ~ "1950-2000",
    year >=2000 ~ "2000+"
  ))

# Reorder the factor levels
dat_lab$age_group <- fct_relevel(dat_lab$age_group, 
                                 "Infant (<1)",
                                 "Child (1 - <10)",
                                 "Children (0-19)",
                                 "Adolsecent (10 - 19)",
                                 "Adult (>19)",
                                 "All age groups",
                                 "Not Reported"
                                 )


dat_lab <- dat_lab %>% 
  mutate(age = case_when(
    age_group=="Infant (<1)" ~ "Children (0-19)",
    age_group=="Child (1 - <10)" ~ "Children (0-19)",
    age_group=="Children (0-19)" ~ "Children (0-19)",
    age_group=="Adolsecent (10 - 19)" ~ "Children (0-19)",
    age_group=="Adult (>19)" ~ "Adult (>19)",
    age_group=="All age groups" ~ "All ages",
    age_group=="Not Reported" ~ "Not Reported"
  ))

dat_lab$age <- fct_relevel(dat_lab$age, 
                                  "All ages",
                                  "Children (0-19)",
                                  "Adult (>19)",
                                  "Not Reported"
                            )

dat_lab <- dat_lab %>% 
  mutate(region = case_when(
    continent=="Africa" ~ "Africa",
    continent=="Asia" ~ "Asia",
    continent=="Europe" ~ "Europe",
    continent=="The Americas" ~ "The Americas",
    continent=="Middle-East" ~ "Rest",
    continent=="Multi-country" ~ "Rest",
    continent=="Not stated" ~ "Rest"  
  ))


dat_lab$year_cut <- fct_relevel(dat_lab$year_cut, 
                           "Pre-1900",
                           "1900-1950",
                           "1950-2000",
                           "2000+"
  )

# create a new variable called time period
dat_lab <- dat_lab %>% 
  mutate(time_period = case_when(
    year < 1900 ~ "Pre-1900",
    year >= 1900 & year < 1925 ~ "1900-1924",
    year >= 1925 & year < 1950 ~ "1925-1949",
    year >= 1950 & year < 1975 ~ "1950-1974",
    year >= 1975 & year < 1999 ~ "1975-1999",
    year >= 1999 & year < 2010 ~ "2000-2009",
    year >= 2010  ~ "Post-2010"
  )
  )

# Change the order of levels
dat_lab$time_period <- fct_relevel(dat_lab$time_period , 
                                   "Pre-1900", "1900-1924", "1925-1949","1950-1974",
                                   "1975-1999","2000-2009","Post-2010")

#===================================================
# Summarise microbiology data
#===================================================
(total_studies <- dat_lab %>%
    dplyr::summarise(
      n_studies = length(unique(row_id))
    )
  )

(total_studies_mb <- dat_lab %>%
    filter(MICROBIOLOGY_observed_reported=="Yes") %>% 
    dplyr::summarise(
      n_studies = length(unique(row_id))
    )
)

(n_articles_mb <- dat_lab %>%
    dplyr::group_by(MICROBIOLOGY_observed_reported) %>%
    dplyr::summarise(
      n_articles = length(row_id),
      n_patients = sum(n_patients, na.rm=T)
      
    )) 

(n_articles_mb <- dat_lab %>%
    dplyr::group_by(MICROBIOLOGY_observed_reported,SPECIFIC_MICROORGANISM) %>%
    dplyr::summarise(
      n_articles = length(row_id),
      n_patients = sum(n_patients, na.rm=T)
    )) 

(n_articles_mb <- dat_lab %>%
    dplyr::group_by(SPECIFIC_MICROORGANISM) %>%
    dplyr::summarise(
      n_articles = length(row_id),
      n_patients = sum(n_patients, na.rm=T)
    )) 

#------------------------------------
# SPECIFIC_MICROORGANISM
# microbiology_details
#------------------------------------
(n_articles_mb <- dat_lab %>%
    dplyr::group_by(microbiology_details) %>%
    dplyr::summarise(
      n_articles = length(row_id),
      n_patients = sum(n_patients, na.rm=T)
    )) 
#===========================================================
# Stacked bar chart: WHO Noma stage distribution by region
#===========================================================
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

tiff(file="I:/Prabin/Noma/Results/2023_noma_stages_by_region.tiff", 
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
# Stacked bar chart: stage distribution by Age
#===================================================
(n_articles_stage_age <- dat_lab %>%
   dplyr::group_by(WHO_noma_stage, age) %>%
   dplyr::summarise(
     n_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T)
   )) 

tiff(file="I:/Prabin/Noma/Results/2023_noma_stages_by_age.tiff", 
     width=32, height=24, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )


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
# Stacked bar chart: stage distribution by time
#===================================================
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

tiff(file="I:/Prabin/Noma/Results/2023_noma_stages_by_time.tiff", 
     width=32, height=24, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none")

ggbarplot(n_articles_stage_time, 
          x = "time_period",
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

#==============================================
# Export reports of death 
#==============================================
dat_lab [dat_lab == -99] <- NA

#-----------------------
# By age-group
#-----------------------
(n_patients_death <- dat_lab %>% 
   filter(!is.na(n_deaths)) %>% 
    dplyr::group_by(continent, design) %>%
      dplyr::summarise(
          number_of_patients = sum(n_patients, na.rm=T),
          number_of_deaths = sum(n_deaths, na.rm=T)
   )) 

tiff(file="I:/Prabin/Noma/Results/2023_deaths_reported_by_region.tiff", 
     width=28, height=18, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )


ggbarplot(n_patients_death, 
          x = "continent",
          y = "number_of_deaths",
          fill = "design",
          palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
          sorting = "descending",                       # Sort value in descending order
          rotate = FALSE,                                # Rotate vertically
          ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Reported number of deaths") +
  #scale_y_log10() +
  xlab("")+
  guides(fill=guide_legend(title=""), size=60)+
  theme(legend.text=element_text(size=rel(10.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.7)),
        strip.text.x = element_text(size=rel(2.7)),
        strip.text.y = element_text(size=rel(5.7)))+
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 80))+
  theme(legend.position = "right")

dev.off()

#------------------------------------------
# Reported number of death by study design
#------------------------------------------
(n_patients_death_design <- dat_lab %>% 
    filter(!is.na(n_deaths)) %>%
    filter(!is.na(n_patients)) %>% 
      dplyr::group_by(design) %>%
        dplyr::summarise(
            enrolled = sum(n_patients, na.rm=T),
            reported.death = sum(n_deaths, na.rm=T)
        )
  ) 

n_patients_death_design <- n_patients_death_design %>% 
        mutate(alive = enrolled - reported.death)

n_patients_death_design1 <- n_patients_death_design %>% 
  pivot_longer(
    cols = c("alive","reported.death"),
  names_to = "group",
  names_prefix = "cohort",
  values_to = "n_patients",
  values_drop_na = TRUE)

tiff(file="I:/Prabin/Noma/Results/2023_deaths_reported_by_design.tiff", 
     width=28, height=18, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none")

ggbarplot(n_patients_death_design1, 
          x = "design",
          y = "n_patients",
          fill = "group",
          #title="Deaths in records with non-missing information",
          #palette = brewer.pal(n = 2, name = "Set2"),                             # Color by groups
          sorting = "descending",                       # Sort value in descending order
          rotate = FALSE,                                # Rotate vertically
          ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Total number of patients") +
  #scale_y_log10() +
  xlab("")+
  guides(fill=guide_legend(title=""), size=60)+
  theme(legend.text=element_text(size=rel(10.5)))+
  theme(legend.key.size = unit(1.8, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.7)),
        strip.text.x = element_text(size=rel(2.7)),
        strip.text.y = element_text(size=rel(5.7)))+
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 60))+
  theme(legend.position = "right")


dev.off()

#------------------------------------
# Nutritional deficit by continent
#------------------------------------
(n_nutri <- dat_lab %>% 
    filter(nutritional_deficit=="Yes") %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))) %>% 
    mutate(risk ="Nutritional deficit")
)

## END
################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Create analysis dataset by tidying the raw data
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

# Read the original datasets
dat_lab<-read.csv("data_20_03_2023.csv")

#########################################
# SECTION I: READ THE SYS REVIEW DATASET
#########################################

#---------------------------------
# Tidy up country names
#---------------------------------
# Get rid of trailing/leading white spaces in the country names
dat_lab$country <- trimws(dat_lab$country)

multicountry <- dat_lab %>% 
        filter(country %in% c("UK, Ethiopia", "Togo, Mali","Nigeria, Ethiopia",
                              "Niger, Guinea Bissau. hospital belonging to the German Hilfsaktion Noma foundation.",
                              "Guinea, Republic of Congo, Madagascar, Benin, Cameroon, and Senegal.",
                              "Ethiopia/Nigeria"
                              ))
# 41, 230, 298, 312, 339
dat_lab$country[dat_lab$country == "UK, Ethiopia"]<- "Multi-country"
dat_lab$country[dat_lab$country == "Togo, Mali"]<- "Multi-country"
dat_lab$country[dat_lab$country == "Nigeria, Ethiopia"]<- "Multi-country"
dat_lab$country[dat_lab$country == "Niger, Guinea Bissau. hospital belonging to the German Hilfsaktion Noma foundation."]<- "Multi-country"
dat_lab$country[dat_lab$country == "Guinea, Republic of Congo, Madagascar, Benin, Cameroon, and Senegal."]<- "Multi-country"
dat_lab$country[dat_lab$country == "Ethiopia/Nigeria"]<- "Multi-country"

# Standardise the discordant country names 
dat_lab$country[dat_lab$country == "Ivory coast"]<- "Côte d'Ivoire"
dat_lab$country[dat_lab$country == "Africa"]<- "Not stated"

# All the countries in the UK coded as the UK
dat_lab$country[dat_lab$country == "United Kingdom"]<- "UK"
dat_lab$country[dat_lab$country == "Wales"]<- "UK"
dat_lab$country[dat_lab$country == "North Ireland"]<- "UK"
dat_lab$country[dat_lab$country == "England"]<- "UK"
dat_lab$country[dat_lab$country == "Scotland"]<- "UK"

dat_lab$country[dat_lab$country == "United states America"]<- "USA"
dat_lab$country[dat_lab$country == "Lao People's Democratic Republic (Laos)"]<- "Lao People's Democratic Republic (Laos)"
dat_lab$country[dat_lab$country == "Laos"]<- "Lao People's Democratic Republic (Laos)"
dat_lab$country[dat_lab$country == "Burma"]<- "Myanmar"

# New Guinea is: Papua, New Guinea (Malden, N. (1985).)
dat_lab$country[dat_lab$country == "New Guinea"]<- "Papua New Guinea"

# multi-country
dat_lab$country[dat_lab$country == "UK, Ethiopia"]<- "Multi-country"
dat_lab$country[dat_lab$country == "Togo, Mali"]<- "Multi-country"
dat_lab$country[dat_lab$country == "Nigeria, Ethiopia"]<- "Multi-country"
dat_lab$country[dat_lab$country == "Niger, Guinea Bissau. hospital belonging to the German Hilfsaktion Noma foundation."]<- "Multi-country"
dat_lab$country[dat_lab$country == "Guinea, Republic of Congo, Madagascar, Benin, Cameroon, and Senegal."]<- "Multi-country"
dat_lab$country[dat_lab$country == "Ethiopia/Nigeria"]<- "Multi-country"
dat_lab <- droplevels(dat_lab)

## There are still some characters in the Laos that it is being picked as two countries
## Use the textclean solution
dat_lab <-  dat_lab %>% 
  mutate(country = replace_non_ascii(str_trim(country)))

dat_lab <- dat_lab %>% 
  mutate(continent= case_when(
    country=="Afghanistan" ~ "Asia",
    country=="Algeria" ~ "Africa",
    country=="Austria" ~ "Europe",
    country=="Bangladesh" ~ "Asia",
    country=="Brazil" ~ "The Americas",
    country=="Bulgaria" ~ "Europe",
    country=="Burkina Faso" ~ "Africa",
    country=="Burundi" ~ "Africa",
    country=="Cambodia" ~ "Asia",
    country=="Cameroon" ~ "Africa",
    country=="Chile" ~ "The Americas",
    country=="China" ~ "Asia",
    country=="Colombia" ~ "The Americas",
    country=="Côte d'Ivoire" ~ "Africa",
    country=="Cote d'Ivoire" ~ "Africa",
    country=="Croatia" ~ "Europe",
    country=="Democratic Republic of Congo" ~ "Africa",
    country=="Ethiopia" ~ "Africa",
    country=="France" ~ "Europe",
    country=="Gabon" ~ "Africa",
    country=="Gambia" ~ "Africa",
    country=="Germany" ~ "Europe",
    country=="Ghana" ~ "Africa",
    country=="Guinea" ~ "Africa",
    country=="Guinea-Bissau" ~ "Africa",
    country=="Hungary" ~ "Europe",
    country=="Chad" ~ "Africa",
    country=="Ireland" ~ "Europe",
    country=="Pakistan" ~ "Asia",
    
    
    
    country=="India" ~ "Asia",
    country=="Indonesia" ~ "Asia",
    country=="Iran" ~ "Asia",
    country=="Israel" ~ "Middle-East",
    country=="Italy" ~ "Europe",
    
    country=="Jamaica" ~ "The Americas",
    country=="Japan" ~ "Asia",
    country=="Kenya" ~ "Africa",
    country=="Lao People's Democratic Republic (Laos)" ~ "Asia",
    #country=="Lao People's Democratic Republic (Laos)" ~ "Asia",
    
    country=="Lesotho" ~ "Africa",
    country=="Madagascar" ~ "Africa",
    country=="Mali" ~ "Africa",
    country=="Micronesia" ~ "Asia",
    country=="Morocco" ~ "Africa",
    country=="Multi-country" ~ "Multi-country",
    country=="Myanmar" ~ "Asia",
    country=="Namibia" ~ "Africa",
    country=="Nepal" ~ "Asia",
    
    country=="Niger" ~ "Africa",
    
    country=="Nigeria" ~ "Africa",
    country=="Not stated" ~ "Not stated",
    country=="Papua New Guinea" ~ "Asia",
    country=="Philippines" ~ "Asia",
    country=="Portugal" ~ "Europe",
    country=="Saudi Arabia" ~ "Middle-East",
    country=="Senegal" ~ "Africa",
    country=="Singapore" ~ "Asia",
    
    country=="Slovenia" ~ "Europe",
    country=="South Africa" ~ "Africa",
    country=="South Korea" ~ "Asia",
    country=="Spain" ~ "Europe",
    country=="Switzerland" ~ "Europe",
    country=="Taiwan" ~ "Asia",
    country=="Togo" ~ "Africa",
    country=="Tunisia" ~ "Africa",
    country=="Turkey" ~ "Europe",
    country=="Uganda" ~ "Africa",
    
    country=="UK" ~ "Europe",
    country=="USA" ~ "The Americas",
    country=="Vietnam" ~ "Asia",
    country=="Zambia" ~ "Africa",
    country=="Zimbabwe" ~ "Africa"
  ))

dat_lab$continent[dat_lab$row_id==41]  <- "Africa"
dat_lab$continent[dat_lab$row_id==149] <- "Africa"
dat_lab$continent[dat_lab$row_id==230] <- "Africa"
dat_lab$continent[dat_lab$row_id==298] <- "Africa"
dat_lab$continent[dat_lab$row_id==312] <- "Africa"
dat_lab$continent[dat_lab$row_id==339] <- "Africa"

### WHO Regions
dat_lab <- dat_lab %>% 
  mutate(WHO_region= case_when(
    country=="Afghanistan" ~ "EMR",
    country=="Algeria" ~ "AFR",
    country=="Austria" ~ "EUR",
    country=="Bangladesh" ~ "SEAR",
    country=="Brazil" ~ "AMR",
    country=="Bulgaria" ~ "EUR",
    country=="Burkina Faso" ~ "AFR",
    country=="Burundi" ~ "AFR",
    country=="Cambodia" ~ "WPR",
    country=="Cameroon" ~ "AFR",
    country=="Chile" ~ "AMR",
    country=="China" ~ "WPR",
    country=="Colombia" ~ "AMR",
    country=="Côte d'Ivoire" ~ "AFR",
    country=="Cote d'Ivoire" ~ "AFR",
    country=="Croatia" ~ "EUR",
    country=="Democratic Republic of Congo" ~ "AFR",
    country=="Ethiopia" ~ "AFR",
    country=="France" ~ "AFR",
    country=="Gabon" ~ "AFR",
    country=="Gambia" ~ "AFR",
    country=="Germany" ~ "EUR",
    country=="Ghana" ~ "AFR",
    country=="Guinea" ~ "AFR",
    country=="Guinea-Bissau" ~ "AFR",
    country=="Hungary" ~ "EUR",
    country=="Chad" ~ "AFR",
    country=="Ireland" ~ "EUR",
    country=="Pakistan" ~ "EMR",
    
    
    
    country=="India" ~ "SEAR",
    country=="Indonesia" ~ "SEAR",
    country=="Iran" ~ "EMR",
    country=="Israel" ~ "EUR",
    country=="Italy" ~ "EUR",
    
    country=="Jamaica" ~ "AMR",
    country=="Japan" ~ "WPR",
    country=="Kenya" ~ "AFR",
    country=="Lao People's Democratic Republic (Laos)" ~ "WPR",
    #country=="Lao People's Democratic Republic (Laos)" ~ "WPR",
    
    country=="Lesotho" ~ "AFR",
    country=="Madagascar" ~ "AFR",
    country=="Mali" ~ "AFR",
    country=="Micronesia" ~ "WPR",
    country=="Morocco" ~ "EMR",
    country=="Multi-country" ~ "Multi-country",
    country=="Myanmar" ~ "SEAR",
    country=="Namibia" ~ "AFR",
    country=="Nepal" ~ "SEAR",
    
    country=="Niger" ~ "AFR",
    
    country=="Nigeria" ~ "AFR",
    country=="Not stated" ~ "Not stated",
    country=="Papua New Guinea" ~ "WPR",
    country=="Philippines" ~ "WPR",
    country=="Portugal" ~ "EUR",
    country=="Saudi Arabia" ~ "EMR",
    country=="Senegal" ~ "AFR",
    country=="Singapore" ~ "WPR",
    
    country=="Slovenia" ~ "EUR",
    country=="South Africa" ~ "AFR",
    country=="South Korea" ~ "WPR",
    country=="Spain" ~ "EUR",
    country=="Switzerland" ~ "EUR",
    country=="Taiwan" ~ "WPR",
    country=="Togo" ~ "AFR",
    country=="Tunisia" ~ "EMR",
    country=="Turkey" ~ "EUR",
    country=="Uganda" ~ "AFR",
    
    country=="UK" ~ "EUR",
    country=="USA" ~ "AMR",
    country=="Vietnam" ~ "WPR",
    country=="Zambia" ~ "AFR",
    country=="Zimbabwe" ~ "AFR"
  ))

dat_lab$WHO_region[dat_lab$row_id==149] <- "AFR"
dat_lab$WHO_region[dat_lab$row_id==41]  <- "AFR"
dat_lab$WHO_region[dat_lab$row_id==230] <- "AFR"
dat_lab$WHO_region[dat_lab$row_id==298] <- "AFR"
dat_lab$WHO_region[dat_lab$row_id==312] <- "AFR"
dat_lab$WHO_region[dat_lab$row_id==339] <- "AFR"

#---------------------------------
# Tidy up lat and lon
#---------------------------------
# Get rid of trailing comma in lat 
dat_lab$lat <- as.numeric(trimws(dat_lab$lat, whitespace = ","))
dat_lab$lat[dat_lab$lat== -99 ] <- NA
dat_lab$lon[dat_lab$lon== -99 ] <- NA

#=================================================================
# Create time-binning & specify the factor orders
#=================================================================

# cutyear
dat_lab <- dat_lab %>% 
    mutate(cutyear = case_when(
                            year < 1900 ~ "Pre-1900",
                            year >= 1900 & year < 1941 ~ "1900-1940",
                            year >= 1941 & year < 2000 ~ "1941-1999",
                            year >= 2000 & year < 2010 ~ "2000-2009",
                            year >= 2010  ~ "Post-2010"
            )
    )

dat_lab$cutyear <- fct_relevel(dat_lab$cutyear, 
                                "Pre-1900",
                                "1900-1940",
                                "1941-1999",
                                "2000-2009",
                                "Post-2010"
)

## year_cut
dat_lab <- dat_lab %>% 
  mutate (year_cut = case_when(
    year <1900 ~ "Pre-1900",
    year >=1900 & year <1950 ~ "1900-1950",
    year >=1950 & year <2000 ~ "1950-2000",
    year >=2000 ~ "2000+"
  ))

dat_lab$year_cut <- fct_relevel(dat_lab$year_cut, 
                                "Pre-1900",
                                "1900-1950",
                                "1950-2000",
                                "2000+")

# time period
dat_lab <- dat_lab %>% 
  mutate(time_period = case_when(
    year < 1900 ~ "Pre-1900",
    year >= 1900 & year < 1925 ~ "1900-1924",
    year >= 1925 & year < 1950 ~ "1925-1949",
    year >= 1950 & year < 1975 ~ "1950-1974",
    year >= 1975 & year < 1999 ~ "1975-1999",
    year >= 1999 & year < 2010 ~ "2000-2009",
    year >= 2010  ~ "Post-2010"
  ))

# Change the order of levels
dat_lab$time_period <- fct_relevel(dat_lab$time_period , 
                                   "Pre-1900", 
                                   "1900-1924", 
                                   "1925-1949",
                                   "1950-1974",
                                   "1975-1999",
                                   "2000-2009",
                                   "Post-2010"
          )

#=================================================================
# Create age-groups
#=================================================================
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

# create simplified age
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
                           "Not Reported")
#=================
# Create regions
#=================
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

#========================
# Re-code study design
#========================
dat_lab <- dat_lab %>% 
            mutate(design = recode_factor(design, 
                    `Interventional (RCT or non-randomised clinical study)` = "Interventional"
                    )
                  )
#=================================================================
# Re-code treatment-modality
#=================================================================
dat_lab <- dat_lab %>% 
              mutate(treatment_modality = 
                recode_factor(treatment_modality, `Neither - N/A` = "Neither")
              )

#-------------------------------------
# Tidy up missing number of patients
#-------------------------------------
dat_lab$n_patients[dat_lab$n_patients==-99] <- NA

#-------------------------------------
# Tidy up data from Farley 2020a study
#-------------------------------------
#2. Farley 2020a has 194 noma cases not 7122 (7122 was the survey population) with only 194 confirmed as having any Noma (See Table 2)
dat_lab[which(dat_lab$study=="Farley 2020a"),]$n_patients <- 194

#------------------------------------------------
# Remove Farley 2019b study as it is a dupicate
#------------------------------------------------
#3. Farley 2019 should be excluded as it is the ASTMH abstract for Farley 2020 paper
#This describes a total of 181 with S0 Noma, 10 with S1 Noma and 3 with S3 Noma i.e. a total of 194 cases out of 7120 surveyed population.
#The same stage distribution of Noma cases are reported in Farley 2020a paper. So, I would like to think that this is a duplicated entry - very unlikely that two different studies from the same author set will found exact distribution of cases.
#The survey denominator is slightly different (by 2).

# Exclude duplicated record
dat_lab <- dat_lab %>% 
    filter(study!="Farley 2019b")

# Export analysis dataset
write.csv(dat_lab, "analysis_data_20_03_2023.csv", 
          row.names=FALSE
          )
## END
################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Summarise reported risk factors
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

#---------------------------------------------------------------------------------------
# Read the original database with wordmaps - this contains curated data for risk factors
#---------------------------------------------------------------------------------------
dat_lab <- read_excel("analysis_data_20_03_2023_wordmap.xlsx", 
                         sheet = "analysis_data_20_03_2023"
                      ) 
# declare numeric
dat_lab[c("n_patients","n_deaths")] <- sapply(dat_lab[c("n_patients","n_deaths")], as.numeric)

dat_lab <- dat_lab %>% 
  dplyr::rename(
    "immunosupression_old" = "immunosupression",
    "infectious_diseases_old" = "infectious_diseases"
  )

# Revised database on risk factors column
# Remove old immune-suppression and infectious diseases classification as it has been revised now
# we pull the immune-suppression and infectious diseases classification from the risk factors tab instead
risk_factors <- read_excel("analysis_data_20_03_2023_wordmap.xlsx", 
                      sheet = "Risk Factors") %>% 
                    clean_names() %>% 
                      select(row_id, immunosupression_9,infectious_diseases_10) %>% 
                        dplyr::rename(
                          "immunosupression" = "immunosupression_9",
                          "infectious_diseases" = "infectious_diseases_10"
                      )
dat_lab <- dat_lab %>% 
  left_join(risk_factors)

############################################
## Summarise risk factors distribution
############################################
# set output directory
setwd("C:/IDDO_Other/Noma/Results/out3_risk_factors")

#-------------------------------
# Total number of studies
#-------------------------------
(total_studies <- dat_lab %>%
   dplyr::summarise(n_studies = length(unique(row_id))
   ))

#--------------------------------------------------
# Total number of articles by risk factors reported
#--------------------------------------------------
(n_patients_risk_factors <- dat_lab %>% 
   dplyr::group_by(risk_factors__observed_reported) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id)),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))


## Number of studies reporting risk factors
(n_rf_reported <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes") %>% 
    dplyr::summarise(
      number_of_articles = length(unique(row_id))
    )
)

#-----------------------------------------------------------------------
# Total number of articles by risk factors reported by time-period
#-----------------------------------------------------------------------
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes") %>%
    dplyr::group_by(year_cut, country) %>%
        dplyr::summarise(
             number_of_patients = sum(n_patients, na.rm=T),
              number_of_articles = length(unique(row_id))
        )
    )

#-----------------------------
# Risk factors prior to 1900s
#-----------------------------
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year <1950) %>%
    #filter(risk_factors__observed_reported=="Yes" & year_cut=="Pre-1900") %>%
    dplyr::group_by(infectious_diseases) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

#----------------------------------
# Risk factors prior to 1900-1950s
#----------------------------------
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="1900-1950") %>%
    dplyr::group_by(Respiratory_infections) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)
#-----------------------------
# Risk factors 1950-2000
#-----------------------------
# by region
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="1950-2000") %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    ))

# by ID
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="1950-2000") %>%
    dplyr::group_by(infectious_diseases) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

# by nutri
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="1950-2000") %>%
      dplyr::group_by(nutritional_deficit) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

# by resp
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="1950-2000") %>%
    dplyr::group_by(Respiratory_infections) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

# by ID
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="1950-2000") %>%
    dplyr::group_by(infectious_diseases) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

# by immune
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="1950-2000") %>%
    dplyr::group_by(immunosupression) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

#-----------------------------
# Risk factors 2000+ papers
#-----------------------------
(n_patients_risk_factors <- dat_lab %>% 
   filter(year_cut=="2000+") %>%
   dplyr::group_by(risk_factors__observed_reported) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id))
   )
)

# by region
(n_patients_risk_factors <- dat_lab %>% 
   filter(risk_factors__observed_reported=="Yes" & year_cut=="2000+") %>%
   dplyr::group_by(continent) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id))
   )
)

# by immune status
(n_patients_risk_factors <- dat_lab %>% 
   filter(risk_factors__observed_reported=="Yes" & year_cut=="2000+") %>%
   dplyr::group_by(immunosupression) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id))
   )
)

# by infectious diseases new
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="2000+") %>%
    dplyr::group_by(infectious_diseases) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

# by nutritional status
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="2000+") %>%
    dplyr::group_by(nutritional_deficit) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

# by ID
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="2000+") %>%
    dplyr::group_by(infectious_diseases) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

##########################################################
## Number of papers describing a given risk factor
##########################################################

#-------------------------------------------
# remove hidden characters
#-------------------------------------------
dat_lab <-  dat_lab %>% 
  mutate(risk_factors_details = replace_non_ascii(str_trim(risk_factors_details)))

#----------------------
# nutritional_deficit
#------------------------
(n_rf_nutri <- dat_lab %>% 
    #filter(nutritional_deficit!="Unknown") %>% 
    dplyr::group_by(nutritional_deficit) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_nutri <- dat_lab %>% 
    filter(nutritional_deficit=="Yes") %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))) %>% 
    mutate(risk ="Nutritional deficit")
)

#-------------------------------------------
# nutritional deficit in the context of HIV
#-------------------------------------------
hiv_detect <- paste(c('HIV', 'hiv','HIV/AIDS','AIDS', 'immunodeficiency virus',
                      ' human immunodeficiency virus'), collapse = "|")

n_nutri_hiv <- dat_lab %>% 
  filter(nutritional_deficit=="Yes") %>% 
  mutate(hiv_flag = case_when(
    str_detect(risk_factors_details, hiv_detect) ~ 1, 
    TRUE ~ 0 )) %>% 
  select(year_cut, nutritional_deficit,risk_factors_details,hiv_flag) 

sum(n_nutri_hiv$hiv_flag)
(sum(n_nutri_hiv$hiv_flag)/n_rf_reported)*100

#----------------------
# HIV in general
#----------------------
n_hiv <- dat_lab %>% 
  mutate(hiv_all_flag = case_when(
    str_detect(risk_factors_details, hiv_detect) ~ 1, 
    TRUE ~ 0 )) %>% 
  select(year_cut, risk_factors_details,hiv_all_flag) 

table(n_hiv$year_cut,n_hiv$hiv_all_flag)

#-------------------------------------------
# marasmus;kwashiorkor
#-------------------------------------------
nutri_detect <- paste(c('kwashiorkor', 'marasmus','underweight','stunting', 'wasting',
                      'pre-kwashiorkor','wasted','stunted'), collapse = "|")

#----------------------
# Infectious diseases
#------------------------
(n_rf <- dat_lab %>% 
    dplyr::group_by(infectious_diseases) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_id <- dat_lab %>% 
    filter(infectious_diseases=="Yes") %>% 
    dplyr::group_by(continent) %>%
      dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))) %>% 
    mutate(risk ="Infectious disease comorbidities")
)

#-------------------------------------------
# Measles
#-------------------------------------------
measles_detect <- paste(c('measles','measel','Measles'), collapse = "|")

n_measles <- dat_lab %>% 
  filter(infectious_diseases=="Yes") %>% 
  mutate(measles_flag = case_when(
    str_detect(risk_factors_details, measles_detect) ~ 1, 
    TRUE ~ 0 )) %>% 
  select(infectious_diseases,risk_factors_details,year_cut,measles_flag) 
sum(n_measles$measles_flag)

#------------------------
# Other clinical history
#------------------------
(n_rf <- dat_lab %>% 
    dplyr::group_by(other_diseases_clinical_history) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),    
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_other_clinical <- dat_lab %>% 
    filter(other_diseases_clinical_history=="Yes") %>% 
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))) %>% 
    mutate(risk ="Other diseases or clinical comorbidities")
)

#------------------------
# Poverty/SES
#------------------------
(n_rf <- dat_lab %>% 
    dplyr::group_by(Poverty) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_poverty <- dat_lab %>% 
    filter(Poverty=="Yes") %>% 
    dplyr::group_by(continent) %>%
        dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))) %>% 
    mutate(risk ="Poverty")
)

#-------------------------------------------
# immunosupression
#-------------------------------------------
(n_rf <- dat_lab %>% 
    dplyr::group_by(immunosupression) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_immunosupression <- dat_lab %>% 
    filter(immunosupression=="Yes") %>% 
    dplyr::group_by(continent) %>%
    
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))) %>% 
    mutate(risk ="Immunosupression {a}")
)

#----------------------------------------------------------
# Number of studies with  HIV as immune supression
#----------------------------------------------------------
immune_hiv_detect <- paste(c('HIV', 'hiv','HIV/AIDS','AIDS', 'immunodeficiency virus',
                      ' human immunodeficiency virus'), collapse = "|")

n_immune_hiv <- dat_lab %>% 
  filter(immunosupression=="Yes") %>% 
  mutate(hiv_flag = case_when(
    str_detect(risk_factors_details, immune_hiv_detect) ~ 1, 
    TRUE ~ 0 )) %>% 
  select(immunosupression,risk_factors_details,hiv_flag) 
sum(n_immune_hiv$hiv_flag)

#-------------------------------------------
# cancer
#-------------------------------------------
cancer_detect <- paste(c('cancer','leukemia','luekemia','luekemia','Chemotherapy',
                         'luekaemia','leukaemia'), collapse = "|")

leukaemia_detect <- paste(c('leukemia','luekemia','luekemia',
                         'luekaemia','leukaemia'), collapse = "|")

n_immune_cancer <- dat_lab %>% 
  filter(immunosupression=="Yes") %>% 
  mutate(cancer_flag = case_when(
    str_detect(risk_factors_details, cancer_detect) ~ 1, 
    TRUE ~ 0 )) %>% 
  select(age,immunosupression,risk_factors_details,cancer_flag) %>% 
  filter(cancer_flag==1)
sum(n_immune_cancer$cancer_flag)
table(n_immune_cancer$age)

n_immune_leukaemia <- dat_lab %>% 
  filter(immunosupression=="Yes") %>% 
  mutate(leukaemia_flag = case_when(
    str_detect(risk_factors_details, leukaemia_detect) ~ 1, 
    TRUE ~ 0 )) %>% 
  select(age,immunosupression,risk_factors_details,leukaemia_flag) %>% 
  filter(leukaemia_flag==1)
sum(n_immune_leukaemia$leukaemia_flag)
table(n_immune_leukaemia$age)

#----------------------
# Poor oral hygiene
#------------------------
(n_rf_poor_oral_hygiene <- dat_lab %>% 
    dplyr::group_by(poor_oral_hygiene) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      min_year = min(year),
      max_year = max(year),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

n_rf_poor_oral_hygiene <- dat_lab %>% 
              filter(poor_oral_hygiene=="Yes")

(n_oral <- dat_lab %>% 
    filter(poor_oral_hygiene=="Yes") %>% 
    dplyr::group_by(continent) %>%
    
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))) %>% 
    mutate(risk ="Poor oral hygiene")
)

#------------------------
# Respiratory infections
#------------------------
(n_rf <- dat_lab %>% 
    dplyr::group_by(Respiratory_infections) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_resp <- dat_lab %>% 
    filter(Respiratory_infections=="Yes") %>% 
    dplyr::group_by(continent) %>%
        dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))) %>% 
    mutate(risk ="Respiratory infections")
)

#-------------------------------------------
# penumopathy
#-------------------------------------------
pneumo_detect <- paste(c('bronchopneumonia','pneumonia','pneumopathie','broncho-pnuemonitis'), collapse = "|")

n_pneumo <- dat_lab %>% 
  filter(Respiratory_infections=="Yes") %>% 
  mutate(pneumo_flag = case_when(
    str_detect(risk_factors_details, pneumo_detect) ~ 1, 
    TRUE ~ 0 )) %>% 
  select(Respiratory_infections,risk_factors_details,pneumo_flag)
sum(n_pneumo$pneumo_flag)

#------------------------
# physical environment
#------------------------
(n_rf_pe <- dat_lab %>% 
    dplyr::group_by(physical_enviroment) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_rf_pe <- dat_lab %>% 
    filter(physical_enviroment=="Yes") %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      min_year = min(year),
      max_year = min(year),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_physical_env <- dat_lab %>% 
    filter(physical_enviroment=="Yes") %>% 
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))) %>% 
    mutate(risk ="Physical enviroment")
)

#------------------------
# lifestyle factors
#------------------------
(n_rf_lifestyle <- dat_lab %>% 
    dplyr::group_by(lifestyle_factors) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_rf_lifestyle <- dat_lab %>% 
    filter(lifestyle_factors=="Yes") %>%
    dplyr::group_by(age_group) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_lifestyles <- dat_lab %>% 
    filter(lifestyle_factors=="Yes") %>% 
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))) %>% 
    mutate(risk ="Lifestyle factors")
)

#=============================================================
# collate the number of studies with a given risk factor
#=============================================================
risk_groups <- rbind(
                n_lifestyles,
                n_physical_env, 
                n_resp,
                n_oral, 
                n_immunosupression,
                n_poverty,
                n_other_clinical,
                n_id,
                n_nutri
            )

risk_groups <- risk_groups %>% 
                filter(continent !=" ")

write.csv(risk_groups, "n_patients_risk_factors_by_region_time_june23.csv", row.names=FALSE)

#---------------------------------
# Export as stacked barchart
#---------------------------------
tiff(file="S3_risk_factors_by_region_studies_june23.tiff", 
     width=46, height=24, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none")

ggbarplot(risk_groups, 
          x = "continent",
          y = "number_of_articles",
          fill = "risk",
          #palette = brewer.pal(n = 9, name = "Set2"),                             # Color by groups
          sorting = "descending",                       # Sort value in descending order
          rotate = FALSE,                                # Rotate vertically
          ggtheme = theme_pubr()                        # ggplot2 theme
)  +
  ylab("Number of studies (Not unique)") +
  #scale_y_log10() +
  xlab("")+
  guides(fill=guide_legend(title=""), size=12)+
  theme(legend.text=element_text(size=rel(8)))+
  theme(legend.key.size = unit(1.6, "cm"))+
  theme(text = element_text(size=rel(2.1)),
        strip.text.x = element_text(size=rel(1.7)),
        strip.text.y = element_text(size=rel(1.7)))+
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "right")

dev.off()

## END
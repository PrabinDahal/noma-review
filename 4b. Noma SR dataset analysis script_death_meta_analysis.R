################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Summarise mortality data and undertake meta-analysis
################################################################################
#rm(list=ls())
library(pacman)
pacman::p_load(readxl,tidyverse,ggpubr,purrr,readr,stringr,patchwork,table1,doBy,
               gtools,janitor,ggbreak,ggthemes, RColorBrewer,wesanderson,viridis,forcats,tibble,tidyr,
               dplyr,maps,rworldmap,rworldxtra,naniar,cowplot,maptools,gridExtra,classInt, 
               car,ggpubr,ggalt,plyr,IDPmisc,rms,epitools,textclean,waffle,ggwaffle,cowplot,
               ggthemes,meta,metasens,binom)

# Working directory
setwd("C:/IDDO_Other/Noma/Data")

#==============================
# Read the analysis dataset
#==============================
dat_lab<-read.csv("analysis_data_20_03_2023.csv")

#---------------------------
# Total number of studies
#---------------------------
(total_studies <- dat_lab %>%
   dplyr::summarise(n_studies = length(unique(row_id))
   ))

#-----------------------------------------------------------------------
# Total number of articles by risk factors reported by time-period
#-----------------------------------------------------------------------
dat_lab <- dat_lab %>% 
        mutate (year_cut = case_when(
          year <1950 ~ "Pre-1950",
          year >=1950 & year <2000 ~ "1950-2000",
          year >=2000 ~ "2000+"
        ))
dat_lab$n_deaths[dat_lab$n_deaths ==-99]<- NA

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
                                   "1975-1999","2000-2009","Post-2010"
)

## Keep only study that reported deaths
dat_lab <- dat_lab %>% 
    filter(!is.na(n_deaths))

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
    #dplyr::group_by(design) %>%
    dplyr::summarise(
      n_patients = sum(n_patients, na.rm=T),
      n_studies = length(unique(row_id)),
      n_deaths = sum(n_deaths, na.rm=T)
    )) 

#============================================================================
# Total number of deaths reported by study sites; including temporal map
#============================================================================
(n_rf <- dat_lab %>% 
   dplyr::group_by(year <=1928) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id)),
     number_of_deaths = sum(n_deaths, na.rm=T),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

dat_lab$design[dat_lab$design=="Interventional (RCT or non-randomised clinical study)"] <- "Interventional"

dat_lab <- dat_lab %>% 
  mutate(death_prop = (n_deaths/n_patients)*100) 

#---------------------------------------------
# Keep only cohort or intervention studies
#---------------------------------------------
cohort <- dat_lab %>% 
  filter(design %in% c("Cohort","Interventional"))

(deaths_by_design <- cohort %>% 
    filter(!is.na(n_deaths)) %>% 
    filter(!is.na(n_patients)) %>% 
    filter(surgical_intervention_facial!="Yes") %>% 
    filter(surgical_interventions_aside_from_facial_reconstruction!="Yes") %>% 
    dplyr::group_by(design) %>%
    dplyr::summarise(
      n_patients = sum(n_patients, na.rm=T),
      n_articles = length(unique(row_id)),
      n_deaths = sum(n_deaths, na.rm=T),
      median_deaths = median(death_prop)
    ))

#------------------
# forest plot
#------------------
require(binom)
cohort <- cohort %>% 
  mutate(
   lower= binom.confint(c(cohort$n_deaths),c(cohort$n_patients), methods = "w")[,5]*100,
    upper=binom.confint(c(cohort$n_deaths),c(cohort$n_patients), methods = "w")[,6]*100
  )
summary(cohort$death_prop)

tiff(file="C:/IDDO_Other/Noma/Results/out5_deaths/2023_Forest_plot_for_mortality.tiff", 
     width=36, height=26, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=600, antialias = "none" )

ggplot(cohort, aes(reorder(study, -death_prop), y=death_prop))+
      geom_point(pch=19, size=6,aes(colour = factor(design)))+ 
      geom_errorbar(aes(ymin = lower, ymax = upper),width = 0.3, lwd=0.9)+
        ylab("Case fatality")+
        xlab("Study")+
        coord_flip()+
        ggtitle("")+
        theme_tufte()+
      guides(color = guide_legend(title = "Design"))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"))

dev.off()

#================================================================
# Keep only those cohort studies that excluded surgery patients
#================================================================
cohort1 <- dat_lab %>% 
  filter(design %in% c("Cohort","Interventional")) %>% 
  filter(surgical_intervention_facial!="Yes") %>% 
  filter(surgical_interventions_aside_from_facial_reconstruction!="Yes")
summary(cohort1$death_prop)

# forest plot
cohort1 <- cohort1 %>% 
  mutate(
    lower= binom.confint(c(cohort1$n_deaths),c(cohort1$n_patients), methods = "w")[,5]*100,
    upper=binom.confint(c(cohort1$n_deaths),c(cohort1$n_patients), methods = "w")[,6]*100
  )
summary(cohort1$death_prop)

tiff(file="C:/IDDO_Other/Noma/Results/out5_deaths/2023_Forest_plot_for_mortality_no_surgery.tiff", 
     width=26, height=12, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=600, antialias = "none" )

ggplot(cohort1, aes(reorder(study, -death_prop), y=death_prop))+
  geom_point(pch=19, size=6,aes(colour = factor(design)))+ 
  geom_errorbar(aes(ymin = lower, ymax = upper),width = 0.3, lwd=0.9)+
  ylab("Case fatality")+
  xlab("Study")+
  coord_flip()+
  ggtitle("")+
  theme_tufte()+
  guides(color = guide_legend(title = "Design"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

dev.off()

#=================================================
# Meta-analysis of cohort studies (n=37 studies)
#=================================================
(overall_deaths <- cohort %>% 
    dplyr::group_by(design) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T)
    )) 

(meta.prop <- metaprop(
                data = cohort,
                n_deaths,
                n_patients, 
                prediction=TRUE,
                studlab = study, 
                hakn=TRUE
            )
      )
#----------------------------------------------
# Adjusted for potential publication biases 
#----------------------------------------------
funnel(meta.prop)
metabias(meta.prop)
trimfill(meta.prop)
plot(copas(meta.prop))
copas(meta.prop)

#===============================================================================
# Meta-analysis of cohort studies: excluding surgery studies (n=12 studies)
#===============================================================================
(overall_deaths1 <- cohort1 %>% 
    dplyr::group_by(time_period) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      median_prop = median(death_prop),
      min_prop = min(death_prop),
      max_prop = max(death_prop)
    )) 

(meta.prop1 <- metaprop(
  data = cohort1,
  n_deaths,
  n_patients, 
  prediction=TRUE,
  studlab = study, 
  hakn=TRUE
))

update(meta.prop1, by=time_period)

#-----------------------------------------------
# Adjusted for potential publication biases 
#-----------------------------------------------
funnel(meta.prop1)
metabias(meta.prop1)
trimfill(meta.prop1)
copas(meta.prop1)
plot(copas(meta.prop1))

####################################
# Distribution of risk factors
####################################
#-----------------------------
# Risk factors prior to 1950s
#-----------------------------
(n_rf <- dat_lab %>% 
   dplyr::group_by(year <=1928) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id)),
     number_of_deaths = sum(n_deaths, na.rm=T),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

(n_rf <- dat_lab %>% 
    dplyr::group_by(age_group) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_rf <- dat_lab %>% 
    dplyr::group_by(WHO_noma_stage) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

#-----------------------------
# Risk factors 1950-2000
#-----------------------------
(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="1950-2000") %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

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
(n_patients_risk_factors <- dat_lab %>% 
   filter(risk_factors__observed_reported=="Yes" & year_cut=="2000+") %>%
   dplyr::group_by(continent) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id))
   )
)

(n_patients_risk_factors <- dat_lab %>% 
   filter(risk_factors__observed_reported=="Yes" & year_cut=="2000+") %>%
   dplyr::group_by(immunosupression) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id))
   )
)

(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="2000+") %>%
    dplyr::group_by(nutritional_deficit) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

(n_patients_risk_factors <- dat_lab %>% 
    filter(risk_factors__observed_reported=="Yes" & year_cut=="2000+") %>%
    dplyr::group_by(infectious_diseases) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id))
    )
)

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

table(n_rf_poor_oral_hygiene$country)

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

write.csv(n_rf_pe, "I:/Prabin/Noma/Results/phys_environ_yes.csv", row.names=FALSE)

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

## END

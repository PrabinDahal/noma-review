################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Summarise treatment used
################################################################################
#rm(list=ls())
library(pacman)
pacman::p_load(readxl,tidyverse, ggpubr,purrr, readr,stringr,patchwork,
               table1,doBy,gtools,janitor,RColorBrewer,wesanderson,viridis,forcats, 
               tibble,tidyr,dplyr,maps,rworldmap,rworldxtra,naniar,cowplot,maptools,gridExtra,
               classInt,car,ggpubr,ggalt,plyr,IDPmisc,rms,epitools,textclean,
               waffle,ggwaffle,ggbreak
               )

# Working directory
setwd("C:/IDDO_Other/Noma/Data")

# Read the analysis database
dat_lab<-read.csv("analysis_data_20_03_2023.csv")

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

#########################################
# Summarise the intervention approaches
#########################################

# set output directory
setwd("C:/IDDO_Other/Noma/Results/out4_drug")

#-----------------------------
# Total number of studies
#-----------------------------
(total_studies <- dat_lab %>%
   dplyr::summarise(n_studies = length(unique(row_id))
   ))

#------------------------------------------------------------------------
# Total number of articles and patients for each of the intervention
#------------------------------------------------------------------------
int1<-  dat_lab %>% 
  dplyr::group_by(antiobotic_therapy) %>%
  filter(antiobotic_therapy =="Yes") %>% 
  dplyr::summarise(
    number_of_patients = sum(n_patients, na.rm=T),
    number_of_articles = length(unique(row_id))
  )
int1 <- as.matrix(int1)
rownames(int1) <- c("Antibotics")

int2<-  dat_lab %>% 
  dplyr::group_by(medical_intervention_other_than_antibiotic_thereapy) %>%
  filter(medical_intervention_other_than_antibiotic_thereapy=="Yes") %>% 
  dplyr::summarise(
    number_of_patients = sum(n_patients, na.rm=T),
    number_of_articles = length(unique(row_id))
  )
int2 <- as.matrix(int2)
rownames(int2) <- c("Other medical therapy")

int3<-  dat_lab %>% 
  dplyr::group_by(alternative_therapy) %>%
  filter(alternative_therapy=="Yes") %>% 
  dplyr::summarise(
    number_of_patients = sum(n_patients, na.rm=T),
    number_of_articles = length(unique(row_id))
  )
int3 <- as.matrix(int3)
rownames(int3) <- c("Alternative therapy")

int4<-  dat_lab %>% 
  dplyr::group_by(surgical_intervention_facial) %>%
  filter(surgical_intervention_facial=="Yes") %>% 
  dplyr::summarise(
    number_of_patients = sum(n_patients, na.rm=T),
    number_of_articles = length(unique(row_id))
  )
int4 <- as.matrix(int4)
rownames(int4) <- c("Surgical reconstruction")

int5<-  dat_lab %>% 
  dplyr::group_by(surgical_interventions_aside_from_facial_reconstruction) %>%
  filter(surgical_interventions_aside_from_facial_reconstruction=="Yes") %>% 
  dplyr::summarise(
    number_of_patients = sum(n_patients, na.rm=T),
    number_of_articles = length(unique(row_id))
  )
int5 <- as.matrix(int5)
rownames(int5) <- c("Other surgical intervention")

# Merge the summary table together
int_x <- as.data.frame(
  t( 
    cbind(
      t(int1),
      t(int2),
      t(int3),
      t(int4),
      t(int5)
    )
  )
)

# Tidy up the variable naming now
int_x$intervention <- row.names(int_x)
int_x$antiobotic_therapy <- NULL
rownames(int_x) <- c()
int_x$number_of_patients <- as.numeric(as.character(int_x$number_of_patients))
int_x$number_of_articles <- as.numeric(as.character(int_x$number_of_articles))

#=============================
# side by side lollipop chart
#=============================
int_x$intervention  <- factor(int_x$intervention , 
      levels = c(
        "Other medical therapy", 
        "Antibotics", 
        "Other surgical intervention", 
        "Surgical reconstruction", 
        "Alternative therapy")
      )

a <- ggdotchart(int_x, 
                x = "intervention",
                y = "number_of_articles",
                color = "#00AFBB",                                		# Color by groups
                sorting = "descending",                       
                add = "segments",                             			# Add segments from y = 0 to dots
                rotate = TRUE,                                
                dot.size = 8,                                 
                label = int_x$number_of_articles,                        
                font.label = list(color = "white", size = 9, 
                                  vjust = 0.5),               
                ggtheme = theme_pubr()                        
)+
  ylab("Number of publications") +
  ylim(0,250)+
  xlab("")+
  ggtitle("")+
  theme(axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

b<- ggdotchart(int_x, 
               x = "intervention",
               y = "number_of_patients",
               color = "#00AFBB",                                		# Color by groups
               sorting = "descending",                       
               add = "segments",                             			# Add segments from y = 0 to dots
               rotate = TRUE,                                
               dot.size = 10,                                 
               label = int_x$number_of_patients,                        
               font.label = list(color = "white", size = 9, 
                                 vjust = 0.5),               
               ggtheme = theme_pubr()                        
)+
  ylab("Number of patients") +
  ylim(0,10000)+
  xlab("")+
  ggtitle("")+
  theme(axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

tiff(file="S4_n_patients_publications_by_intervention.tiff", 
     width=30, height=14, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none")

a+b
dev.off()

#====================================
# Specific treatment description
#====================================
dat_lab$n_deaths[dat_lab$n_deaths==-99] <- NA
summary(dat_lab$n_deaths)
sum(dat_lab$n_deaths, na.rm=TRUE)

#========================================================
# Total number of articles by ABx therapy
#========================================================
(n_abx <- dat_lab %>% 
   filter(antiobotic_therapy=="Yes") %>%
   dplyr::summarise(
     number_of_articles = length(unique(row_id)),
     number_of_patients = sum(n_patients, na.rm=T)
   ))

(n_abx_design <- dat_lab %>% 
   filter(antiobotic_therapy=="Yes") %>%
   dplyr::group_by(design) %>%
   dplyr::summarise(
     number_of_articles = length(unique(row_id)),
     number_of_patients = sum(n_patients, na.rm=T)
   ))

(n_abx_time_period <- dat_lab %>% 
    filter(antiobotic_therapy=="Yes") %>%
    dplyr::group_by(time_period) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_abx_region <- dat_lab %>% 
    filter(antiobotic_therapy=="Yes") %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_abx_stage <- dat_lab %>% 
    filter(antiobotic_therapy=="Yes") %>%
    dplyr::group_by(WHO_noma_stage) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

#========================================================
# Total number of articles by other than ABx therapy
#========================================================
(n_other_therapy <- dat_lab %>% 
    dplyr::group_by(medical_intervention_other_than_antibiotic_thereapy) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_other_therapy_design <- dat_lab %>% 
    filter(medical_intervention_other_than_antibiotic_thereapy=="Yes") %>%
    dplyr::group_by(design) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_other_therapy_time_period <- dat_lab %>% 
    filter(medical_intervention_other_than_antibiotic_thereapy=="Yes") %>%
    dplyr::group_by(time_period) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_other_therapy_region <- dat_lab %>% 
    filter(medical_intervention_other_than_antibiotic_thereapy=="Yes") %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_other_therapy_stage <- dat_lab %>% 
    filter(medical_intervention_other_than_antibiotic_thereapy=="Yes") %>%
    dplyr::group_by(WHO_noma_stage) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

#========================================================
# Total number of articles by alternative therapy
#========================================================
(n_alt_therapy <- dat_lab %>% 
    dplyr::group_by(alternative_therapy) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_alt_therapy_design <- dat_lab %>% 
    filter(alternative_therapy=="Yes") %>%
    dplyr::group_by(design) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_alt_time_period <- dat_lab %>% 
    filter(alternative_therapy=="Yes") %>%
    dplyr::group_by(time_period) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_alt_therapy_region <- dat_lab %>% 
    filter(alternative_therapy=="Yes") %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_alt_therapy_stage <- dat_lab %>% 
    filter(alternative_therapy=="Yes") %>%
    dplyr::group_by(WHO_noma_stage) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

#========================================================
# Total number of articles by surgical intervention
#========================================================
(n_surgery <- dat_lab %>% 
    dplyr::group_by(surgical_intervention_facial) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_surgery_design <- dat_lab %>% 
    filter(surgical_intervention_facial=="Yes") %>%
    dplyr::group_by(design) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_surgery_time_period <- dat_lab %>% 
    filter(surgical_intervention_facial=="Yes") %>%
    dplyr::group_by(time_period) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_surgery_region <- dat_lab %>% 
    filter(surgical_intervention_facial=="Yes") %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_surgery_stage <- dat_lab %>% 
    filter(surgical_intervention_facial=="Yes") %>%
    dplyr::group_by(WHO_noma_stage) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_surgery_stage <- dat_lab %>% 
    filter(surgical_intervention_facial=="Yes" & WHO_noma_stage=="2. Oedema Stage") 
    )

#==============================================================================
# Total number of articles by surgical intervention with facial reconstruction
#==============================================================================
(n_surg_face <- dat_lab %>% 
    dplyr::group_by(surgical_interventions_aside_from_facial_reconstruction) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T),
      percentage = round((number_of_articles/total_studies)*100,2)
    )) %>% 
  arrange(desc(percentage))

(n_surg_face_design <- dat_lab %>% 
    filter(surgical_interventions_aside_from_facial_reconstruction=="Yes") %>%
    dplyr::group_by(design) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_surg_face_time_period <- dat_lab %>% 
    filter(surgical_interventions_aside_from_facial_reconstruction=="Yes") %>%
    dplyr::group_by(time_period) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_surg_face_region <- dat_lab %>% 
    filter(surgical_interventions_aside_from_facial_reconstruction=="Yes") %>%
    dplyr::group_by(continent) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

(n_surg_face_stage <- dat_lab %>% 
    filter(surgical_interventions_aside_from_facial_reconstruction=="Yes") %>%
    dplyr::group_by(WHO_noma_stage) %>%
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

#==============================================================================
# Total number of articles by treatment modality
#==============================================================================
(n_patients_trt_modal <- dat_lab %>% 
   dplyr::group_by(treatment_modality) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id)),
     number_of_deaths = sum(n_deaths, na.rm=T),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

#-------------------------------------------------------------
# Total number of articles by study design & trt modality
#-------------------------------------------------------------
(n_patients_trt_modal_design <- dat_lab %>% 
    dplyr::group_by(treatment_modality,design) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))
write.csv(n_patients_trt_modal_design, "n_patients_by_treatment_modality_and_design.csv", row.names=FALSE)

#-------------------------------------------------------------
# Total number of articles by time period & trt modality
#-------------------------------------------------------------
dat_lab <- dat_lab %>% 
  mutate (treated = case_when(
    treatment_modality %in% c("Medical","Surgical","Both") ~ "Yes",
    treatment_modality %in% c("Neither") ~ "No",
    treatment_modality %in% c("Unknown") ~ "Unknown"
  ))

(n_patients_trt_modal_time <- dat_lab %>% 
   dplyr::group_by(treatment_modality,time_period) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id)),
     number_of_deaths = sum(n_deaths, na.rm=T)
   ))
write.csv(n_patients_trt_modal_time, "n_patients_trt_modal_time.csv", row.names=FALSE)

(n_patients_trt_modal_time2 <- dat_lab %>% 
    dplyr::group_by(time_period, treated) %>%
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))
write.csv(n_patients_trt_modal_time2, "n_patients_trt_status_time.csv", row.names=FALSE)

#====================================================================
# Total number of articles by period & noma stages & treat modality
#====================================================================
# generate summary by stage and time            
(n_patients_stage_modality <- dat_lab %>% 
   dplyr::group_by(WHO_noma_stage,time_period, treatment_modality) %>%
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id)),
     number_of_deaths = sum(n_deaths, na.rm=T)
   ))
write.csv(n_patients_stage_modality, "n_patients_stage_trt_modality_time.csv", row.names=FALSE)

#------------------------------
# Export as tiff file
#------------------------------
tiff(file="S4_facet_plot_n_pubs_noma_stages_by_time.tiff", 
     width=56, height=34, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none")

ggbarplot(n_patients_stage_modality %>%  filter(WHO_noma_stage!="Unclear"), 
          x = "time_period",
          y = "number_of_articles",
          fill = "treatment_modality",
          facet.by = "WHO_noma_stage",
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
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "right")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))

dev.off()

#------------------------------
# Export as tiff file
#------------------------------
tiff(file="S4_facet_plot_n_patients_noma_stages_by_time.tiff", 
     width=56, height=34, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none")

ggbarplot(n_patients_stage_modality %>%  filter(WHO_noma_stage!="Unclear"),  
          x = "time_period",
          y = "number_of_patients",
          fill = "treatment_modality",
          facet.by = "WHO_noma_stage",
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
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "right")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))

dev.off()

##################################################################
# Total number of articles by period & specific interventions
##################################################################
# ABx therapy
(n_patients_tx_time <- dat_lab %>% 
   dplyr::group_by(antiobotic_therapy,time_period) %>%
    filter(antiobotic_therapy=="Yes") %>% 
   dplyr::summarise(
     number_of_patients = sum(n_patients, na.rm=T),
     number_of_articles = length(unique(row_id)),
     number_of_deaths = sum(n_deaths, na.rm=T)
   ))

# other medical therapy
(n_patients_tx_time <- dat_lab %>% 
    dplyr::group_by(medical_intervention_other_than_antibiotic_thereapy,time_period) %>%
    filter(medical_intervention_other_than_antibiotic_thereapy=="Yes") %>% 
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))

# alternative therapy
(n_patients_tx_time <- dat_lab %>% 
    dplyr::group_by(alternative_therapy,time_period) %>%
    filter(alternative_therapy=="Yes") %>% 
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))

# surgical_intervention_facial
(n_patients_tx_time <- dat_lab %>% 
    dplyr::group_by(surgical_intervention_facial,time_period) %>%
    filter(surgical_intervention_facial=="Yes") %>% 
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))

# surgical_interventions_aside_from_facial_reconstruction
(n_patients_tx_time <- dat_lab %>% 
    dplyr::group_by(surgical_interventions_aside_from_facial_reconstruction,time_period) %>%
    filter(surgical_interventions_aside_from_facial_reconstruction=="Yes") %>% 
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))

###############################################################################
# Total number of articles by period & specific interventions & staging
# Then use plot_grid to export as panels
###############################################################################
# ABx therapy
(n_a1 <- dat_lab %>% 
    dplyr::group_by(antiobotic_therapy,WHO_noma_stage,time_period) %>%
    filter(antiobotic_therapy=="Yes") %>% 
    dplyr::summarise(
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_articles = length(unique(row_id)),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))

# other medical therapy
(n_a2 <- dat_lab %>% 
    dplyr::group_by(medical_intervention_other_than_antibiotic_thereapy,WHO_noma_stage, time_period) %>%
    filter(medical_intervention_other_than_antibiotic_thereapy=="Yes") %>% 
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))

# alternative therapy
(n_a3 <- dat_lab %>% 
    dplyr::group_by(alternative_therapy,WHO_noma_stage, time_period) %>%
    filter(alternative_therapy=="Yes") %>% 
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))

# surgical_intervention_facial
(n_a4 <- dat_lab %>% 
    dplyr::group_by(surgical_intervention_facial,WHO_noma_stage, time_period) %>%
    filter(surgical_intervention_facial=="Yes") %>% 
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))

# surgical_interventions_aside_from_facial_reconstruction
(n_a5 <- dat_lab %>% 
    dplyr::group_by(surgical_interventions_aside_from_facial_reconstruction,WHO_noma_stage, time_period) %>%
    filter(surgical_interventions_aside_from_facial_reconstruction=="Yes") %>% 
    dplyr::summarise(
      number_of_articles = length(unique(row_id)),
      number_of_patients = sum(n_patients, na.rm=T),
      number_of_deaths = sum(n_deaths, na.rm=T)
    ))

# plot
a1 <- ggbarplot(n_a1, 
                x = "time_period",
                y = "number_of_patients",
                fill = "WHO_noma_stage",
                #facet.by = "WHO_noma_stage",
                #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Number of patients") +
  ggtitle("Antibiotics") +
  #scale_y_log10() +
  xlab(" ")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "right")+
  theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=18))

a2 <- ggbarplot(n_a2, 
                x = "time_period",
                y = "number_of_patients",
                fill = "WHO_noma_stage",
                #facet.by = "WHO_noma_stage",
                #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("") +
  ggtitle("Other medical therapy") +
  #scale_y_log10() +
  xlab(" ")+
  #guides(fill=guide_legend(title=""), size=30)+
  #theme(legend.text=element_text(size=rel(7.5)))+
  #theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=18))

a3 <- ggbarplot(n_a3, 
                x = "time_period",
                y = "number_of_patients",
                fill = "WHO_noma_stage",
                #facet.by = "WHO_noma_stage",
                #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("") +
  ggtitle("Alternative therapy") +
  #scale_y_log10() +
  xlab(" ")+
  #guides(fill=guide_legend(title=""), size=30)+
  #theme(legend.text=element_text(size=rel(7.5)))+
  #theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=18))

a4 <- ggbarplot(n_a4, 
                x = "time_period",
                y = "number_of_patients",
                fill = "WHO_noma_stage",
                #facet.by = "WHO_noma_stage",
                #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Number of patients") +
  ggtitle("Surgical reconstruction") +
  #scale_y_log10() +
  xlab(" ")+
  #guides(fill=guide_legend(title=""), size=30)+
  #theme(legend.text=element_text(size=rel(7.5)))+
  #theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=18))

a5 <- ggbarplot(n_a5, 
                x = "time_period",
                y = "number_of_patients",
                fill = "WHO_noma_stage",
                #facet.by = "WHO_noma_stage",
                #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("") +
  ggtitle("Other surgical procedure") +
  #scale_y_log10() +
  xlab(" ")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "right")+
  theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=18))


tiff(file="S4_noma_interventions_by_staging.tiff", 
     width=56, height=34, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none")

plot_grid(a1,a2,a3,a4,a5)

dev.off()

##################################################################################
# Plot the number of studies by Noma stage, period, sub-group of INT type
##################################################################################
# plot
a1 <- ggbarplot(n_a1, 
                x = "time_period",
                y = "number_of_articles",
                fill = "WHO_noma_stage",
                #facet.by = "WHO_noma_stage",
                #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Number of studies") +
  ggtitle("Antibiotics") +
  #scale_y_log10() +
  xlab(" ")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "right")+
  theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=18))+
  ylim(0,60)

a2 <- ggbarplot(n_a2, 
                x = "time_period",
                y = "number_of_articles",
                fill = "WHO_noma_stage",
                #facet.by = "WHO_noma_stage",
                #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("") +
  ggtitle("Other medical therapy") +
  #scale_y_log10() +
  xlab(" ")+
  #guides(fill=guide_legend(title=""), size=30)+
  #theme(legend.text=element_text(size=rel(7.5)))+
  #theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=18))+
  ylim(0,60)

a3 <- ggbarplot(n_a3, 
                x = "time_period",
                y = "number_of_articles",
                fill = "WHO_noma_stage",
                #facet.by = "WHO_noma_stage",
                #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("") +
  ggtitle("Alternative therapy") +
  #scale_y_log10() +
  xlab(" ")+
  #guides(fill=guide_legend(title=""), size=30)+
  #theme(legend.text=element_text(size=rel(7.5)))+
  #theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=18))+
  ylim(0,60)

a4 <- ggbarplot(n_a4, 
                x = "time_period",
                y = "number_of_articles",
                fill = "WHO_noma_stage",
                #facet.by = "WHO_noma_stage",
                #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Number of studies") +
  ggtitle("Surgical reconstruction") +
  #scale_y_log10() +
  xlab(" ")+
  #guides(fill=guide_legend(title=""), size=30)+
  #theme(legend.text=element_text(size=rel(7.5)))+
  #theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=18))+
  ylim(0,60)

a5 <- ggbarplot(n_a5, 
                x = "time_period",
                y = "number_of_articles",
                fill = "WHO_noma_stage",
                #facet.by = "WHO_noma_stage",
                #palette = brewer.pal(n = 7, name = "Set2"),                             # Color by groups
                sorting = "descending",                       # Sort value in descending order
                rotate = FALSE,                                # Rotate vertically
                ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("") +
  ggtitle("Other surgical procedure") +
  #scale_y_log10() +
  xlab(" ")+
  guides(fill=guide_legend(title=""), size=20)+
  theme(legend.text=element_text(size=rel(7.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(2.2)),
        strip.text.x = element_text(size=rel(2.2)),
        strip.text.y = element_text(size=rel(5.2)))+
  theme(
    axis.title = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30))+
  theme(legend.position = "right")+
  theme(legend.position = c(0.29,0.7))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 25))+
  font("title", size = 22)+
  theme(axis.text=element_text(size=18))+
  ylim(0,60)

tiff(file="S4_noma_interventions_by_staging_studies.tiff", 
     width=56, height=34, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

plot_grid(a1,a2,a3,a4,a5)
dev.off()

## END
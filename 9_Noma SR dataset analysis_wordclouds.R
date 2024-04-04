##########################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Generate word-clouds for risk factors, microbiology and treatment
##########################################################################################
#rm(list=ls())
library(pacman)

pacman::p_load(readxl,tidyverse, ggpubr,purrr, readr, stringr,patchwork,table1,doBy,gtools,
               janitor,RColorBrewer,wesanderson,viridis,forcats, tibble,tidyr,dplyr,
               maps,rworldmap,rworldxtra,naniar,cowplot,maptools,gridExtra,ggbreak,
               classInt, car,ggpubr,ggplot2,ggalt,plyr,IDPmisc,rms,epitools,
               waffle, ggwaffle,cowplot)

#install.packages("tm")
#install.packages("wordcloud2")
#install.packages("wordcloud")
library(wordcloud)
library(wordcloud2)
library(tm)

# Working directory
setwd("C:/IDDO_Other/Noma/Data")

#============================================================
# Read the datasets for generating wordclouds for treatment
#============================================================
trt_cloud0 <- read_excel("analysis_data_20_03_2023_wordmap.xlsx", 
                   sheet = "Treatment") %>% 
              clean_names()

# Keep only the columns with treatment names and rename columns
trt_cloud <- trt_cloud0 %>% 
  select(please_separate_all_individual_treatment_terms_from_column_f_to_individual_cells_horizontally_here:ncol(trt_cloud0)) 

trt_cloud <- trt_cloud %>% 
  rename_with(~ str_c("x", seq_along(.x)), 1:ncol(trt_cloud))

# Covert all the columns to 1 single column and remove missing fields
trt_cloud <- trt_cloud %>% 
  pivot_longer(
    cols = c(x1:x17),
    names_to = "col_num",
    names_prefix = "column",
    values_to = "treatment_used",
    values_drop_na = TRUE
  ) 

# count
(trt_cloud1 <- trt_cloud %>%
    dplyr::group_by(treatment_used) %>%
    dplyr::summarise(
      n_freq = length(treatment_used)
    ))
trt_cloud1$treatment_used1 <- str_to_sentence(trt_cloud1$treatment_used)

#----------------------------------------------------------------------------------
# Re-code some of the different variation for referring to the same treatment used
#----------------------------------------------------------------------------------
trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                      c('Abbe flap|Abbe flaps'), 'Abbe flap')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Abbe flaps'), 'Abbe flap')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Anterolateral thigh flap|Anterolateral thigh flapss'), 'Anterolateral thigh flaps')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Antifungal therapy|Antifungals'), 'Antifungals')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Antiparasitic solution|Antiparasitic therapy'), 'Antiparasitics')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Antiseptic care|Antiseptic dressing|Antiseptic mouthwash|Antiseptic solutions|Antiseptic therapy|Antiseptic treatment|Antiseptics'), 'Antiseptics')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Beef tea|Beef-tea'), 'Beef tea')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Benzydamime hydrocloride|Benzydamine hydrochloride'), 'Benzydamine hydrochloride')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Bone graft|Bone grafting|Bone grafts'), 'Bone graft')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Cheek roatation|Cheek rotation'), 'Cheek rotation')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Commissuroplasty|Commisuroplasty|Commisurroplasty'), 'Commissuroplasty')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Debridement|Debirdement|Debridemeent'), 'Debridement')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Deltopectoral flaps'), 'Deltopectoral flap')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Estalander flap|Estlander flap'), 'Estlander flap')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Fomentation|Fomentations'), 'Fomentation')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Forehead flap|Forehead flaps'), 'Forehead flap')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Free flap|Free flaps'), 'Free flap')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Free radial forearm flap|Free radial forearm flaps'), 'Free radial forearm flap')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Gentamicin|Gentamycin'), 'Gentamicin')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Iron sulfate|Iron sulphate'), 'Iron sulphate')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Lip reconstruction|Lips reconstruction'), 'Lip reconstruction')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Magnesium sulfate|Magnesium sulphate|Magnesium-sulphate'), 'Magnesium sulphate')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Multivitamin supplements|Multivitamins'), 'Multivitamins')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Myocutaneous flap|Myocutaneous flaps'), 'Myocutaneous flap')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Neosalvarsan|Neosalvarsen'), 'Neosalvarsan')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Nutritional supplements|Nutritional support|Nutritional supports'), 'Nutritional support')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Opiate|Opiates'), 'Opiates')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Piperacillin|Pipercillin'), 'Piperacillin')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Poultice|Poultices'), 'Poultices')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Skin graft|Skin grafts'), 'Skin graft')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Submental flap|Submental flaps'), 'Submental flap')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Sulphate of potash|Sulphate of potass'), 'Sulphate of potash')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Surgical reconstruction|Surgical recontruction'), 'Surgical reconstruction')

trt_cloud1$treatment_used1 = str_replace(trt_cloud1$treatment_used1, 
                                         c('Vitamin supplements|Vitamins|Vitamins supplements'), 'Vitamins supplements')


#----------------------------
# Summarise the frequency
#----------------------------
(trt_cloud2 <- trt_cloud1 %>%
    dplyr::group_by(treatment_used1) %>%
    dplyr::summarise(
      n_freq = sum(n_freq)
    ))

#==============================================================
# Read the datasets for generating wordclouds for risk factors
#==============================================================
rf_cloud0 <- read_excel("analysis_data_20_03_2023_wordmap.xlsx", 
                         sheet = "Risk Factors") %>% 
  clean_names()

# Keep only the columns with treatment names and rename columns
rf_cloud <- rf_cloud0 %>% 
  select( please_separate_all_individual_risk_factor_terms_from_column_f_to_individual_cells_horizontally_here:ncol(rf_cloud0)) 

rf_cloud <- rf_cloud %>% 
  rename_with(~ str_c("x", seq_along(.x)), 1:ncol(rf_cloud))

# Covert all the columns to 1 single column and remove missing fields
rf_cloud <- rf_cloud %>% 
  pivot_longer(
    cols = c(x1:x15),
    names_to = "col_num",
    names_prefix = "column",
    values_to = "risk_factors_reported",
    values_drop_na = TRUE
  ) 

# count the freq
(rf_cloud1 <- rf_cloud %>%
    dplyr::group_by(risk_factors_reported) %>%
    dplyr::summarise(
      n_freq = length(risk_factors_reported)
    ))

#?str_to_sentence
rf_cloud1$risk_factors_reported1 <- str_to_sentence(rf_cloud1$risk_factors_reported)

# Manually correct the spelling variations
rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                         c('Anaemia|Anaemic|Anemic|Aplastic anaemia|Marked anaemia|Mycrocytic anaemia'), 'Anaemia')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Bacillary dysentery|Bacillary dysentry'), 'Bacillary dysentery')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Bad teeth|Bad tooth'), 'Bad teeth')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Bronchopneumonia|Bronchopnuemonia|Broncho-pnuemonitis|Broncopneumonia|Lobar pneumonia|Pneumonia|Mycoplasma pneumonia'), 'Pneumonia')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Burkitt tumor|Burkitts lymphoma'), 'Haematological malignancy')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Chronic bronchitis|Tubular bronchitis'), 'Bronchitis')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Chronic obstructive airways disease|Chronic obstructive pulmonary disease'), 'Chronic obstructive pulmonary disease')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Clinically wasted|Global acute malnutrition|Low bmi|Malnourished|Malnutrion|Malnutrition|Marasmus|Protein energy malnutrition|Sever malnutrition|Severe acute malnutrition|Severe malnutrition|Severe stunting|Severe wasting|Severely malnourished|Small for age|Stunted growth|Stunting|Underweight|Wasted|Wasting|Features of rickets|Moderate acute malnutrition|Poor nutritional status'),   
                                                'Malnutrition')
# Complete failure of self care
rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Complete failure of self care|Failing self-care|No self care'), 'Failing self-care')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Dehydrated|Dehydration'), 'Dehydration')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Diabetes mellitus|Diabetic|Diabtes mellitus|Type ii diabetes'), 'Diabetes mellitus')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Diarrhoea|Diarrhea'), 'Diarrhoea')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Epstein barr virus|Epstein-barr virus type 1 and type 2'), 'Epstein barr virus')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Febrile|Febrile syndrome|Fever|Fever syndrome|Fever with chill and rigor|Fever with chill and rigor|Pyrecial||Pyrexia|Pyrexial|Intermittent fever|Feverl'), 'Fever')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Foetor oris|Foetor otis'), 'Foetor oris')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('General weakness|Weakness|Weak'), 'Weakness')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Hiv'), 'HIV')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Hypoproteinaemia|Hypoproteinemia'), 'Hypoproteinemia')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Icterus|Jaundice'), 'Liver failure')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Liver dysfunction|Liver failure'), 'Liver failure')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Immunosuppression|Immunosupression'), 'Immunosuppression')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Intestinal parasitism|Intestinal parasitosis|Hookworm infection'), 'Intestinal parasites')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Kala-azar|Visceral leishmaniasis'), 'Kala-azar')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Kwashiorker|Kwashiorkor'), 'Malnutrition')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Leukemia|Luekemia'), 'Leukemia')

ported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Low socio-econoic background|Low socio-economic background|Low socio-economic levels|Low standard of living|Low socio-econoic background|Low socio-economic levels|Low standard of living'), 'Low socio-economic background')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Obstructive deep apnea'), 'Obstructive sleep apnea')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Pertussis|Whooping cough'), 'Pertussis')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Poor oral health|Poor oral hygeine|Poor oral hygiene'), 'Poor oral health')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Proximity to livestock|Proximity with domestic animals'), 'Proximity with domestic animals')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Pulmonary tubercolosis|Pulmonary tuberculosis'), 'Pulmonary tuberculosis')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Right-sided otitis media|Otitis media'), 'Otitis media')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Rubella|Rubeola'), 'Rubella')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Scalatina|Scarlet fever'), 'Scarlet fever')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Sepsis|Septic shock|Septicemia'), 'Sepsis')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Seventy-two blood transfusion'), 'Blood transfusion')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Smoker|Smoking'), 'Smoking')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Syphilis|Syphillis|Syphylis'), 'Syphilis')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Tooth extaction|Tooth extraction'), 'Tooth extraction')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Conjestive heart failure|Coronary artery disease|Cardiac arrest and became comatose during partial gastrectomy|Coronary artery disease|Cardiac condition|Heart disease'), 'Cardiac condition')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Diarrhoea|Chronic diarrhoea'), 'Diarrhoea')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Extreme cachexia|Cachexia'), 'Cachexia')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('High stress|Severe stress|Stress'), 'Severe stress')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Feverl'), 'Fever')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Low socio-econoic background|Low socio-economic levels'), 'Low socio-economic background')

rf_cloud1$risk_factors_reported1 = str_replace(rf_cloud1$risk_factors_reported1, 
                                               c('Not well educated|Poor literacy'), 'Poor literacy')

#Burkitt tumor, Burkitt's lymphoma, Lymphoma,  Luekemia,Myelodysplastic syndrome,
#Myeloid metaplasia,Myeloid leukemia,T cell acute lymphoblastic leukaemia

#Intestinal parasitism, hookworm, -->Intestinal parasites

(rf_cloud2 <- rf_cloud1 %>%
    dplyr::group_by(risk_factors_reported1) %>%
    dplyr::summarise(
      n_freq = sum(n_freq)
    ))

#==============================================================
# Read the datasets for generating wordclouds for Microbiology
#==============================================================
mb_cloud0 <- read_excel("analysis_data_20_03_2023_wordmap.xlsx", 
                        sheet = "Microbiology") %>% 
  clean_names()

# Keep only the columns with treatment names and rename columns
mb_cloud <- mb_cloud0 %>% 
  select( please_separate_all_individual_microbes_from_column_f_to_individual_cells_horizontally_here: ncol(mb_cloud0))

mb_cloud <- mb_cloud %>% 
  rename_with(~ str_c("x", seq_along(.x)), 1:ncol(mb_cloud))

# Covert all the columns to 1 single column and remove missing fields
mb_cloud <- mb_cloud %>% 
  pivot_longer(
    cols = c(x1:x21),
    names_to = "col_num",
    names_prefix = "column",
    values_to = "microbiology",
    values_drop_na = TRUE
  ) 

mb_cloud$microbiology <- str_to_sentence(mb_cloud$microbiology)

# count the freq
(mb_cloud1 <- mb_cloud %>%
    dplyr::group_by(microbiology) %>%
    dplyr::summarise(
      n_freq = length(microbiology)
    ))

#mb_cloud1$microbiology1 <- str_to_sentence(mb_cloud1$microbiology)
as.data.frame(mb_cloud1$microbiology)

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology, 
                                      c('Escherichia coli |Eschericha coli|Escherichia coli|Escherichia coli|Escherichia coli|Escherichia coli|Escherichia coli|Escherichia coli '), 'Escherichia coli')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Fusiform bacilli|Fusiform bacilli'), 'Fusiform bacilli')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Haemoophilus influenzae|Haemophilus influenzae|Haemoophilus influenzae|Haemophilus influenxae'), 'Haemoophilus influenzae')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Klebsiella pneumoniae.|Klebsiella pneumonia|Klebsiella pneumoniae|Klebsiella pneumoniaee|Klebsiella pnuemoniae|Klebsiella pneumoniaee|Klebsiella pneumoniae'), 'Klebsiella pneumoniae')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Porphyro- monas|Porphyromonas'), 'Porphyromonas')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Proteus mirabilis|Proteus mirabilus'), 'Proteus mirabilis')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Pseudomonas aeruginosa|Pseudomonas aeruginosa|Pseudonomas aeruginosa|Psuedomonas aeruginosa|Pseudomonas aeroginosa|Pseudomonas aeruginosa|Pseudomnunas aenuginosa'), 'Pseudomonas aeruginosa')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Serratia marcencens|Serratia marcescens'), 'Serratia marcescens')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Staphlococcus aureus|Staphyloccocus aureus|Staphylococcus\r\nAureus|Staphylococcus aureus'), 'Staphylococcus aureus')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Staphylococcus|Staphylococcus'), 'Staphylococcus')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Alpha haemolytic streptococci|Alpha hemolytic stroptoaocci|Alpha- hemolytic streptococci|Alpha- hemolytic streptococci|Alpha-hemolytic streptococci|Alpha-hemolytic streptococci'), 'Alpha-hemolytic streptococci')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c(' Diphtheria bacilli|Diphtheria bacillus|Diphtheria bacillus'), 'Diphtheria bacillus')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Coagulase negative staphylococcus|Coagulase-negative staphylococcus'), 'Coagulase negative staphylococcus')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Enteroccocus|Enterococcus|Enterococci'), 'Enterococcus')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Fusiform bacilli|Fusiform bacillus|Fusiforrn bacilli'), 'Fusiform bacilli')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Gram-negative bacill|Gram-negative bacilli|Gram negative bacilli|Gram-negative bacilli|Gram-negative bacillii'), 'Gram-negative bacilli')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Gram positive cocci|Gram-positive cocci'), 'Gram-positive cocci')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Neisseria|Neissseria'), 'Neissseria')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Spirochaetes|Spirochete|Spirochetes'), 'Spirochetes')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Gram positive cocci|Gram-positive cocci'), 'Gram-positive cocci')

mb_cloud1$microbiology1 = str_replace(mb_cloud1$microbiology1, 
                                      c('Gram positive cocci|Gram-positive cocci'), 'Gram-positive cocci')

#-------------------------------------
# microbiology word-cloud frequency
#-------------------------------------
(mb_cloud2 <- mb_cloud1 %>%
    dplyr::group_by(microbiology1) %>%
    dplyr::summarise(
      n_freq = sum(n_freq)
    ))

#==============================
# RISK FACTORS WORD CLOUD
#==============================
## plot this
tiff(file="C:/IDDO_Other/Noma/Results/risk_factors_cloud_v3.tiff", 
     width=16, height=10, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none")

set.seed(939838384) # for reproducibility 
wordcloud(words = rf_cloud2$risk_factors_reported1, 
          freq = rf_cloud2$n_freq, 
          min.freq = 1,
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2")          
        )
dev.off()

#==============================
# TREATMENT WORD CLOUD
#==============================
## plot this
tiff(file="C:/IDDO_Other/Noma/Results/treatment_cloud_v2.tiff", 
     width=26, height=12, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

set.seed(939838383) # for reproducibility 
wordcloud(words = trt_cloud1$treatment_used, 
          freq = trt_cloud1$n_freq, 
          min.freq = 1,
          #max.words=200, 
          random.order=FALSE, 
          rot.per=0.45,            
          colors=brewer.pal(8, "Dark2")          
          )
dev.off()

#==============================
# Microbiology WORD CLOUD
#==============================
tiff(file="C:/IDDO_Other/Noma/Results/pathogen_cloud_v2.tiff", 
     width=28, height=16, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

wordcloud(words = mb_cloud2$microbiology1, 
          freq = mb_cloud2$n_freq, 
          min.freq = 1,
          #max.words=200, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2")          
)

dev.off()

## END
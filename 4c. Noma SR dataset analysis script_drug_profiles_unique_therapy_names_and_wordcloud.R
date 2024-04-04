################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Summarise treatment data and generate word-cloud
################################################################################
#rm(list=ls())
library(pacman)
pacman::p_load(readxl,tidyverse,ggpubr,purrr,readr,stringr,patchwork,table1,doBy,gtools,janitor,
               RColorBrewer,wesanderson,viridis,forcats,tibble,tidyr,dplyr,maps,rworldmap,rworldxtra,
               naniar,cowplot,maptools,gridExtra,classInt,car,ggpubr,ggalt,plyr,IDPmisc,rms,epitools,
               textclean,waffle,ggwaffle,ggbreak, tidyr)

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
                                   "Post-2010")

#########################################
# Summarise the intervention approaches
#########################################

#-----------------------------
# Total number of studies
#-----------------------------
(total_studies <- dat_lab %>%
   dplyr::summarise(n_studies = length(unique(row_id))
   ))

#---------------------------------------
# Studies that reported a treatment
#---------------------------------------
dat_lab <- dat_lab %>% 
  select(treatment_details) %>% 
  filter(treatment_details!="-99")

#drug <- dat_lab %>% separate_wider_delim(treatment_details, ",")
drug <- as.data.frame(unlist(strsplit(dat_lab$treatment_details, ",")))

# https://stackoverflow.com/questions/28100780/use-pipe-operator-with-replacement-functions-like-colnames
drug <- drug %>% 
  purrr::set_names(c("trt_names"))

# remove hidden characters
drug <-  drug %>% 
  mutate(trt_names = replace_non_ascii(str_trim(trt_names)))

#------------------------
# Do further unlisting
#------------------------
drug1 <- as.data.frame(unlist(strsplit(drug$trt_names, ";")))

drug1 <- drug1 %>% 
  purrr::set_names(c("trt_names"))

# remove hidden characters
drug1 <-  drug1 %>% 
  mutate(trt_names = replace_non_ascii(str_trim(trt_names)))

#----------------------
# count uniques
#------------------------
(unique_trt <- drug1 %>% 
    dplyr::group_by(trt_names) %>%
        dplyr::summarise(
            n_freq = n()
            )
 )

unique_trt <- unique_trt %>% 
  filter(!(
          trt_names %in% c("and","1","000 IU","100 mg.","2016","22","3","32.4%)","after 10 days",
                           "33and 34","364 (19.0%) vis- ited a hospital","600","oral")
          )
         )

#unique_trt1 <- unique_trt %>% 
#  mutate(trt_names = trimws(str_remove(trt_names, "(\\s+[A-Za-z]+)?[0-9-]+")))

write.csv(unique_trt, "C:/IDDO_Other/Noma/Results/unique_trt_list_17_04_2023.csv", 
          row.names=FALSE
          )

#==============================
# WORD CLOUD
#==============================
library(wordcloud)
library(wordcloud2)
library(tm)

#Create a vector containing only the text
text <- drug1$trt_names

# Create a corpus  
docs <- Corpus(VectorSource(text))

# clean
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

## plot this
tiff(file="C:/IDDO_Other/Noma/Results/out4_drug/S4_drug_wordcloud.tiff", 
     width=56, height=20, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

set.seed(939838383) # for reproducibility 
wordcloud(words = df$word, 
          freq = df$freq, 
          min.freq = 1,
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2")          
          )

dev.off()

## END
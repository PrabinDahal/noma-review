################################################################################
# Title			          :	Summarising Noma systematic literature review database
# Data version		    :	March-2023
# Analysis version		:	March-2023
# Task                : Summarise the analysis data
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

# Read the original dataset
dat_lab<-read.csv("analysis_data_20_03_2023.csv")

#######################################################
# Summarise study data
#######################################################

#-------------------------------
# Total number of studies
#-------------------------------
(total_studies <- dat_lab %>%
   dplyr::summarise(
              n_studies = length(unique(row_id))
        )
    )

#-----------------------------------------------
# By time period
#-----------------------------------------------
(n_articles_yearcut <- dat_lab %>%
   dplyr::group_by(year_cut) %>%
   dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

#-----------------------------------------------
# By time period and country
#-----------------------------------------------
(n_articles_year_country <- dat_lab %>%
   dplyr::group_by(country, year_cut) %>%
   dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

(n_articles_country_2000_plus <- dat_lab %>%
    filter (year_cut=="2000+") %>%
    dplyr::group_by(region) %>%
        dplyr::summarise(
      number_of_articles = length(row_id),
      number_of_patients = sum(n_patients, na.rm=T)
    )) 

#-----------------------------------------------
# single vs multi-centre studies
#-----------------------------------------------
(n_articles_multicentre <- dat_lab %>%
   dplyr::group_by(multicentre) %>%
   dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

#-----------------------------------------------
# Summary of study design
#-----------------------------------------------
(n_articles_design <- dat_lab %>%
   dplyr::group_by(design) %>%
   dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T),
     min_year = min(year),
     max_year = max(year),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

#-----------------------------------------------
(cohort <- dat_lab %>%
   filter(design %in% c("Cohort","Cross-sectional")) %>% 
   #filter(n_patients >50) %>% 
   dplyr::group_by(design) %>%
         dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T),
     min_year = min(year),
     max_year = max(year),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))


#---------------------------------
# studies with missing sample size
#---------------------------------
missing_n <- dat_lab %>% 
            filter(is.na(n_patients))

#------------------------------------------
# Not stated and multi-country studies
#------------------------------------------
missing_country <- dat_lab %>% 
  filter(country %in% c("Multi-country","Not stated"))

#-----------------------------------------------
# List of interventional studies
#-----------------------------------------------
intv_studies <- dat_lab %>%
   filter(design=="Interventional")
summary(intv_studies$n_patients)

#------------------------------------------------------------------------
# Total number of unique articles & sample size by country & study design
#------------------------------------------------------------------------
(n_articles_country_design <- dat_lab %>%
   dplyr::group_by( design) %>%
   dplyr::summarise(
     number_of_articles   = length(row_id),
     number_of_patients   = sum(n_patients, na.rm=T),
     median_patients      = median(n_patients, na.rm=T),
     min_patients         = min(n_patients, na.rm=T),
     max_patients         = max(n_patients, na.rm=T),
     min_year             = min(year),
     max_year             = max(year) 
   )) 

n_articles_country_design <- as.data.frame(n_articles_country_design)
write.csv(n_articles_country_design, "C:/IDDO_Other/Noma/Results/out2_samplesize/n_articles_country_design.csv", row.names=FALSE)

#--------------------------------------------
# Total number of unique articles by country
#--------------------------------------------
(n_articles_country <- dat_lab %>%
   dplyr::group_by(country) %>%
   dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

n_articles_country <- as.data.frame(n_articles_country)
write.csv(n_articles_country, "C:/IDDO_Other/Noma/Results/out2_samplesize/n_articles_country.csv", row.names=FALSE)

#-------------------------------------------------------------
# Total number of unique articles by country and time-period
#-------------------------------------------------------------
(n_articles_country_time <- dat_lab %>%
    dplyr::group_by(year_cut, country) %>%
    dplyr::summarise(
      number_of_articles = length(row_id),
      number_of_patients = sum(n_patients, na.rm=T)
    ))

n_articles_country_time <- as.data.frame(n_articles_country_time)
write.csv(n_articles_country_time, "C:/IDDO_Other/Noma/Results/out2_samplesize/n_articles_country_time.csv", row.names=FALSE)

#-------------------------------------------------------
# Number of articles by design and time-period
#-------------------------------------------------------
(n_articles_space_time <- dat_lab %>%
   dplyr::group_by(year_cut,design,continent) %>%
   dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T)
   )) 

#-----------------------------------------------
# Total number of unique articles by WHO region
#-----------------------------------------------
(n_articles_WHO_region <- dat_lab %>%
   dplyr::group_by(WHO_region) %>%
   dplyr::summarise(
     number_of_articles = length(row_id),
     year_min = min(year),
       year_max= max(year),
     number_of_patients = sum(n_patients, na.rm=T),
     percentage = round((number_of_articles/total_studies)*100,2)
   )) %>% 
  arrange(desc(percentage))

n_articles_country <- as.data.frame(n_articles_country)
write.csv(n_articles_country, "C:/IDDO_Other/Noma/Results/out2_samplesize/n_articles_country.csv", row.names=FALSE)

#######################################################################
## Graphical exports
#######################################################################
setwd("C:/IDDO_Other/Noma/Results/out2_samplesize")

#--------------------------------------------
# Total number of unique articles by country 
#--------------------------------------------

tiff(file="S2_lollipop_plot_n_articles_country.tiff", 
     width=36, height=24, units="cm", 
     pointsize="8", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

ggdotchart(n_articles_country, 
           x = "country",
           y = "number_of_articles",
           color = "#00AFBB",                                		# Color by groups
           sorting = "descending",                       
           add = "segments",                             			# Add segments from y = 0 to dots
           rotate = TRUE,                                
           dot.size = 5.5,                                 
           label = n_articles_country$number_of_articles,                        
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               
           ggtheme = theme_pubr()                        
)+
  ylab("Number of reports") +
  ylim(0,50)+
  xlab("")+
  ggtitle("")+
  theme(axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

dev.off()

#--------------------------------------------------------------
# Total number of unique articles by country and study design
#--------------------------------------------------------------

#-----------------------------------------
# Total number of unique articles by year
#-----------------------------------------
n_articles_year <- dat_lab %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(number_of_articles = length(unique(study)))

tiff(file="S2_histogram_n_pubs_by_year.tiff", 
     width=22, height=22, units="cm", 
     pointsize="8", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

a <- hist(n_articles_year$year,
     ylim=c(0,15),
     xlim=c(1820,2019),
     breaks=15,
     las=1,
     #col="#FC4E07",
     xlab="Publication year",
     ylab="Number of articles",
     lwd=2,
     main="",
     cex.lab=1.6,
     axes=FALSE
)
axis(2,seq(0,15,5), las=1, cex.axis=1.2)
axis(1,seq(1800,2050,50), cex.axis=1.2)

dev.off()

#-----------------------------------------------
# Total number of unique articles by year-group
#-----------------------------------------------
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

n_articles_year_cat <- dat_lab %>% 
                        dplyr::group_by(time_period ) %>%
                        dplyr::summarise(number_of_articles = length(unique(study)))

n_articles_per_year <- dat_lab %>% 
                        dplyr::group_by(year) %>%
                        dplyr::summarise(number_of_articles = length(unique(study)))

# plot as panel and use plot_grid
a <- ggplot(data=n_articles_per_year, aes(x=year, y=number_of_articles)) +
  geom_bar(stat="identity" , fill="#21918c")+
  xlab("Publication year")+
  ylab("Number of studies")+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=30))

b <- ggplot(data=n_articles_year_cat, aes(x=time_period, y=number_of_articles)) +
  geom_bar(stat="identity" , fill="#21918c")+
  #scale_x_discrete( labels=c("<1850", "1851-1900", "1901-1950", "1951-2000","2001-2010","2011+"))+
  xlab("")+
  ylab("")+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=30))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Export the sample size plots
tiff(file="S2_histogram_n_pubs_by_year_panels.tiff", 
     width=50, height=20, units="cm", 
     pointsize="8", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

plot_grid(a, b)
dev.off()

#===================================================
# Total number of unique patients by country
#===================================================
dat_lab$n_patients[dat_lab$n_patients < 0]<- NA

n_patients_country <- dat_lab %>% 
  dplyr::group_by(country) %>%
  dplyr::summarise(
    number_of_patients = sum(n_patients, na.rm=T)
  )
n_patients_country <- n_patients_country[rev(order(n_patients_country$number_of_patients)),]

tiff(file="S2_barchart_sample_size_by_country.tiff", 
     width=36, height=26, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

ggbarplot(n_patients_country, 
          x = "country",
          y = "number_of_patients",
          facet.by = NULL,
          fill = "#00AFBB",
          sorting = "descending",                       # Sort value in descending order
          rotate = T,                                   # Rotate vertically
          dot.size = 6,                                 # Large dot size
          order	= rev(n_patients_country$country),
          ggtheme = theme_pubr()                        # ggplot2 theme
)+
  ylab("Number of patients") +
  xlab("")+
  ggtitle("")+
  theme(axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=10),
        axis.title=element_text(size=14,face="bold")) +
  guides(fill=FALSE, color=FALSE) +
  guides(fill=guide_legend(title=""), size=10
  )+
  scale_y_break(c(1500, 10000),   space = 0.9)

dev.off()

#==================================================================
# Total number of unique patients by country & Study Design
#==================================================================
dat_lab$n_patients[dat_lab$n_patients < 0]<- NA

n_patients_country_design <- dat_lab %>% 
  dplyr::group_by(country,design) %>%
  dplyr::summarise(
    number_of_patients = sum(n_patients, na.rm=T)
  )

## Keep only top 10 countries
n_patients_country_design <- n_patients_country_design %>% 
  dplyr::group_by(country) %>%
  dplyr::mutate(
    total = sum(number_of_patients)
  ) %>% 
  filter(total > 10)

# specify the country order
n_patients_country_design$country <- fct_relevel(n_patients_country_design$country, 
                                   "Nigeria", 
                                   "Niger", 
                                   "Chile",
                                   "Burkina Faso",
                                   "Ethiopia",
                                   "Switzerland",
                                   "Senegal",
                                   "China",
                                   "Multi-country",
                                   "Cote d'Ivoire",
                                   "Zambia",
                                   "UK",
                                   "Lao People's Democratic Republic (Laos)",
                                   "India",
                                   "Zimbabwe",
                                   "Vietnam",
                                   "USA",
                                   "Turkey",
                                   "Pakistan",
                                   "South Africa",
                                   "Mali",
                                   "Iran",
                                   "Afghanistan",
                                   "Madagascar",
                                   "Tunisia",
                                   "Kenya",
                                   "Algeria",
                                   "Not stated"
                                  )
# We want the barplot in descending order
n_patients_country_design$country <- fct_rev(n_patients_country_design$country)

# Bar plot with breaks on x-axis
tiff(file="S2_barchart_sample_size_by_country_&_design.tiff", 
     width=40, height=26, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

ggbarplot(n_patients_country_design, 
          x = "country",
          y = "number_of_patients",
          #yscale = "log10",
          #col= "#00AFBB",
          fill ="design",
          #facet.by = "design",
          sorting = "ascending",                      
          rotate = T,
          dot.size = 8,                                 
          #order	= rev(n_patients_country_design$number_of_patients),
          ggtheme = theme_pubr()                        
)+
  ylab("Number of patients") +
  xlab("")+
  ggtitle("")+
  theme(axis.text.x = element_text(size=18, angle=0),
        axis.text.y = element_text(size=18),
        axis.title=element_text(size=18)) +
  guides(fill=FALSE, color=FALSE, size=16) +
  guides(fill=guide_legend(title=""), size=20
  )+
  scale_y_break(c(1500, 10000),   space = 0.9)+
  theme(legend.text=element_text(size=rel(1.5)))

dev.off()

#=======================================================
# Further exlporation of sample size by age-group etc
#=======================================================
tiff(file="S2_boxplot_sample_size_by_region.tiff", 
     width=28, height=18, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

par(mar = c(2, 7, 2, 2)) # Set the margin on all sides to 2

boxplot(dat_lab$n_patients ~ dat_lab$continent,
        outline=F,
        xlab="",
        log="y",
        las=1,
        ylim=c(1,10000),
        col=2:8,
        cex.axis=1.5,
        cex.lab=2,
        ylab="")

title(ylab="Sample size per study", 
      mgp=c(5,1,0),
      cex.lab=2
      )

stripchart(dat_lab$n_patients ~ dat_lab$continent, 
           add = TRUE, 
           vertical = TRUE,
           method = "jitter", 
           col="slategrey",
          # col=2:8, 
           pch = 1, 
           cex=2
           )
dev.off()

# sample size by the WHO region
tiff(file="S2_boxplot_sample_size_by_WHO_region.tiff", 
     width=28, height=18, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

par(mar = c(2, 7, 2, 2)) # Set the margin on all sides to 2

boxplot(dat_lab$n_patients ~ dat_lab$WHO_region,
        outline=F,
        xlab="",
        log="y",
        las=1,
        ylim=c(1,10000),
        col=2:8,
        cex.axis=1.5,
        cex.lab=2,
        ylab="")

title(ylab="Sample size per study", 
      mgp=c(5,1,0),
      cex.lab=2
)

stripchart(dat_lab$n_patients ~ dat_lab$WHO_region, 
           add = TRUE, 
           vertical = TRUE,
           method = "jitter", 
           col="slategrey",
           # col=2:8, 
           pch = 1, 
           cex=2
)

dev.off()

# By study design
setwd("C:/IDDO_Other/Noma/Results/out2_samplesize")

tiff(file="S2_boxplot_sample_size_by_design_v1.tiff", 
     width=28, height=18, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

par(mar = c(4, 7, 2, 2)) # Set the margin on all sides to 2

boxplot(dat_lab$n_patients ~ dat_lab$design,
        outline=F,
        xlab="",
        log="y",
        las=1,
        ylim=c(1,10000),
        col=2:8,
        cex.axis=3,
        cex.lab=10,
        ylab="")

title(ylab="Sample size per study", 
      mgp=c(5,1,0),
      cex.lab=4
)

stripchart(dat_lab$n_patients ~ dat_lab$design, 
           add = TRUE, 
           vertical = TRUE,
           method = "jitter", 
           col="slategrey",
           # col=2:8, 
           pch = 1, 
           cex=2
  )

dev.off()

###########################################################################
# Plot Sample size by region and design in single graph for publication
###########################################################################
setwd("C:/IDDO_Other/Noma/Results/out2_samplesize")

tiff(file="S2_boxplot_sample_size_by_design_&_region_v1.tiff", 
     width=36, height=14, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

a2 <- ggplot(dat_lab, aes(design, n_patients)) + 
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.2))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  scale_y_log10()+
  ylab("") + 
  xlab("")  +
  theme(axis.title = element_text(size = 15))+
  theme(axis.text = element_text(size = 15))

a1 <- ggplot(dat_lab, aes(continent, n_patients)) + 
  geom_boxplot() +
  geom_jitter(position=position_jitter(0.2))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  scale_y_log10()+
  ylab("Sample size per study (log-scale)") + 
  xlab("")  +
  theme(axis.title = element_text(size = 15))+
  theme(axis.text = element_text(size = 15))

plot_grid(a1, a2)

dev.off()

#===============================================================================
# Stacked bar chart: Number of studies by region,study design & time-period
#===============================================================================
(n_articles_space_time <- dat_lab %>%
   dplyr::group_by(year_cut,design,continent) %>%
   dplyr::summarise(
     number_of_articles = length(row_id),
     number_of_patients = sum(n_patients, na.rm=T)
   )) 

# Pre-1900
dat0 <- n_articles_space_time[which(n_articles_space_time$year_cut=="Pre-1900"),]

a<- ggbarplot(dat0, 
              x = "continent",
              y = "number_of_articles",
              fill = "design",
              palette = brewer.pal(n = 5, name = "Set2"),                             # Color by groups
              sorting = "descending",                       # Sort value in descending order
              rotate = FALSE,                                # Rotate vertically
              ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Number of studies") +
  xlab("")+
  ylim(0,40)+
  ggtitle("Pre-1900")+
  theme(axis.text.x = element_text(size=8, angle=0),
        axis.text.y = element_text(size=10),
        axis.title=element_text(size=14)) +
  guides(fill=FALSE, color=FALSE) +
  guides(fill=guide_legend(title=""), size=12)+
  theme(legend.text=element_text(size=rel(0.6)))+
  theme(legend.key.size = unit(0.5, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) 

# 1900-1950
dat1 <- n_articles_space_time[which(n_articles_space_time$year_cut=="1900-1950"),]

b <-ggbarplot(dat1, 
              x = "continent",
              y = "number_of_articles",
              fill = "design",
              palette = brewer.pal(n = 5, name = "Set2"),                             # Color by groups
              sorting = "descending",                       # Sort value in descending order
              rotate = FALSE,                                # Rotate vertically
              ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("") +
  xlab("")+
  ylim(0,40)+
  ggtitle("1950-2000")+
  theme(axis.text.x = element_text(size=8, angle=0),
        axis.text.y = element_text(size=10),
        axis.title=element_text(size=14)) +
  guides(fill=FALSE, color=FALSE) +
  guides(fill=guide_legend(title=""), size=12)+
  theme(legend.text=element_text(size=rel(0.6)))+
  theme(legend.key.size = unit(0.5, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) 

# 1950-2000
dat2 <- n_articles_space_time[which(n_articles_space_time$year_cut=="1950-2000"),]

c <- ggbarplot(dat2, 
               x = "continent",
               y = "number_of_articles",
               fill = "design",
               palette = brewer.pal(n = 5, name = "Set2"),                             # Color by groups
               sorting = "descending",                       # Sort value in descending order
               rotate = FALSE,                                # Rotate vertically
               ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("Number of studies") +
  xlab("")+
  ylim(0,150)+
  ggtitle("1950-2000")+
  theme(axis.text.x = element_text(size=8, angle=0),
        axis.text.y = element_text(size=10),
        axis.title=element_text(size=14)) +
  guides(fill=FALSE, color=FALSE) +
  guides(fill=guide_legend(title=""), size=12)+
  theme(legend.text=element_text(size=rel(0.6)))+
  theme(legend.key.size = unit(0.5, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) 

## 2000 onwards
dat3 <- n_articles_space_time[which(n_articles_space_time$year_cut=="2000+"),]

d <-ggbarplot(dat3, 
              x = "continent",
              y = "number_of_articles",
              fill = "design",
              palette = brewer.pal(n = 5, name = "Set2"),                             # Color by groups
              sorting = "descending",                       # Sort value in descending order
              rotate = FALSE,                                # Rotate vertically
              ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylab("") +
  xlab("")+
  ylim(0,150)+
  ggtitle("2000 onwards")+
  theme(axis.text.x = element_text(size=8, angle=0),
        axis.text.y = element_text(size=10),
        axis.title=element_text(size=14)) +
  guides(fill=FALSE, color=FALSE) +
  guides(fill=guide_legend(title=""), size=12)+
  theme(legend.text=element_text(size=rel(0.6)))+
  theme(legend.key.size = unit(0.5, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) 


tiff(file="S2_stacked_barchart_n_pubs_by_region_time_design.tiff", 
     width=28, height=18, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )


plot_grid(a, b,c,d)

dev.off()

#==============================================
# Export age distribution as a waffle plot
#==============================================
tiff(file="S2_waffle_plot_age_distribution.tiff", 
     width=28, height=18, units="cm", 
     pointsize="7", compression = "lzw+p", 
     bg="white", res=400, antialias = "none" )

waffle(c(table(dat_lab$age)), 
       rows = 8, 
       glyph_size = 30,
       title="",
       legend_pos="right",
       pad=1,
       xlab="1 square = 1 study")+
  guides(fill=guide_legend(title=""), size=30)+
  theme(legend.text=element_text(size=rel(9.5)))+
  theme(legend.key.size = unit(0.9, "cm"))+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(text = element_text(size=rel(1.8)),
        strip.text.x = element_text(size=rel(1.8)),
        strip.text.y = element_text(size=rel(1.8)))+
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))+
  theme(legend.position = "right")+
  theme(plot.title = element_text(size = 25, colour = "darkred"))

dev.off()

## END
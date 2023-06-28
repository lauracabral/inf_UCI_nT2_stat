##Script and calls used for stats
#Uses basic r and packages
#Laura Cabral 2023

#Load an assorted number of packages
library(stringr)
library(tidyverse)
library(ggpubr)
library(tidyr)
library("data.table")
library(psych)
library(olsrr)
library(visreg)

#Load data in long format 
long_four_tat2_inf <- read.csv("")

#Already set, but for clarity
names(long_four_tat2_inf) <- c("idchild", "tat2","roi" )

#Box plot needs it to be a factor
long_four_tat2_inf$roi <- as.factor(long_four_tat2_inf$roi)

#Run the boxplot putamen=2, caudate=3, pallidum=4
ggplot(long_four_tat2_inf, aes(x=roi, y=tat2)) + 
  geom_boxplot(fill="grey") +  scale_y_reverse() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black")) 

##Load the data in wide format
allstri_tat2roi <- read.csv("")

#Example line plot
ggplot(allstri_tat2roi, aes(x=Age, y=putamen)) + ylab("nt2s_putamen")+ xlab("Postnatal Age") + 
  geom_point(shape=21, fill="darkgrey", color="black", size=3) + geom_smooth(method = lm, color="orange", se=FALSE) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_y_reverse()

#Example t.test between regions
t.test(allstri_tat2roi$putamen,allstri_tat2roi$pallidum, paired = TRUE)

#Example linear model. Models were run for each roi separately 
lnmdl_age <- lm(putamen ~ Ageweeks + ga_birth + BW + Sex + OBrisk + education + mean_fd, data=allstri_tat2roi)

#Example linear model with interaction term

#Mean center the age terms
allstri_tat2roi$ga_birth_mnc <- allstri_tat2roi$ga_birth-(mean(allstri_tat2roi$ga_birth))
allstri_tat2roi$Ageweeks_mnc <- allstri_tat2roi$Ageweeks-(mean(allstri_tat2roi$Ageweeks))

lnmdl_age <- lm(pallidum ~ Ageweeks_mnc + ga_birth_mnc + Ageweeks_mnc*ga_birth_mnc + BW + Sex + OBrisk + education + mean_fd, data=allstri_tat2roi)

#Plot interaction
lnmdl_age_int <- lm(pallidum ~ Age*ga_birth + BW + Sex + OBrisk + education + mean_fd, data=allstri_tat2roi)
visreg(lnmdl_age_int, "Age", by="ga_birth", overlay=TRUE)

#Example linear model with brain volume
lnmdl_age <- lm(pallidum ~ Ageweeks + ga_birth + BW + Sex + OBrisk + education + mean_fd + BrainSegVolNotVent, data=allstri_tat2roi)

#Correlation for brain volume
cor.test(allstri_tat2roi$BrainSegVolNotVent,allstri_tat2roi$BW)

#Assess colinear variables
model_colin <- lm(pallidum ~ Age + ga_birth, allstri_tat2roi)
ols_vif_tol(model_colin)

##Child stats

allstri_tat2_common <- read.csv("")

#T test
t.test(allstri_tat2_common$putamen,allstri_tat2_common$pallidum, paired = TRUE)

#linear model
lnmdl_preschool_common <- lm(pallidum ~ pre_scan_age + ga_birth, data=allstri_tat2_common)


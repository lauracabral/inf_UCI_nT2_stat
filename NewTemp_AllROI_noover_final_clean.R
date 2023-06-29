#Let's make a final draft of the new stats

#rm(list=ls())

library(stringr)
library(tidyverse)
library(ggpubr)
library(tidyr)
library("data.table")
library(psych)

inf_tat <- read.csv("", stringsAsFactors = FALSE)

inf_tat$subnum=str_extract(inf_tat$subj,'_\\d+')
inf_tat$idchild=str_extract(inf_tat$subnum,'\\d+')

inf_tat <- inf_tat %>% filter (roi ==3)

infga <- read.csv("", stringsAsFactors = FALSE)
Age_p <- read.csv("", stringsAsFactors = FALSE)

#extract the subject from Age
Age_p$idchild=str_extract(Age_p$Subj,'\\d+')
#merge them together
agetat2 <- merge(inf_tat, Age_p, by.x = "idchild", by.y = "idchild")

#merge the ga from the seperate sheet
#lose one participant here for missing variable missing ga (dc?)
agetat2 <- merge(agetat2,infga)
agetat2$pma <- (agetat2$Age/7) + agetat2$ga_birth

four_tat2roi = data.frame(agetat2$idchild,agetat2$tat2,agetat2$roi,agetat2$Age,agetat2$ga_birth)
names(four_tat2roi) <- c("idchild", "tat2", "roi", "Age", "ga_birth")

wide_four = four_tat2roi %>% spread(roi,tat2)
names(wide_four) <- c("idchild", "Age", "ga_birth","caudate")

#run the pallidum 
inf_tat_pal <- read.csv("", stringsAsFactors = FALSE)

inf_tat_pal$subnum=str_extract(inf_tat_pal$subj,'_\\d+')
inf_tat_pal$idchild=str_extract(inf_tat_pal$subnum,'\\d+')

one_tat2roi = data.frame(inf_tat_pal$idchild,inf_tat_pal$tat2)
names(one_tat2roi) <- c("idchild", "pallidum")

allstri_tat2roi <- merge(one_tat2roi,wide_four)

#run the putamen
inf_tat_put <- read.csv("", stringsAsFactors = FALSE)

inf_tat_put$subnum=str_extract(inf_tat_pal$subj,'_\\d+')
inf_tat_put$idchild=str_extract(inf_tat_pal$subnum,'\\d+')

two_tat2roi = data.frame(inf_tat_put$idchild,inf_tat_put$tat2)
names(two_tat2roi) <- c("idchild", "putamen")

allstri_tat2roi <- merge(two_tat2roi,allstri_tat2roi)

#Make it long and clean for box plot

box_three <- data.frame(four_tat2roi$idchild,four_tat2roi$tat2, four_tat2roi$roi)
names(box_three) <- c("idchild", "tat2","roi")


one_tat2roi$roi <- 4
names(one_tat2roi) <- c("idchild", "tat2","roi" )

two_tat2roi$roi <- 2
names(two_tat2roi) <- c("idchild", "tat2","roi" )


long_four_tat2_inf <- rbind(one_tat2roi,box_three)
long_four_tat2_inf <- rbind(two_tat2roi,long_four_tat2_inf)
long_four_tat2_inf$roi <- as.factor(long_four_tat2_inf$roi)
#long_four_tat2_inf <- long_four_tat2_inf %>% filter (roi !=1)


#Run some age models

#lnmdl_age <- lm(caudate ~ Age + ga_birth, data=allstri_tat2roi)

#Now we need to test some of the covariates so we can write up the final stats
inf <- read.csv("", stringsAsFactors = FALSE )
infsex <- read.csv("", stringsAsFactors = FALSE)
infob <- read.csv("", stringsAsFactors = FALSE)

##birthweight  
infbw <- read.csv("", stringsAsFactors = FALSE)


#Motion
#Source the Motion.R script

allstri_tat2roi <- merge(allstri_tat2roi,motion)
allstri_tat2roi <- allstri_tat2roi %>% filter (mean_fd<3)

#### THIS IS OUR PLOT DIVISION

#Boxplot after that participant
long_four_tat2_inf <- subset(long_four_tat2_inf, idchild %in% allstri_tat2roi$idchild)
ggplot(long_four_tat2_inf, aes(x=roi, y=tat2)) + 
  geom_boxplot(fill="grey") +  scale_y_reverse() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black")) 
#Can run the line plots here

ggplot(allstri_tat2roi, aes(x=Age, y=pallidum)) + ylab("nt2s_putamen")+ xlab("Postnatal Age") + 
  geom_point(shape=21, fill="darkgrey", color="black", size=3) + geom_smooth(method = lm, color="orange", se=FALSE) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_y_reverse()

ggplot(allstri_tat2roi, aes(x=ga_birth, y=putamen)) + ylab("nt2s_putamen")+ xlab("Gestational Age") + 
  geom_point(shape=21, fill="darkgrey", color="black", size=3) + geom_smooth(method = lm, color="orange", se=FALSE) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_y_reverse()


lnmdl_age <- lm(putamen ~ Age + ga_birth + mean_fd, data=allstri_tat2roi)


#Birthweight
BW = data.frame(infbw$idchild,infbw$BW)
names(BW) <- c("idchild", "BW")
allstri_tat2roi <- merge(allstri_tat2roi,BW)

lnmdl_age <- lm(putamen ~ Age + ga_birth + BW + mean_fd, data=allstri_tat2roi)

ggplot(allstri_tat2roi, aes(x=BW, y=pallidum)) + ylab("nt2s_pallidum")+ xlab("BirthWeight") + 
  geom_point(shape=21, fill="darkgrey", color="black", size=3) + geom_smooth(method = lm, color="orange", se=FALSE) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_y_reverse()

#Sex 

allstri_tat2roi <- merge(allstri_tat2roi,infsex)
allstri_tat2roi$Sex <- as.factor(allstri_tat2roi$Sex)

lnmdl_age <- lm(putamen ~ Age + ga_birth + BW + Sex + mean_fd, data=allstri_tat2roi)

#Obsetric Risk

allstri_tat2roi <- merge(allstri_tat2roi,infob)
allstri_tat2roi$OBrisk <- as.factor(allstri_tat2roi$OBrisk)

allstri_tat2roi$pma <- (allstri_tat2roi$Age/7) + allstri_tat2roi$ga_birth

lnmdl_age <- lm(putamen ~ Age + ga_birth + BW + Sex + OBrisk + mean_fd, data=allstri_tat2roi)

#SES

inf$idchild <- inf$idmaternal+1000

inf$educationscr <-NA
inf$educationscr[which(inf$education == "Primary, Elementary, or Middle School")] = 1 
inf$educationscr[which(inf$education == "High School or GED")] = 2
inf$educationscr[which(inf$education == "Technical or Vocational School")] =3
inf$educationscr[which(inf$education == "Some College, but no degree" )]  =3
inf$educationscr[which(inf$education == "Certificate" )]  =3
inf$educationscr[which(inf$education == "Associate's Degree")] =4
inf$educationscr[which(inf$education == "Bachelor's Degree")] =4
inf$educationscr[which(inf$education == "Graduate Degree")] =5

inf$hoshldinc <- NA
inf$hoshldinc[which(inf$household_income == "Below $15,000")]= 1
inf$hoshldinc[which(inf$household_income == "$15,000 - $29,999")]= 2
inf$hoshldinc[which(inf$household_income == "$30,000 - $49,999")]=3
inf$hoshldinc[which(inf$household_income == "$50,000 - $100,000")]=4
inf$hoshldinc[which(inf$household_income == "Over $100,000")]=5

#WOOT we've got a score for SES
inf$ses <- (inf$hoshldinc + inf$educationscr)/2

infses = data.frame(inf$idchild,inf$ses,inf$educationscr,inf$hoshldinc)
names(infses) <- c("idchild", "ses","education","houseinc")
allstri_tat2roi_ses <- merge(allstri_tat2roi,infses)

allstri_tat2roi_ses$Ageweeks <- allstri_tat2roi_ses$Age/7

lnmdl_age <- lm(putamen ~ Age + ga_birth + BW + Sex + OBrisk + education + mean_fd, data=allstri_tat2roi_ses)
lnmdl_age_weeks <- lm(putamen ~ Ageweeks + ga_birth + BW + Sex + OBrisk + education + mean_fd, data=allstri_tat2roi_ses)

####NOT AN AGE MODEL####

## This is the portion of the script that deals with the volume measures
#We're going to take the aseg_summ variable from its script and merge it with the ever growing agetat2bw
#Script is in this directory
allstri_tat2roi_ses_aseg <- merge(allstri_tat2roi_ses, aseg_summ, by.x = "idchild", by.y = "visit")

#do the corr test on the variable Finn and I picked. 
cor.test(allstri_tat2roi_ses_aseg$BrainSegVolNotVent,allstri_tat2roi_ses_aseg$BW)


lnmdl_age <- lm(pallidum ~ Ageweeks + ga_birth + BW + Sex + OBrisk + education + mean_fd + BrainSegVolNotVent, data=allstri_tat2roi_ses_aseg)

ggplot(allstri_tat2roi_ses_aseg, aes(x=BrainSegVolNotVent, y=BW)) + ylab("BW")+ xlab("BrainSegVolNotVent") + 
  geom_point(shape=21, fill="darkgrey", color="black", size=3) + geom_smooth(method = lm, color="orange", se=FALSE) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_y_reverse()

allstri_tat2roi_ses_aseg <- allstri_tat2roi

###### Make a subject list for the mean image
#sub_mean_list <- merge(inf_tat_pal,allstri_tat2roi_ses, by.x = "idchild", by.y = "idchild")

#sub_mean_list <- sub_mean_list$subj

#write.table(sub_mean_list, file = "inf_mean_sublst", append = FALSE, quote = FALSE, sep = " ",
           # eol = "\n", na = "NA", dec = ".", row.names = FALSE,
           # col.names = FALSE)



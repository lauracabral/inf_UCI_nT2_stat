#This is a new version of childtat2_split.R to do the putamen without the overlapping voxel
#See that script for more notes about which tat2 files and what test files are which.

#These are the files with motion censoring
#chilt2pall <- read.table('~/Documents/infantUCI/tat2_pallidum_mtchinf_mtn.txt', header = TRUE, sep = ",")
#chilt2split <- read.table('~/Documents/infantUCI/tat2_split_mtchinf_mtn.txt', header = TRUE, sep = ",")

#These are the files without motion censoring as confirmed by filepaths and 3dinfo
#This code is gross, but it was one of my first r scripts. 

rm(list=ls())

library(stringr)
library(tidyverse)
library(ggpubr)
library(tidyr)
library("data.table")
library(psych)

#The pallidum
chilt2pall <- read.table('', header = TRUE, sep = ",")

#for the putamen, 
chilt2put <- read.table('', header = TRUE, sep = ",")

#Going to pull the caudate from here
chilt2split <- read.table('', header = TRUE, sep = ",")

chilt2split$ChildID=str_extract(chilt2split$subj,'[!2]\\d+')
chilt2split$chiltat2 <- chilt2split$tat2
chilt2split$visnum <- str_extract(chilt2split$subj,'_0\\d+')
chilt2split$visnum <- str_extract(chilt2split$visnum, '\\d+')
#Missing from way extracted string
chilt2split$visnum[2]=10132018
chilt2split$subvis <- paste(chilt2split$ChildID, chilt2split$visnum, sep= "_")


#Get rid of other roi and screen out repeated subj. One doesn't carry over to common subjects
chilt2split <- chilt2split %>% filter (roi !=2)
chilt2split <- chilt2split %>% filter (subvis !="2139_05052018", subvis !="2119_03182018", subvis !="2165_04072018", subvis !="2184_09222018", subvis !="2087_03032018")

chilt2split_v2 = data.frame(chilt2split$ChildID,chilt2split$tat2,chilt2split$roi)
names(chilt2split_v2) <- c("idchild", "tat2","roi")

#Spread it so we can merge
wide_three_child = chilt2split_v2 %>% spread(roi,tat2)
names(wide_three_child) <- c("idchild","caudate")

#Need to do the same for the other two rois. let's do the pallidum first because we have code for it
#Pallidum
chilt2pall$ChildID=str_extract(chilt2pall$subj,'[!2]\\d+')
chilt2pall$chiltat2 <- chilt2pall$tat2
chilt2pall$visnum <- str_extract(chilt2pall$subj,'_0\\d+')
chilt2pall$visnum <- str_extract(chilt2pall$visnum, '\\d+')
chilt2pall$visnum[2]=10132018
chilt2pall$subvis <- paste(chilt2pall$ChildID, chilt2pall$visnum, sep= "_")
chilt2split_pall <- chilt2pall %>% filter (subvis !="2139_05052018", subvis !="2119_03182018", subvis !="2165_04072018", subvis !="2184_09222018", subvis !="2087_03032018")

##Let's just make this cute and put it in one array
chilt2split_v3 = data.frame(chilt2split_pall$ChildID,chilt2split_pall$tat2,chilt2split_pall$roi)
names(chilt2split_v3) <- c("idchild", "tat2","roi")
wide_three_child_pall= chilt2split_v3 %>% spread(roi,tat2)
names(wide_three_child_pall) <- c("idchild", "pallidum")

#Putamen
chilt2put$ChildID=str_extract(chilt2put$subj,'[!2]\\d+')
chilt2put$chiltat2 <- chilt2put$tat2
chilt2put$visnum <- str_extract(chilt2put$subj,'_0\\d+')
chilt2put$visnum <- str_extract(chilt2put$visnum, '\\d+')
chilt2put$visnum[2]=10132018
chilt2put$subvis <- paste(chilt2put$ChildID, chilt2put$visnum, sep= "_")
chilt2split_put <- chilt2put %>% filter (subvis !="2139_05052018", subvis !="2119_03182018", subvis !="2165_04072018", subvis !="2184_09222018", subvis !="2087_03032018")


##Let's just make this cute and put it in one array
chilt2split_v4 = data.frame(chilt2split_put$ChildID,chilt2split_put$tat2,chilt2split_put$roi)
names(chilt2split_v4) <- c("idchild", "tat2","roi")

wide_three_child_put= chilt2split_v4 %>% spread(roi,tat2)
names(wide_three_child_put) <- c("idchild", "putamen")

#Merge the dataframes
allstri_tat2roi_child <- merge(wide_three_child,wide_three_child_pall)
allstri_tat2roi_child <- merge(allstri_tat2roi_child,wide_three_child_put)

#Add preschool age
preschool_age <- read.csv("", stringsAsFactors = FALSE)
allstri_tat2roi_child <- merge(allstri_tat2roi_child,preschool_age)

ggplot(allstri_tat2roi_child, aes(x=pre_scan_age, y=pallidum)) + ylab("nt2s_pallidum")+ xlab("Postnatal Age") + 
  geom_point(shape=21, fill="darkgrey", color="black", size=3) + geom_smooth(method = lm, color="orange", se=FALSE) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_y_reverse()

lnmdl_preschool <- lm(pallidum ~ pre_scan_age, data=allstri_tat2roi_child)

#With GA
infga <- read.csv("", stringsAsFactors = FALSE)
allstri_tat2roi_child <- merge(allstri_tat2roi_child,infga)

lnmdl_preschool <- lm(pallidum ~ pre_scan_age + ga_birth, data=allstri_tat2roi_child)

chilt2split_v3$roi <- 4
chilt2split_v4$roi <- 2

long_child_allstri <- rbind(chilt2split_v3,chilt2split_v2)
long_child_allstri <- rbind(long_child_allstri,chilt2split_v4)

long_child_allstri$roi <- as.factor(long_child_allstri$roi)

ggplot(long_child_allstri, aes(x=roi, y=tat2)) + 
  geom_boxplot(fill="grey") +  scale_y_reverse() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black")) 
#Let's find the common subjects with infants

commonsublst <- read.csv("", stringsAsFactors = FALSE, header = FALSE)

allstri_tat2_common <- subset(allstri_tat2roi_child, idchild %in% commonsublst$V1)

allstri_tat2_common_long <- subset(long_child_allstri, idchild %in% commonsublst$V1)

ggplot(allstri_tat2_common_long, aes(x=roi, y=tat2)) + 
  geom_boxplot(fill="grey") +  scale_y_reverse() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black")) 


lnmdl_preschool_common <- lm(pallidum ~ pre_scan_age + ga_birth, data=allstri_tat2_common)

ggplot(allstri_tat2_common, aes(x=pre_scan_age, y=pallidum)) + ylab("nt2s_pallidum")+ xlab("Postnatal Age") + 
  geom_point(shape=21, fill="darkgrey", color="black", size=3) + geom_smooth(method = lm, color="orange", se=FALSE) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_y_reverse()

ggplot(allstri_tat2_common, aes(x=pre_scan_age, y=putamen)) + ylab("nt2s_putamen")+ xlab("Postnatal Age") + 
  geom_point(shape=21, fill="darkgrey", color="black", size=3) + geom_smooth(method = lm, color="orange", se=FALSE) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_y_reverse()

ggplot(allstri_tat2_common, aes(x=pre_scan_age, y=caudate)) + ylab("nt2s_caudate")+ xlab("Postnatal Age") + 
  geom_point(shape=21, fill="darkgrey", color="black", size=3) + geom_smooth(method = lm, color="orange", se=FALSE) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + scale_y_reverse()

###### Make a subject list for the mean image
#sub_mean_list_child <- merge(chilt2split_pall,allstri_tat2_common, by.x = "ChildID", by.y = "idchild")

#sub_mean_list_child <- sub_mean_list_child$subj

#write.table(sub_mean_list_child, file = "child_mean_sublst_flipscan_v2", append = FALSE, quote = FALSE, sep = " ",
 #           eol = "\n", na = "NA", dec = ".", row.names = FALSE,
  #          col.names = FALSE)





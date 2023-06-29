
#Get the subject list that $f would have made because there aren't any subj 
#lists in the freesurfer output and no way to set them
sublst <- read.table("", header = FALSE, sep=" ")
df <- as.data.frame(t(sublst))
df$visit=str_extract(df$V1,'\\d+')

#Going to merge this with the table that has the outputs

summ <- read.table(file = '', header = TRUE)

#Merge with the subject list

aseg_summ <- cbind(summ,df)


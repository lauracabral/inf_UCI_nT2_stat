#Laura doing a quick test for her sanity. 
#Going to model the number of voxels for the BW variable.
 

infnumvox <- read.csv("")


#get the childid in the same format
infnumvox$idchild=str_extract(infnumvox$V1,'_\\d+')
infnumvox$idchild=str_extract(infnumvox$idchild,'\\d+')
infnumvox$numvox <- infnumvox$V2



agetat2bw <- merge(allstri_tat2roi, infnumvox, by.y = "idchild", by.x ="idchild")
cor.test(agetat2bw$numvox,agetat2bw$BW)



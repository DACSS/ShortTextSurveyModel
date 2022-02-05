#Set working directory, load processed data#
setwd("C:/Users/Tim/Dropbox/Democracy Meaning/Survey Re-Analysis/")
# setwd("~/Dropbox/Democracy Meaning/Survey Re-Analysis/")
load("freeResponseDataUPDATED.RData")
#Create coding DF with id, column names of terms, and empty type column; write it#
codeDF<-data.frame(id=1:ncol(cat.ttm),term=as.character(colnames(cat.ttm)),type=NA)
write.csv(codeDF,"C:/Users/Tim/Dropbox/R Projects/DefDemPlay/category_type_coding.csv",row.names=F)

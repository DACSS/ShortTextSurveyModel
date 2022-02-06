#Set working directory, libraries, and load processed data

library(igraph)
library(sna)
library(ggplot2)
library(GGally)
library(stringr)
library(dplyr)
library(tidyr)
load("Cleaned Scripts and Data/freeResponseDataCurrent.RData")

#Turn the data back to individual-level dataframe#
cat.df<-data.frame(cat.dtm) #Document term matrix to dataframe
cat.df$fh<-rowSums(cat.dtm[,c("voting","party","govt","rights")]) #Row-level sum of Freedom House topics
cat.df$fh2<-rowSums(cat.dtm[,c("voting","party","govt","civil","law")]) #Same as above, expanded
defdem<-cbind(defdem,cat.df) #Add the respondent-level data to the processed responses
colnames(defdem) #Identify columns with free response categories
cols<-c(107:139)
sort(colMeans(defdem[,cols])) #Get means of category invocation across respondents

#Graph agreement along principles and institutions#
#Free responses#
colMeans<-data.frame(name=names(sort(colMeans(defdem[,cols]))), #Dataframe of topic means
                     proportion=sort(colMeans(defdem[,cols])))
row.names(colMeans)<-NULL
sorted<-as.character(colMeans$name[order(-colMeans$proportion)])
sorted<-sorted[sorted!="misc"]
tops<-sorted[1:20]
longNames<-c("Freedom","Voting","USA","Fair","Government","Representation","Equality",
             "Party","Elections","Rights","Choice","Voice","US Government","Rule","Fail",
             "Majority","Economy","Community","Law","People")
plotDF<-data.frame(id=1:length(tops),short=tops,long=longNames,proportion=colMeans$proportion[match(tops,colMeans$name)])
# print(cbind(1:nrow(colMeans),as.character(colMeans$name)))
# nums<-c(26,24,27,10,32,29,20,1,31,5,9,30) #Identify free response topic locations
# colMeansSub<-colMeans[nums,] #Reduce dataframe to relevant free responses
# colMeansSub$name #Ensure they're all accounted for
# colMeansSub$name2<-c("Choice","Party","Equality","People","Freedom","Rights", #Re-name for plotting
#                  "Law","Government","Voting","West","Other Countries","USA")

category_key<-read.csv("Cleaned Scripts and Data/category_type_coding.csv",header=T,stringsAsFactors=F) #Read in the hand-coded key of category types
plotDF$Legend<-category_key$type_long[match(plotDF$short,category_key$name)] #Add type classifier
plotDF$type<-factor(plotDF$Legend, levels=c("Institution", "Value","Group","Misc")) #Factor it

#Differentiate dataframes by type to order by prominence#
plotDFSub1<-plotDF[plotDF$type=="Institution",]
plotDFSub1<-plotDFSub1[order(-plotDFSub1$proportion),]
plotDFSub2<-plotDF[plotDF$type=="Value",]
plotDFSub2<-plotDFSub2[order(-plotDFSub2$proportion),]
plotDFSub3<-plotDF[plotDF$type=="Group",]
plotDFSub3<-plotDFSub3[order(-plotDFSub3$proportion),]
plotDFSub4<-plotDF[plotDF$type=="Misc",]
plotDFSub4<-plotDFSub4[order(-plotDFSub4$proportion),]
#Omitting the miscellaneous category in the plot - only one in this sample#
plotDFSubF<-rbind(plotDFSub1,plotDFSub2,plotDFSub3) #Re-combine them for plotting
plotDFSubF$proportionSmall<-round(plotDFSubF$proportion,2)

#Write out the plot for the proportions of shared open response categories chosen above
# ggplot(plotDFSubF,aes(x=reorder(long,proportion),y=proportion,fill=Legend)) +
#   geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3) +
#   xlab("Theme from Open-Ended Questions") +
#   ylab("Proportion of Respondents Stating Category is...") +
#   ggtitle("Shared Responses in Open Response Themes") + 
#   theme_bw(base_size = 10, base_family = "Helvetica") +
#   geom_text(aes(label=proportionSmall,fontface=3),size=3,
#             position=position_dodge(width=0.9),vjust=-0.25) +
#   theme(plot.title = element_text(face="bold"),
#         axis.text=element_text(angle = 30, hjust=1)) +
#   scale_y_continuous(limits=c(0,1),expand=c(0,0)) +
#   facet_grid(.~type,scale="free_x")
# dev.off()

#Stacked plots of fundamental, excluded, and the non-interactive proportions#
fund<-names(defdem)[grep("Fund",names(defdem))] #Identifying column names from fundamental question
not<-names(defdem)[grep("Not",names(defdem))] #Identifying column names from excluded question
themes<-gsub("Fund","",fund)  #Identifying theme names
df<-data.frame(theme=as.character(themes), #Build hollow data frame for proportions of respondents
               fund=rep(NA,length(themes)),
               mid=rep(NA,length(themes)),
               not=rep(NA,length(themes)),
               sum=rep(NA,length(themes)))
for(i in 1:nrow(df)){ #Cycle through the hollow dataframe and fill it
  t<-as.character(df$theme[i]) #Cycle through themes 1 by 1
  cols<-grep(t,names(defdem)) #Get relevant columns for theme 
  fcol<-cols[2] #Identify column with fundamental for theme
  ncol<-cols[1] #Identify column with excluded for theme
  df$fund[i]<-sum(defdem[,fcol])/nrow(defdem) #Proportion marked fundamental
  df$not[i]<-sum(defdem[,ncol])/nrow(defdem) #Proportion marked excluded
  df$mid[i]<-1-(df$fund[i]+df$not[i]) #Proportion not interacting with this theme
  df$sum[i]<-sum(df$fund[i],df$not[i],df$mid[i]) #Ensuring this reaches 1
}
df$theme<-as.character(df$theme) #Ensure they're characters
View(df) #View the data
df$long<-c("Free Market Economy","Healthcare for All","Laws Protecting Private Property",
           "Peaceful Transfers of Power","Free and Open Elections","Univ. Adult Suffrage",
           "Rep. Government","Equality","Civil Rights","Bill of Rights","Civil Society",
           "Basic Human Rights","Protest","Constitution","Educated Population","Western Values",
           "Strong Nat'l Defense") #Assign plotting names for themes
dfCont<-subset(df,fund > 0.1 & not > 0.1) #Subset themes with high contestation (>0.1 on each)
fund<-data.frame(cbind(df$long,df$fund,rep("Fundamental",nrow(df)))) #Isolate by theme-proportion-type
names(fund)<-c("theme","fund","Sentiment")
mid<-data.frame(cbind(df$long,df$mid,rep("Ambivalent",nrow(df))))
names(mid)<-c("theme","fund","Sentiment")
not<-data.frame(cbind(df$long,df$not,rep("Excluded",nrow(df))))
names(not)<-c("theme","fund","Sentiment")
fin<-rbind(fund,mid,not) #Recombine
fin<-data.frame(theme=fin[,1], #Reformat properly
                proportion=as.numeric(as.character(fin[,2])),
                sentiment=fin[,3])
fin$theme<-as.character(fin$theme) #Make both columns characters
fin$Sentiment<-as.character(fin$sentiment)
temp<-fin[fin$Sentiment=="Fundamental",]
nom<-temp[order(temp$proportion),1]
fin$type<-factor(fin$Sentiment, levels=c("Fundamental","Ambivalent","Excluded"))
fin2<-subset(fin,Sentiment!="Ambivalent") #Get version without ambivalent portion
temp2<-fin2[fin$sentiment=="Fundamental",]
nom2<-temp2[order(temp2$proportion),1]
fin2$type<-factor(fin2$Sentiment, levels=c("Fundamental", "Excluded"))

#Plot with and without ambivalent category below#
# #ggplot(fin,aes(x=theme,y=proportion,fill=Sentiment)) +
#   geom_bar(stat="identity",colour="black",size=0.3) +
#   xlab("Category from Closed-Ended Questions") +
#   ylab("Proportion of Respondents Stating Category is...") +
#   ggtitle("Shared Responses in Closed Response Categories") + 
#   theme_bw(base_size = 10, base_family = "Helvetica") +
#   theme(plot.title = element_text(face="bold"),
#         axis.text=element_text(angle = 30, hjust=1)) +
#   scale_y_continuous(limits=c(0,1.01),expand=c(0,0)) +
#   scale_x_discrete(limits=nom)   
# dev.off()
# ggplot(fin2,aes(x=theme,y=proportion,fill=Sentiment)) +
#   geom_bar(stat="identity",colour="black",size=0.3) +
#   xlab("Category from Closed-Ended Questions") +
#   ylab("Proportion of Respondents Stating Category is...") +
#   ggtitle("Shared Responses in Closed Response Categories") + 
#   theme_bw(base_size = 10, base_family = "Helvetica") +
#   theme(plot.title = element_text(face="bold"),
#         axis.text=element_text(angle = 30, hjust=1)) +
#   scale_y_continuous(limits=c(0,1.01),expand=c(0,0)) +
#   scale_x_discrete(limits=nom2)
# dev.off()

#Only by themes marked fundamental#
tdf0<-fund[order(-as.numeric(as.character(fund$fund))),1:2]
row.names(tdf0)<-1:nrow(tdf0)
tdf0$fund<-as.numeric(as.character(tdf0$fund))
tdfTop<-tdf0[tdf0$fund>0.4,]
tdfTop$name<-c("Free and Open Elections","Civil Rights","Bill of Rights",
               "Universal Adult Suffrage","Representative Government",
               "Basic Human Rights","Equality")
tdfF<-data.frame(name=tdfTop$name,
                 proportion=tdfTop$fund)
tdfF$legend<-ifelse(tdfF$proportion>=0.5,"Consensual","Contested")

# ggplot(data=tdfF, aes(x=factor(name), y=proportion, fill=legend)) +
#   geom_bar(stat="identity") + 
#   scale_x_discrete(limits=tdfF[order(-tdfF$proportion),1]) +
#   geom_hline(yintercept=.5,colour="black",linetype="dashed") +
#   labs(colour="Legend") + xlab("Theme from Free Response") + ylab("Frequency of Occurrence") + 
#   ggtitle("Consensual 'Fundamental' Themes from Closed-Ended Survey Questions") +
#   theme(plot.title = element_text(lineheight=.8, face="bold"),
#         axis.text=element_text(angle = 35, hjust=1))
# dev.off()

#Look for pattetns of cleavages along following dimensions:#
# -IVs: age, income, education, party, gender
# -DVs: free response, fund, not fund by category/type/principle/institution
#Construct table to show that generally not a big impact on DVs#

#Do more educated give more "correct" responses?#
#Test broadly with freedom house score#
t.test(defdem$fh~defdem$college)
t.test(defdem$fh2~defdem$college)
#Test with inclusion of common components#
t.test(defdem$FundFreeOpenElect~defdem$college)
t.test(defdem$FundUnivAdultSuff~defdem$college) #Significant +(~0.2)
t.test(defdem$FundUnivAdultSuff~defdem$voting) #Significant +(~0.17)
#Test with exclusion of common components#
t.test(defdem$NotFreeOpenElect~defdem$college)
t.test(defdem$NotUnivAdultSuff~defdem$college)
t.test(defdem$NotUnivAdultSuff~defdem$voting)
#Test with sample open response category#
t.test(defdem$voting~defdem$college) #Significant +(~0.1)

#Test relationship between gender and invocation of equality#
t.test(defdem$equal~defdem$gender)
t.test(defdem$NotEquality~defdem$gender) #Close to 0.05 significance
t.test(defdem$community~defdem$gender)
t.test(defdem$party~defdem$gender)
t.test(defdem$FundEduPop~defdem$gender) #Close

#Test coherence of related closed and open ended categories#
t.test(defdem$FundEquality~defdem$equal) 
t.test(defdem$FundFreeOpenElect~defdem$voting) 
t.test(defdem$FundFreeOpenElect~defdem$election)
t.test(defdem$FundUnivAdultSuff~defdem$voting)
t.test(defdem$FundUnivAdultSuff~defdem$election)
t.test(defdem$FundWestern~defdem$west)

#Systematized T-Tests#
#For all possible interactions, run t.test of control and theme interaction#
#For each: first construct data frame and then cycle through theme-control tests#
controls<-c("college","gender","rep","dem")

#'Not fundamental' t.tests#
notMat<-matrix(NA,17,4)
notMat<-data.frame(matrix(NA,17,4))
names(notMat)<-controls
row.names(notMat)<-names(defdem)[26:42]
for (i in 26:42){
  for (j in controls){
    print(paste(colnames(defdem)[i],j,signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2),sep=", "))
    notMat[match(names(defdem)[i],row.names(notMat)),match(j,controls)]<-signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2)
  }
}
View(notMat)
write.csv(notMat,"Category Exclusion T-Test Matrix.csv")

#Side test: educated do not exclude more in general#
t.test(defdem$excludeTot~defdem$college)

#'Fundamental' t.tests#
fundMat<-matrix(NA,17,4)
fundMat<-data.frame(matrix(NA,17,4))
names(fundMat)<-controls
row.names(fundMat)<-names(defdem)[seq(43,76,2)]
for (i in seq(43,76,2)){
  for (j in controls){
    print(paste(colnames(defdem)[i],j,signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2),sep=", "))
    fundMat[match(names(defdem)[i],row.names(fundMat)),match(j,controls)]<-signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2)
  }
}
View(fundMat)
write.csv(fundMat,"Category Fundamental T-Test Matrix.csv")

#Ranking t.tests#
fundRMat<-matrix(NA,17,4)
fundRMat<-data.frame(matrix(NA,17,4))
names(fundRMat)<-controls
row.names(fundRMat)<-names(defdem)[seq(44,76,2)]
for (i in seq(44,76,2)){
  for (j in controls){
    print(paste(colnames(defdem)[i],j,signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2),sep=", "))
    fundRMat[match(names(defdem)[i],row.names(fundRMat)),match(j,controls)]<-signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2)
  }
}
View(fundRMat)
write.csv(fundRMat,"Category Fundamental Ranking T-Test Matrix.csv")

#Free Response t.tests#
cols<-107:138
names(defdem)[cols]
#Quickly report results before filling data frame#
for (i in cols){
  for (j in controls){
    if(t.test(defdem[,i]~ defdem[,j])$p.value<0.06){
      print(paste(colnames(defdem)[i],j,
                  signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2),sep=", "))
    }
  }
}

freeMat<-matrix(NA,length(cols),4)
freeMat<-data.frame(matrix(NA,length(cols),4))
names(freeMat)<-controls
row.names(freeMat)<-names(defdem)[cols]
for (i in cols){
  for (j in controls){
    freeMat[match(names(defdem)[i],row.names(freeMat)),match(j,controls)]<-signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2)
  }
}
View(freeMat)
write.csv(freeMat,"Free Response T-Test Matrix.csv")

#Not Fundamental Correlations#
for (i in 26:42){
  for (j in c("college","gender","rep","dem")){
    print(paste(colnames(defdem)[i],j,
                signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2),sep=", "))
  }
}

###EDUCATION CLEAVAGE###
#Closed ended - barplot with error bars#
signif_themesFund<-row.names(fundMat[which(fundMat$college<0.06),])
signif_themesNot<-row.names(notMat[which(notMat$college<0.06),])
signif_themes<-c(signif_themesFund,signif_themesNot)
theme<-c(rep(signif_themes,2)) #By theme
groups<-c(rep("College",(length(theme)/2)),rep("No College",(length(theme)/2))) #Differentiate groups for plotting
n<-c(rep(sum(defdem$college==1),(length(theme)/2)),rep(sum(defdem$college!=1),(length(theme)/2))) #Set n value by total membership in data
df<-data.frame(Group=groups, #Build data frame needed to plot
               Theme=theme,
               n=n,
               Average=rep(NA,length(theme)),
               SD=rep(NA,length(theme)),
               SE=rep(NA,length(theme)),
               CI=rep(NA,length(theme)))
c<-as.character(unique(df$Theme)) #Revise to proper format
conf.interval=0.95 #Set interval for CI
for(i in c){ #Calculate it out
  rows<-grep(i,df$Theme)
  df[rows[1],4]<-mean(defdem[defdem$college==1,grep(i,names(defdem))])
  df[rows[1],5]<-sd(defdem[defdem$college==1,grep(i,names(defdem))])
  df[rows[1],6]<-(df[rows[1],5]/sqrt(df[rows[1],3]))
  ciMult<-qt(conf.interval/2 +0.5,(df[rows[1],3]-1))
  df[rows[1],7]<-df[rows[1],6]*ciMult
  df[rows[2],4]<-mean(defdem[defdem$college!=1,grep(i,names(defdem))])
  df[rows[2],5]<-sd(defdem[defdem$college!=1,grep(i,names(defdem))])
  df[rows[2],6]<-(df[rows[2],5]/sqrt(df[rows[2],3]))
  ciMult<-qt(conf.interval/2 +0.5,(df[rows[2],3]-1))
  df[rows[2],7]<-df[rows[2],6]*ciMult
}
View(df) #View results
df$Theme
df$Theme2<-c(rep(c("Free Market Economy","Universal Adult Suffrage", #Rename for plotting
                   "Representative Government","Western Values",
                   "Strong National Defense"),2))
df$type <- NA
df$type[grep("Fund",df$Theme)]<-"Fundamental"
df$type[grep("Not",df$Theme)]<-"Excluded"

##png("~/Dropbox/Democracy Meaning/Survey Re-Analysis/Education Closed Response.#png",width=8,height=5, units="in", res=300)
# ggplot(df,aes(x=Theme2,y=Average,fill=Group)) +
#   geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3) +
#   geom_errorbar(aes(ymin=Average-SE,ymax=Average+SE),size=0.3,width=0.2,position=position_dodge(0.9)) +
#   xlab("Category from Closed-Ended Questions") +
#   ylab("Proportion of Respondents Stating Category is...") +
#   ggtitle("Significant Differences in Closed Response Categories, by College Education") + 
#   theme_bw(base_size = 10, base_family = "Helvetica") +
#   theme(plot.title = element_text(face="bold"),
#         axis.text=element_text(angle = 30, hjust=1)) +
#   scale_y_continuous(limits=c(0,1),expand=c(0,0)) +
#   facet_grid(.~type,scale="free_x")
# dev.off()

#Open ended - barplot with error bars#
signif_themesOpen<-row.names(freeMat[which(freeMat$college<0.06),])
theme<-c(rep(signif_themesOpen,2)) #By theme
groups<-c(rep("College",length(theme)/2),rep("No College",length(theme)/2))
# theme<-c(rep(c("voting","rep","human","equal","trad","conflict"),2))
n<-c(rep(sum(defdem$college==1),length(theme)/2),rep(sum(defdem$college!=1),length(theme)/2))
df<-data.frame(Group=groups,
               Theme=theme,
               n=n,
               Average=rep(NA,length(theme)),
               SD=rep(NA,length(theme)),
               SE=rep(NA,length(theme)),
               CI=rep(NA,length(theme)))
c<-as.character(unique(df$Theme))
conf.interval=0.95
names(defdem)
for(i in c){
  rows<-grep(i,df$Theme)
  df[rows[1],4]<-mean(defdem[defdem$college==1,max(grep(i,names(defdem)))])
  df[rows[1],5]<-sd(defdem[defdem$college==1,max(grep(i,names(defdem)))])
  df[rows[1],6]<-(df[rows[1],5]/sqrt(df[rows[1],3]))
  ciMult<-qt(conf.interval/2 +0.5,(df[rows[1],3]-1))
  df[rows[1],7]<-df[rows[1],6]*ciMult
  df[rows[2],4]<-mean(defdem[defdem$college!=1,max(grep(i,names(defdem)))])
  df[rows[2],5]<-sd(defdem[defdem$college!=1,max(grep(i,names(defdem)))])
  df[rows[2],6]<-(df[rows[2],5]/sqrt(df[rows[2],3]))
  ciMult<-qt(conf.interval/2 +0.5,(df[rows[2],3]-1))
  df[rows[2],7]<-df[rows[2],6]*ciMult
}
View(df)
df$Theme
df$Theme2<-c("Voting","Representation")
# df$Theme2<-c("Voting","Representation","Human Rights","Equality","Tradition","Conflict")
df$type<-c(rep("Open Response",length(theme)))
xlims<-df$Theme2[order(df$Average[df$Group=="College"])]

#png("Education Open Response.#png",width=6,height=4, units="in", res=300)
# ggplot(df,aes(x=Theme2,y=Average,fill=Group)) +
#   geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3) +
#   geom_errorbar(aes(ymin=Average-SE,ymax=Average+SE),size=0.3,width=0.2,position=position_dodge(0.9)) +
#   xlab("Component from Open-Ended Questions") +
#   ylab("% of Respondents Mentioning...") +
#   ggtitle("Educational Differences in Open Responses") + 
#   theme_bw(base_size = 12) +
#   theme(plot.title = element_text(face="bold"),
#         axis.text=element_text(angle = 30, hjust=1)) +
#   scale_y_continuous(limits=c(0,1),expand=c(0,0)) +
#   scale_x_discrete(limits=xlims) +
#   facet_grid(.~type,scale="free_x")
# dev.off()

###GENDER DIFFERENCES###
#Open responses#
signif_themesOpen<-row.names(freeMat[which(freeMat$gender<0.06),])
theme<-c(rep(signif_themesOpen,2)) #By theme
groups<-c(rep("Male",length(theme)/2),rep("Female",length(theme)/2))
n<-c(rep(nrow(defdem[defdem$gender==1,]),length(theme)/2),rep(nrow(defdem[defdem$gender==2,]),length(theme)/2))
df<-data.frame(Group=groups,
               Theme=theme,
               n=n,
               Average=rep(NA,length(theme)),
               SD=rep(NA,length(theme)),
               SE=rep(NA,length(theme)),
               CI=rep(NA,length(theme)))
c<-as.character(unique(df$Theme))
conf.interval=0.95
for(i in c){
  rows<-grep(i,df$Theme)
  df[rows[1],4]<-mean(defdem[defdem$gender==1,grep(i,names(defdem))[1]],na.rm=T)
  df[rows[1],5]<-sd(defdem[defdem$gender==1,grep(i,names(defdem))[1]],na.rm=T)
  df[rows[1],6]<-(df[rows[1],5]/sqrt(df[rows[1],3]))
  ciMult<-qt(conf.interval/2 +0.5,(df[rows[1],3]-1))
  df[rows[1],7]<-df[rows[1],6]*ciMult
  df[rows[2],4]<-mean(defdem[defdem$gender==2,grep(i,names(defdem))[1]],na.rm=T)
  df[rows[2],5]<-sd(defdem[defdem$gender==2,grep(i,names(defdem))[1]],na.rm=T)
  df[rows[2],6]<-(df[rows[2],5]/sqrt(df[rows[2],3]))
  ciMult<-qt(conf.interval/2 +0.5,(df[rows[2],3]-1))
  df[rows[2],7]<-df[rows[2],6]*ciMult
}
View(df)
df$Theme
df$Theme2<-c("Country","Tolerance")
# df$Theme2<-c("Equality","Other Countries","Community")
df$type<-c(rep("Open Response",length(theme)))
xlims<-df$Theme2[order(df$Average[df$Group=="Female"])]

#png("Gender Open Response.#png",width=6,height=4, units="in", res=300)
# ggplot(df,aes(x=Theme2,y=Average,fill=Group)) +
#   geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3) +
#   geom_errorbar(aes(ymin=Average-SE,ymax=Average+SE),size=0.3,width=0.2,position=position_dodge(0.9)) +
#   xlab("Components from Open-Ended Questions") +
#   ylab("% Rrespondents Mentioning...") +
#   ggtitle("Gender Differences in Open Response") + 
#   theme_bw(base_size = 12) +
#   theme(plot.title = element_text(face="bold"),
#         axis.text=element_text(angle = 30, hjust=1)) +
#   scale_y_continuous(limits=c(0,1),expand=c(0,0)) +
#   scale_x_discrete(limits=xlims) +
#   facet_grid(.~type,scale="free_x")
# dev.off()

#Closed response#
signif_themesFund<-row.names(fundMat[which(fundMat$gender<0.06),])
signif_themesNot<-row.names(notMat[which(notMat$gender<0.06),])
signif_themes<-c(signif_themesFund,signif_themesNot)
theme<-rep(signif_themes,2)
groups<-c(rep("Male",length(theme)/2),rep("Female",length(theme)/2))
n<-c(rep(nrow(defdem[defdem$gender==1,]),length(theme)/2),rep(nrow(defdem[defdem$gender==2,]),length(theme)/2))
df<-data.frame(Group=groups,
               Theme=theme,
               n=n,
               Average=rep(NA,length(theme)),
               SD=rep(NA,length(theme)),
               SE=rep(NA,length(theme)),
               CI=rep(NA,length(theme)))
c<-as.character(unique(df$Theme))
conf.interval=0.95
for(i in c){
  rows<-grep(i,df$Theme)
  df[rows[1],4]<-mean(defdem[defdem$gender==1,grep(i,names(defdem))[1]],na.rm=T)
  df[rows[1],5]<-sd(defdem[defdem$gender==1,grep(i,names(defdem))[1]],na.rm=T)
  df[rows[1],6]<-(df[rows[1],5]/sqrt(df[rows[1],3]))
  ciMult<-qt(conf.interval/2 +0.5,(df[rows[1],3]-1))
  df[rows[1],7]<-df[rows[1],6]*ciMult
  df[rows[2],4]<-mean(defdem[defdem$gender==2,grep(i,names(defdem))[1]],na.rm=T)
  df[rows[2],5]<-sd(defdem[defdem$gender==2,grep(i,names(defdem))[1]],na.rm=T)
  df[rows[2],6]<-(df[rows[2],5]/sqrt(df[rows[2],3]))
  ciMult<-qt(conf.interval/2 +0.5,(df[rows[2],3]-1))
  df[rows[2],7]<-df[rows[2],6]*ciMult
}
View(df)

df$Theme
df$Theme2<-c("Free Market Economy","Equality","Strong Nat'l Defense")
df$type<-c(rep("Excluded",length(theme)))

#png("~/Dropbox/Democracy Meaning/Survey Re-Analysis/Gender Closed Response.#png",width=8,height=5, units="in", res=300)
# ggplot(df,aes(x=Theme2,y=Average,fill=Group)) +
#   geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3) +
#   geom_errorbar(aes(ymin=Average-SE,ymax=Average+SE),size=0.3,width=0.2,position=position_dodge(0.9)) +
#   xlab("Category from Closed Ended Questions") +
#   ylab("Proportion of Respondents Stating Category is...") +
#   ggtitle("Significant Differences in Excluded Categories, by Gender") + 
#   theme_bw(base_size = 10, base_family = "Helvetica") +
#   theme(plot.title = element_text(face="bold"),
#         axis.text=element_text(angle = 30, hjust=1)) +
#   scale_y_continuous(limits=c(0,1),expand=c(0,0)) +
#   facet_grid(.~type,scale="free_x")
# dev.off()

###PARTISAN CLEAVAGES###
#Open responses#
names(defdem)
names(defdem)[103]<-"republican"

signif_themesOpenRep<-row.names(freeMat[which(freeMat$rep<0.06),])
signif_themesOpenDem<-row.names(freeMat[which(freeMat$dem<0.06),])
signif_themes<-unique(signif_themesOpenRep,signif_themesOpenDem)
theme<-rep(signif_themes,2)
groups<-c(rep("Republican",length(theme)/2),rep("Democrat",length(theme)/2))
n<-c(rep(sum(defdem$republican, na.rm=TRUE),length(theme)/2),rep(sum(defdem$dem, na.rm=TRUE),length(theme)/2))
df<-data.frame(Group=groups,
               Theme=theme,
               n=n,
               Average=rep(NA,length(theme)),
               SD=rep(NA,length(theme)),
               SE=rep(NA,length(theme)),
               CI=rep(NA,length(theme)))
c<-as.character(unique(df$Theme))
conf.interval=0.95
for(i in c){
  rows<-grep(i,df$Theme)
  df[rows[1],4]<-mean(defdem[defdem$republican==TRUE,i],na.rm=T)
  df[rows[1],5]<-sd(defdem[defdem$republican==TRUE,i],na.rm=T)
  df[rows[1],6]<-(df[rows[1],5]/sqrt(df[rows[1],3]))
  ciMult<-qt(conf.interval/2 +0.5,(df[rows[1],3]-1))
  df[rows[1],7]<-df[rows[1],6]*ciMult
  df[rows[2],4]<-mean(defdem[defdem$dem==TRUE,i],na.rm=T)
  df[rows[2],5]<-sd(defdem[defdem$dem==TRUE,i],na.rm=T)
  df[rows[2],6]<-(df[rows[2],5]/sqrt(df[rows[2],3]))
  ciMult<-qt(conf.interval/2 +0.5,(df[rows[2],3]-1))
  df[rows[2],7]<-df[rows[2],6]*ciMult
}
for(i in c){
  rows<-grep(i,df$Theme)
  df[rows[1],4]<-sum(defdem[defdem$republican==TRUE,i],na.rm=T)
  df[rows[2],4]<-sum(defdem[defdem$dem==TRUE,i],na.rm=T)
}
# library(Hmisc)
# df[,5:7]<-Hmisc::binconf(df$Success,df$n)
# df$Lower[9]<-df$Average[9]-2*sd(defdem$wordTot[defdem$republican], na.rm=TRUE)/sqrt(df$n[9])
# df$Upper[9]<-df$Average[9]+2*sd(defdem$wordTot[defdem$republican], na.rm=TRUE)/sqrt(df$n[9])
# df$Lower[18]<-df$Average[18]-2*sd(defdem$wordTot[defdem$dem], na.rm=TRUE)/sqrt(df$n[18])
# df$Upper[18]<-df$Average[18]+2*sd(defdem$wordTot[defdem$dem], na.rm=TRUE)/sqrt(df$n[18])
View(df)

df$Theme
df$Theme2<-c("People","Country","Voice")
df$Theme2<-ordered(df$Theme2,levels=c("Voice","People","Country"))
# df$Theme2<-c("Majority","Representation","Greece","Order","Nation","Voice","Election","Fair")
# df$Theme2<-ordered(df$Theme2, levels=c("Election","Representation","Majority","Greece","Fair","Voice","Order","Nation"))
df$type<-rep(c("Values","Groups","Groups"),2)
# df$type<-rep(c("Groups","Institutions", "Groups", "Values", "Groups","Values","Institutions","Values"),2)
df$type<-ordered(df$type,levels=c("Institutions","Values","Groups"))
t.test(defdem$wordTot~defdem$dem)
t.test(defdem$wordTot~defdem$republican)
levels(df[,1])<-c("Democrats=8.1","Republicans=7.2")
df$SE<-100*df$SE

#Hitting error with this plot...#
#png("Party Open Response SE Bars 2.#png",width=6,height=4, units="in", res=300)
# ggplot(df,aes(x=Theme2,y=Average,fill=Group)) +
#   geom_bar(position=position_dodge(),stat="identity",colour="black",size=0.3) +
#   geom_errorbar(aes(ymin=Average-SE,ymax=Average+SE),size=0.3,width=0.2,position=position_dodge(0.9)) +
#   xlab("Components from Open-Ended Questions") +
#   ylab("% Respondents Mentioning...") +
#   ggtitle("Partisan Differences in Open Response") + 
#   theme_bw(base_size = 12) +
#   theme(plot.title = element_text(face="bold"),
#         axis.text=element_text(angle = 30, hjust=1)) +
#   scale_y_continuous(limits=c(0,1),expand=c(0,0)) +
#   guides(fill=guide_legend(title="Total Words")) +
#   # scale_x_discrete(limits=c("Other Countries","Community","Equality")) +
#   facet_grid(.~type,scales="free")
# dev.off()
# 
#Plot the network of positive term connections#
#Pairwise coocurrance - turn significant co-occurances into graph (tie) format#
term.list<- make_empty_graph(n=32)
V(term.list)$name<-colnames(defdem)[107:138]
V(term.list)$mentions<-colSums(defdem[,107:138])
#Assign colors based on type#
type.inst<-c("voting","election","rep","govt","usgov","rights","law","human", "civil", "rule")
type.value<-c("justice","equal","fair","freedom","choice","voice","tolerance")
type.group<-c("people","majority","party","nation","community","usa","country")
V(term.list)$color<-ifelse(V(term.list)$name%in%type.inst,1,4)
V(term.list)$color<-ifelse(V(term.list)$name%in%type.value,2,V(term.list)$color)
V(term.list)$color<-ifelse(V(term.list)$name%in%type.group,3,V(term.list)$color)
V(term.list)$label<-c("Voting","Majority", "Parties", "Represent", "US Govt", "Govt.", "Rights", "Civil Rts.",
                      "Law", "Justice", "Human Rts.", "Equality", "Freedom", "Choice", "People", "USA",
                      "Greece", "West", "Order", "Nation", "Tradition", "Economic",
                      "Conflict", "Rule", "Good", "Fail", "Voice", "Change",
                      "Community", "Tolerance", "Election", "Fair")
V(term.list)$groupr<-c("Institutions","Values","Groups", "Misc.")[V(term.list)$color]
#Pair-wise t.tests, add to list with significance#
for (i in 107:138){
  for (j in 107:138){
    if(i!=j){
      w <- t.test(defdem[,i]~ defdem[,j])$statistic
      p<-t.test(defdem[,i]~ defdem[,j])$p.value
      term.list <- add_edges(term.list,c(i-106,j-106), weight=w,pvalue=p)
    }
  }
}
#Different subgraphs based on positive or negative relationships#
term.neg <- subgraph.edges(term.list, c(1:992)[E(term.list)$pvalue <0.05& E(term.list)$weight>0])
term.pos <- subgraph.edges(term.list, c(1:992)[E(term.list)$pvalue <0.05 & E(term.list)$weight<0])
E(term.pos)$weight<-abs(E(term.pos)$weight)
#Remove specific vertices from network#
term.pos<- delete_vertices(term.pos,c(1:29)[V(term.pos)$name%in%c("conflict","trad","west")])
#Get adjacency matrices#
pos<-as.matrix(as_adjacency_matrix(term.pos))
neg<-as.matrix(as_adjacency_matrix(term.neg))

#Plot this network to an image - may need to try multiple layouts to get desired visual#
#png("PositiveTermNetwork.#png",width=8,height=6, units="in", res=300)
# ggnet2(pos,node.size=V(term.pos)$mentions,node.label=V(term.pos)$label,
#        label.size=log(V(term.pos)$mentions),legend.position="bottom",
#        max_size=25,color=V(term.pos)$color,palette="Set1", alpha=.5)+
#   guides(size=FALSE)+
#   scale_color_brewer("", palette = "Set1",labels=c("Institutions","Values","Groups", "Misc"),
#                      guide = guide_legend(override.aes = list(size = 6)))
# dev.off()
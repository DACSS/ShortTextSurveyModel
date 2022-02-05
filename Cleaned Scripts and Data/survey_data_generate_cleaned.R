#Working directory and library#
# setwd("~/Dropbox/Democracy Meaning/Survey Re-Analysis/")
setwd("~/Desktop/R Project")

#setwd("C:/Users/Tim/Dropbox/Democracy Meaning/Survey Re-Analysis/")
library(tm)

#Read in the original data and extract open responses to allow matching#
study1<-read.csv("Conceptions_of_Democracy.csv",skip=1,stringsAsFactors = FALSE)
names(study1)[11:31]<-c("intro",paste("word",1:20,sep="")) #Renaming columns for analysis#
names(study1)[32:48]<-c("NotFreeMark","NotHealth","NotPrivProp","NotPeaceful",
                        "NotFreeOpenElect","NotUnivAdultSuff","NotRepGov","NotEquality",
                        "NotCivilRights","NotBillRights","NotCivSociety","NotBasicHR",
                        "NotProtest","NotConstitution",
                        "NotEduPop","NotWestern","NotStrongNatDef")
names(study1)[seq(49,82,2)]<-c("FundFreeMark","FundHealth","FundPrivProp","FundPeaceful",
                               "FundFreeOpenElect","FundUnivAdultSuff","FundRepGov",
                               "FundEquality","FundCivilRights","FundBillRights",
                               "FundCivSociety","FundBasicHR","FundProtest","FundConstitution",
                               "FundEduPop","FundWestern","FundStrongNatDef")
names(study1)[seq(50,82,2)]<-c("RankFreeMark","RankHealth","RankPrivProp","RankPeaceful",
                               "RankFreeOpenElect","RankUnivAdultSuff","RankRepGov",
                               "RankEquality","RankCivilRights","RankBillRights",
                               "RankCivSociety","RankBasicHR","RankProtest",
                               "RankConstitution","RankEduPop",
                               "RankWestern","RankStrongNatDef")
names(study1)[83:96]<-c("lang","lang.txt","foreign0","foreign.me","foreign.par","foreign.gp",
                        "income","age","gender","race","race.txt","educ","educ.txt","end")
study1<-study1[,c(1,6,8:10,12:95,97:99)] #Subset to relevant columns

#Create empty new columns for manipulation of responses#
study1$party3<-NA
study1$party.text<-NA
study1$party.rep<-NA
study1$party.dem<-NA
study1$party.lean<-NA

#Check overall response time#
#library(chron)
#quantile(chron::times(substr(study1$EndDate,12,19))-times(substr(study1$StartDate,12,19)),
#         probs=c(.1,.5,.8,.9,.95))

#Read in supplementary data with partisanship responses, repeat renaming#
study2<-read.csv("Conceptions_of_Democracy__with_PartyID.csv",skip=1,stringsAsFactors = FALSE)
names(study2)[11:31]<-c("intro",paste("word",1:20,sep=""))
names(study2)[32:48]<-c("NotFreeMark","NotHealth","NotPrivProp","NotPeaceful",
                        "NotFreeOpenElect","NotUnivAdultSuff","NotRepGov",
                        "NotEquality","NotCivilRights","NotBillRights",
                        "NotCivSociety","NotBasicHR","NotProtest","NotConstitution",
                        "NotEduPop","NotWestern","NotStrongNatDef")
names(study2)[seq(49,82,2)]<-c("FundFreeMark","FundHealth","FundPrivProp","FundPeaceful",
                               "FundFreeOpenElect","FundUnivAdultSuff","FundRepGov",
                               "FundEquality","FundCivilRights","FundBillRights",
                               "FundCivSociety","FundBasicHR","FundProtest",
                               "FundConstitution","FundEduPop","FundWestern","FundStrongNatDef")
names(study2)[seq(50,82,2)]<-c("RankFreeMark","RankHealth","RankPrivProp","RankPeaceful",
                               "RankFreeOpenElect","RankUnivAdultSuff","RankRepGov",
                               "RankEquality","RankCivilRights","RankBillRights",
                               "RankCivSociety","RankBasicHR","RankProtest",
                               "RankConstitution","RankEduPop","RankWestern","RankStrongNatDef")
names(study2)[83:95]<-c("lang","lang.txt","foreign0","foreign.me","foreign.par","foreign.gp",
                        "income","age","gender","race","race.txt","educ","educ.txt")
names(study2)[96:101]<-c("party3","party.text","party.rep","party.dem","party.lean","end")
study2<-study2[,c(1,6,8:10,12:100,102:104)] #Subset to relevant columns

#Merge data and remove missing values#
defdem<-rbind(study1,study2)
defdem<-defdem[defdem$word1!="",]

#Fix the 'Fund' and 'Not' variables to numeric#
for (i in grep("Fund",colnames(defdem))){
  defdem[,i]<-defdem[,i]+1
  defdem[,i]<-ifelse(is.na(defdem[,i]), 0, 1)
}
for (i in grep("Not",colnames(defdem))){
  defdem[,i]<-defdem[,i]+1
  defdem[,i]<-ifelse(is.na(defdem[,i]), 0, 1)
}
defdem<-defdem[defdem$Finished==1,] #Reduce to complete responses

#Some variable creation and recoding#
defdem$college<-defdem$educ>=5 #College indicator
defdem$party7<-defdem$party.dem #Quantify partisanship on 7 pt scale
defdem$party7[defdem$party3%in%c(3:5)]<-2+defdem$party.lean[defdem$party3%in%c(3:5)]
defdem$party7[240:432][is.na(defdem$party7[240:432])]<-8-defdem$party.rep[defdem$party3%in%c(1)]
defdem$party.strong<-abs(defdem$party7-4)
defdem$wordTot <-20-rowSums(defdem[,6:25]=="") #Calculate total word count per response
defdem$excludeTot<-rowSums(defdem[,grep("Not",colnames(defdem))]) #Count exclusions
defdem$rep<-defdem$party7>4 #Republican binary indicator
defdem$dem<-defdem$party7<4 #Democrat binary indicator
defdem$fund.proc<-rowSums(defdem[,c("FundRepGov","FundPeaceful", #Procedural norms sum
                                    "FundFreeOpenElect","FundUnivAdultSuff")]) 
defdem$fund.fh<-rowSums(defdem[,c("fund.proc","FundCivilRights","FundBillRights")]) #Freedomhouse sum
free<-defdem[,c(1,6:25)] #Subset free responses for coding and cleaning (see: clean_free_response.R)
# write.csv(free,"freeresponse_withid.csv",row.names=F) #Write this out to be spell-checked#

#########################################################
###Run clean_free_response.R, read in output file here###
#########################################################

#Repetition of some basic cleaning functions (to be sure)#
freeClean<-read.csv("Spell Checked and Substituted Free Response Vectors2.csv")
freeClean$subbed<-gsub("vot*\\b","voting",freeClean$subbed)
freeClean$subbed<-gsub("equal*\\b","equality",freeClean$subbed)
freeClean$subbed<-gsub("right[s]?\\>","rights",freeClean$subbed) #Note, this gets rid of 'righteous'
freeClean$subbed<-gsub("gov*\\b", "government",freeClean$subbed)
freeClean$subbed<-gsub("america*\\b","america",freeClean$subbed)
freeClean$subbed<-gsub("elect*\\b","election",freeClean$subbed)
freeClean$subbed<-gsub("represent*\\b","representation",freeClean$subbed)
freeClean$subbed<-gsub("free[^-][a-z]*\\b", "freedom", freeClean$subbed)
freeClean$subbed<-gsub("freedom[[:punct:]]+", "freedom", freeClean$subbed)
freeClean$subbed <-gsub("\\bfree\\b", "freedom", freeClean$subbed)
freeClean$subbed<-gsub("vot[a-z]*\\b","voting", freeClean$subbed)
freeClean$subbed<-gsub("\\b[a-z]*[[:punct:]]*equal[a-z]*\\b","equality", freeClean$subbed)
freeClean$subbed<-gsub("right[s]?\\>","rights", freeClean$subbed) ##note, this gets rid of righteous
freeClean$subbed<-gsub("gov[a-z]*\\b", "government", freeClean$subbed)
freeClean$subbed<-gsub("self[[:punct:]]government", "government", freeClean$subbed)
freeClean$subbed<-gsub("government[[:punct:]]*", "government", freeClean$subbed)
freeClean$subbed<-gsub("america[a-z]*\\b","america", freeClean$subbed)
freeClean$subbed<-gsub("america[[:punct:]]+","america", freeClean$subbed)
freeClean$subbed<-gsub("[s]*elect[a-z]*\\b","election", freeClean$subbed)
freeClean$subbed<-gsub("represent[a-z]*\\b", "representation", freeClean$subbed)
freeClean$subbed<-gsub("freedom[[:punct:]]+", "freedom", freeClean$subbed)
freeClean$subbed<-gsub("freedommark[a-z]*\\b","freedom market", freeClean$subbed)
freeClean$subbed<-gsub("united states", "UnitedStates", freeClean$subbed)
freeClean$subbed<-gsub("versus[[:punct:]]+", "versus", freeClean$subbed)
freeClean$subbed<-gsub("politic[a-z]*\\b", "politics", freeClean$subbed)
freeClean$subbed<-gsub("leaders[a-z]*\\b", "leader", freeClean$subbed)
freeClean$subbed<-gsub("president[a-z]*\\b", "president", freeClean$subbed,ignore.case=TRUE)
freeClean$subbed<-gsub("citizen[a-z]*\\b|citizen[']+s\\b", "citizen", freeClean$subbed,ignore.case=TRUE)
freeClean$subbed<-gsub("rule[s]*\\b", "rule", freeClean$subbed,ignore.case=TRUE)
freeClean$subbed<-gsub("participat[a-z]*\\b", "participation", freeClean$subbed,ignore.case=TRUE)
freeClean$subbed<-gsub("\\bpart[iy][^c][a-z]*", "party", freeClean$subbed,ignore.case=TRUE)
freeClean$subbed<-gsub("multi[[:punct:]]party", "party", freeClean$subbed)
freeClean$subbed<-gsub("corrupt[a-z]*\\b", "corrupt", freeClean$subbed)
freeClean$subbed<-gsub("\\begal[a-z]*\\b", "egalitarian", freeClean$subbed)
freeClean$subbed<-gsub("\\b[un]*fair[a-z]*[[:punct:]]*\\b", "fair", freeClean$subbed)
freeClean$subbed<-gsub("\\blaw[a-z]*\\b", "laws", freeClean$subbed)
freeClean$subbed<-gsub("\\bjust\\b", "justice", freeClean$subbed)
freeClean$subbed<-gsub("people[[:punct:]]ruled", "people rules", freeClean$subbed)
freeClean$subbed<-gsub("u[[:punct:]]s[[:punct:]]", "usa", freeClean$subbed)
freeClean$subbed<-gsub("choices","choice",freeClean$subbed)
freeClean$subbed<-gsub("choose","choice",freeClean$subbed)
freeClean$subbed<-gsub("voice[[:punct:]]*[s]*", "voice", freeClean$subbed)
freeClean$subbed<-gsub("everyone[[:punct:]]*", "everyone", freeClean$subbed)
freeClean$subbed<-gsub("everybody", "everyone", freeClean$subbed)
freeClean$subbed<-gsub("group[s]*", "group", freeClean$subbed)
freeClean$subbed<-gsub("voice[a-z]*", "voice", freeClean$subbed)
freeClean$subbed<-gsub("opinion[s]*", "opinion", freeClean$subbed)
freeClean$subbed<-gsub("debate[s]*", "debate", freeClean$subbed)
freeClean$subbed<-gsub("help[a-z]*\\b", "help", freeClean$subbed)
freeClean$subbed<-gsub("\\bflaw[a-z]*\\b", "flawed", freeClean$subbed)
freeClean$subbed<-gsub("\\bconservative[s]*\\b", "conservatives", freeClean$subbed)
freeClean$subbed<-gsub("\\btogether[a-z]*\\b", "together", freeClean$subbed)
freeClean$subbed<-gsub("\\bargu[a-z]*\\b", "argue", freeClean$subbed)
freeClean$subbed<-gsub("\\bunite[d]?\\b", "unity", freeClean$subbed)
freeClean$subbed<-gsub("people, people", "people", freeClean$subbed)
freeClean$subbed<-gsub("\\bpeople[[:punct:]]?[s]?\\b", "people", freeClean$subbed)
freeClean$subbed<-gsub("\\bpresidency\\b", "president", freeClean$subbed)
freeClean$subbed<-gsub("\\bdebat[a-z]*\\b", "debate", freeClean$subbed)
freeClean$subbed<-gsub("\\barm[s]*\\b","arms", freeClean$subbed)
freeClean$subbed<-gsub("\\bgun[s]*\\b", "guns", freeClean$subbed)
freeClean$subbed<-gsub("\\human[a-z]*\\b", "human", freeClean$subbed)
freeClean$subbed <- gsub("self[[:punct:]]government", "government", freeClean$subbed)
freeClean$subbed<-gsub("independencee","independence",freeClean$subbed)
freeClean$subbed<-gsub("peoplee","people",freeClean$subbed)
freeClean$subbed<-gsub("governmenternment","government",freeClean$subbed)
freeClean$subbed<-gsub("governmenter","govern",freeClean$subbed)

#Build corpus by respondent/free response with spell-checked data#

temp2<-freeClean[,c(2,5)]
colnames(temp2)<-c("doc_id","text")
mycorpus <- VCorpus(DataframeSource(temp2))
sub.dtm<-DocumentTermMatrix(mycorpus) #Create the document-term matrix
sub.ttm<-t(as.matrix(sub.dtm))%*%as.matrix(sub.dtm) #Create the term-term matrix

#Review how many respondents mentioned each concept at least once#
sub.dtm1<-as.matrix(sub.dtm)
sub.dtm1<-ifelse(sub.dtm1>0,1,0)
sub.ttm1<-t(as.matrix(sub.dtm1))%*%as.matrix(sub.dtm1)
sort(diag(sub.ttm1))


find.coloc<-function(ttm,term,low=0.2){ #Function to find colocates by term in a ttm
  l<-low*ttm[term,term] #Given a lower threshold 
  sort(ttm[,term][ttm[,term]>l])
}
# find.coloc(sub.ttm,"flag")
find.colocs<-function(ttm=ttm,termlist,low=0.1){ #Similar function with lower threshold
  len=dim(ttm)[1]
  t<-len[colnames(ttm)%in%termlist]
  total<-sum(diag(ttm)[t])
  sort(colSums(ttm[t,-t])[colSums(ttm[t,-t])>low*total])
}
# find.colocs(sub.ttm,c("flag","free"))

#Create categories for each concept, grouped by common recognition (FH)#
#First three correspond to freedom house political rights: voting, pluralism, functioning of govt#
cat.vote<-c("voting","ballot","participation","suffrage") #Voting and elections
cat.election<-c("election","polls","campaign")
cat.maj<-c("majority","plurality","majorities","mob")
cat.people<-c("people","popular","public","citizen") #People - sometimes "power of people", community
cat.party<-c("politics","party","democrat","republican","conservative","left","versus") #Partisan pluralism
cat.rep<-c("representation","republic","direct","federal")
cat.gov<-c("government") #Functioning of representative government
cat.usgov<-c("senate","house","president","congress","checks","balances","amendments",
             "veto","states")

#The next few correspond to freedom house civil liberties#
cat.legal<-c("constitution","laws","supremecourt","courts","court","process","trial",
             "bill","legal","illegal") #Rule of law
cat.justice<-c("justice","unjust")
cat.rights<-c("rights") #Rights
cat.human<-c("human") #Human rights (not in Freedom House)
cat.civil<-c("civil","speech","minority","religion","religious","church","arms",
             "guns","press","expression","amendement","bill") #Civil rights

#More generic terms - non-Freedom House#
cat.free<-c("freedom","liberty","independence") #Liberty appears 5 times without freedom,not independence
cat.equal<-c("equality","egalitarian") #Equality
cat.fair<-c("fair")
cat.usa<-c("america","unitedstates","usa","flag","founding","fathers","washington",
           "declaration","red","1776","land","statue","ellisisland") #America
# free[c(30,136,250,277,342,392),2:21] #Show that the flag is really USA
cat.country<-c("athens","greece","greek","greeks","rocks","india","china","french") #Greece/other countries
cat.voice<-c("voice","say","opinion","argue","speaking","debate","disagreement",
             "speech","expression") #Voice and choice
cat.choice<-c("choice","options","will")
cat.econ<-c("capitalism","money","opportunity", #Economic (not about opportunity - often tied to equality)
            "class","market","communism","welfare","money","working","handouts",
            "tax","debt","corporate","prosperous","monopolies",
            "prosperous","anti-corporatist","succeed","job")

#Other words that are not in a category, but are used >2 times#
cat.good<-c("good","hope","happiness","ideal","pride","great","happy","helpful",
            "helping","honest","love","supportive","kind","kindness","goodness")
cat.fail<-c("corrupt","lie","lies","war","wars","alienating","chaos","mob","shady",
            "cliquish","dishonest","slow","mean","flawed","abuse")
cat.change<-c("modern","progress","change","progressive","old","young","growth")
cat.community<-c("unity","community","together","collaboration","concensus",
                 "everyone","family","group") 
cat.nation<-c("nation","patriotic","flag")
cat.conflict<-c("peace","peaceful","violence","civil war","revolution","calmer society")
cat.tolerance<-c("respect","civility","compromise","balance","understanding",
                 "open","inclusive","tolerance","diversity",
                 "acceptance","accepting")
cat.order<-c("order","civilization","society","civilized")
cat.west<-c("western","world","first-world")
cat.hist<-c("history","forefathers","tradition","founding","fathers","1776")
cat.rule<-c("power","powerful","rule","powers","empowerment")

#Function to convert terms into respective categories#
terms2cats<-function(dtm=mydtm, cats=list()){
  len=c(1:dim(dtm)[2])
  temp<-matrix(NA,ncol=length(cats)+1,nrow=dim(dtm)[1])
  colnames(temp)<-c(names(cats),"misc")
  for (i in 1:length(cats)){
    print(cats[i])
    termlist<-cats[i]
    t<-len[colnames(dtm)%in%termlist[[1]]]
    if(dim(as.matrix(dtm[,t]))[2]==1) {
      temp[,i]<-as.matrix(dtm)[,t]
    }else{
      temp[,i]<-rowSums(as.matrix(dtm)[,t])
      temp[,i]<-ifelse(temp[,i]>0,1,0)
    }
  }
  temp[,i+1]<-rowSums(as.matrix(dtm[,colnames(dtm) %in% paste(unlist(cats))==FALSE]))
  temp
}

#Collapse DTM to TTM#
cats<-list(voting=cat.vote,majority=cat.maj,party=cat.party,rep=cat.rep,usgov=cat.usgov,
            govt=cat.gov,rights=cat.rights,civil=cat.civil,law=cat.legal,
            justice=cat.justice,human=cat.human,
            equal=cat.equal,freedom=cat.free,choice=cat.choice,people=cat.people,
            usa=cat.usa,country=cat.country,west=cat.west,order=cat.order,nation=cat.nation,
            trad=cat.hist,econ=cat.econ,conflict=cat.conflict,rule=cat.rule,
            good=cat.good,fail=cat.fail,voice=cat.voice,
            change=cat.change,community=cat.community,tolerance=cat.tolerance,
            election=cat.election,fair=cat.fair)
cat.dtm<-terms2cats(sub.dtm1,cats)
cat.dtm[,"rights"]<-ifelse(cat.dtm[,"civil"]==1,1,cat.dtm[,"rights"]) #Distinguish different rights
cat.dtm[,"misc"]<-ifelse(cat.dtm[,"misc"]>=1,1,0) #Distinguish misc
cat.ttm<-t(cat.dtm[,])%*%cat.dtm[,]

#Now turning back into individual-level dataframe, save all the data for analysis (survey_analysis.R)#
defdem<-defdem[1:106]
cat.df<-data.frame(cat.dtm)
cat.df$fh<-rowSums(cat.dtm[,c("voting","party","govt","rights")])
cat.df$fh2<-rowSums(cat.dtm[,c("voting","party","govt","civil","law","human")])
defdem<-cbind(defdem,cat.df)
save.image("freeResponseDataCurrent.RData")



##These are the final dtm and ttm with categories pulled from the collocates. 
#The next steps are to turn these into graph objects to explore with networks
#Also, redo this process for dem/rep and male/fem respondents to see if categories change
#or the structures change. 
dim(cat.dtm) #432 respondents, 33 categories
dim(cat.ttm) #33 categories to 33 categories

save(sub.dtm1, sub.ttm, sub.ttm1, temp2, mycorpus, cats, defdem, cat.ttm, cat.dtm, cat.df,
     file="~/Desktop/R Project/projectdata.RData")


library(tm)

##read in original data and then extract the free response to allow matching
study1<-read.csv("Conceptions_of_Democracy.csv",skip=1,stringsAsFactors = FALSE)
names(study1)[12:31]<-paste("word",1:20,sep="")
study2<-read.csv("Conceptions_of_Democracy__with_PartyID.csv",skip=1,stringsAsFactors = FALSE)
names(study2)[12:31]<-paste("word",1:20,sep="")
defdem<-rbind(study1,study2)
defdem<-defdem[defdem$word1!="",]


##cleaning up the free response data
#READ IN THE FILE GENERATED ABOVE HERE#
##this starts over and reads in the spell checked data
temp<-read.csv("Spell Checked and Substituted Free Response Vectors2.csv")
temp$textChecked<-gsub("independencee","independence",temp$textChecked)
temp$textChecked<-gsub("peoplee","people",temp$textChecked)
temp$textChecked<-gsub("governmenternment","government",temp$textChecked)
temp$textChecked<-gsub("governmenter","govern",temp$textChecked)
temp$subbed<-temp$textChecked

##terms to be hyphenated or compounded

proper.nouns <-c("president", "President", "supreme court", "SupremeCourt", "ellis island", "EllisIsland",
                 "civil war", "CivilWar", "magna carte", "MagnaCarta", "abraham lincoln", "AbrahamLincoln", "enron", "Enron", 
                 "iphone", "iPhone", "indians", "Indians", "microsoft", "Microsoft", "f86 sabre", "F86-Sabre",
                 "bill clinton", "BillClinton") 
# proper<-data.frame(matrix(proper.nouns,length(proper.nouns)/2,2))

hyphen<-c("open minded", "open-minded", "non exclusion", "non-exclusion", 
          "potato chips", "potato-chips")
# hyphen<-data.frame(matrix(hyphen,length(hyphen)/2,2))

# subs<-rbind(proper,hyphen)
# names(subs)<-c("Pre","Post")

for (i in seq(1,length(proper.nouns),2)){
  j<-i+1
    temp$subbed<-gsub(proper.nouns[i],proper.nouns[j], temp$subbed)
}

for (i in seq(1,length(hyphen),2)){
j<-i+1
temp$subbed<-gsub(hyphen[i],hyphen[j], temp$subbed)
}

##this part does the quasi "lemmatizing" for most frequent terms
##replacement uses most frequent of terms
temp$subbed<-gsub("vot[a-z]*\\b","voting", temp$subbed)
temp$subbed<-gsub("\\b[a-z]*[[:punct:]]*equal[a-z]*\\b","equality", temp$subbed)
temp$subbed<-gsub("right[s]?\\>","rights", temp$subbed) ##note, this gets rid of righteous
temp$subbed<-gsub("gov[a-z]*\\b", "government", temp$subbed)
temp$subbed <- gsub("self[[:punct:]]government", "government", temp$subbed)
temp$subbed<-gsub("government[[:punct:]]*", "government", temp$subbed)
temp$subbed<-gsub("america[a-z]*\\b","america", temp$subbed)
temp$subbed<-gsub("america[[:punct:]]+","america", temp$subbed)
temp$subbed<-gsub("[s]*elect[a-z]*\\b","election", temp$subbed)
temp$subbed<-gsub("represent[a-z]*\\b", "representation", temp$subbed)
temp$subbed<-gsub("free[^-][a-z]*\\b", "freedom", temp$subbed)
temp$subbed<-gsub("freedom[[:punct:]]+", "freedom", temp$subbed)
temp$subbed <-gsub("\\bfree\\b", "freedom", temp$subbed)
temp$subbed <-gsub("freedommark[a-z]*\\b","freedom market", temp$subbed)
temp$subbed<-gsub("united states", "UnitedStates", temp$subbed)
temp$subbed<-gsub("versus[[:punct:]]+", "versus", temp$subbed)
temp$subbed<-gsub("politic[a-z]*\\b", "politics", temp$subbed)
temp$subbed<-gsub("leaders[a-z]*\\b", "leader", temp$subbed)
temp$subbed<-gsub("president[a-z]*\\b", "president", temp$subbed,ignore.case=TRUE)
temp$subbed<-gsub("citizen[a-z]*\\b|citizen[']+s\\b", "citizen", temp$subbed,ignore.case=TRUE)
temp$subbed<-gsub("rule[s]*\\b", "rule", temp$subbed,ignore.case=TRUE)
temp$subbed<-gsub("participat[a-z]*\\b", "participation", temp$subbed,ignore.case=TRUE)
temp$subbed<-gsub("\\bpart[iy][^c][a-z]*", "party", temp$subbed,ignore.case=TRUE)
temp$subbed<-gsub("multi[[:punct:]]party", "party", temp$subbed)
temp$subbed<-gsub("corrupt[a-z]*\\b", "corrupt", temp$subbed)
temp$subbed<-gsub("\\begal[a-z]*\\b", "egalitarian", temp$subbed)
temp$subbed<-gsub("\\b[un]*fair[a-z]*[[:punct:]]*\\b", "fair", temp$subbed)
temp$subbed<-gsub("\\blaw[a-z]*\\b", "laws", temp$subbed)
temp$subbed<-gsub("\\bjust\\b", "justice", temp$subbed)
temp$subbed<-gsub("people[[:punct:]]ruled", "people rules", temp$subbed)
temp$subbed<-gsub("u[[:punct:]]s[[:punct:]]", "usa", temp$subbed)
temp$subbed<-gsub("choices","choice",temp$subbed)
temp$subbed<-gsub("choose","choice",temp$subbed)
temp$subbed<-gsub("voice[[:punct:]]*[s]*", "voice", temp$subbed)
temp$subbed<-gsub("everyone[[:punct:]]*", "everyone", temp$subbed)
temp$subbed<-gsub("everybody", "everyone", temp$subbed)
temp$subbed<-gsub("group[s]*", "group", temp$subbed)
temp$subbed<-gsub("voice[a-z]*", "voice", temp$subbed)
temp$subbed<-gsub("opinion[s]*", "opinion", temp$subbed)
temp$subbed<-gsub("debate[s]*", "debate", temp$subbed)
temp$subbed<-gsub("help[a-z]*\\b", "help", temp$subbed)
temp$subbed <-gsub("\\bflaw[a-z]*\\b", "flawed", temp$subbed)
temp$subbed <-gsub("\\bconservative[s]*\\b", "conservatives", temp$subbed)
temp$subbed <-gsub("\\btogether[a-z]*\\b", "together", temp$subbed)
temp$subbed <-gsub("\\bargu[a-z]*\\b", "argue", temp$subbed)
temp$subbed <-gsub("\\bunite[d]?\\b", "unity", temp$subbed)
temp$subbed <-gsub("people, people", "people", temp$subbed)
temp$subbed <-gsub("\\bpeople[[:punct:]]?[s]?\\b", "people", temp$subbed)
temp$subbed <-gsub("\\bpresidency\\b", "president", temp$subbed)
temp$subbed <-gsub("\\bdebat[a-z]*\\b", "debate", temp$subbed)
temp$subbed <-gsub("\\barm[s]*\\b","arms", temp$subbed)
temp$subbed <-gsub("\\bgun[s]*\\b", "guns", temp$subbed)
temp$subbed <-gsub("\\human[a-z]*\\b", "human", temp$subbed)

m <- list(ID = "ID", content = "subbed")
myReader <- readTabular(mapping = m)
mycorpus <- Corpus(DataframeSource(temp[,c(2,5)]), readerControl = list(reader = myReader))
sub.dtm<-DocumentTermMatrix(mycorpus)
sub.ttm<-t(as.matrix(sub.dtm))%*%as.matrix(sub.dtm)

##now look at how many poeople made at least one mention of concept
sub.dtm1<-as.matrix(sub.dtm)
sub.dtm1<-ifelse(sub.dtm1>0,1,0)
sub.ttm1<-t(as.matrix(sub.dtm1))%*%as.matrix(sub.dtm1)
sort(diag(sub.ttm1))


##this step will go through and stem the terms manually
## expand for high frequency words

lem.vot <- grep("vot",colnames(sub.ttm), value=TRUE)
lem.equal <- grep("equal",colnames(sub.ttm), value=TRUE)
lem.right <- grep("right[s]?\\>",colnames(sub.ttm), value=TRUE) ##note, this gets rid of righteous
lem.gov <- grep("gov",colnames(sub.ttm), value=TRUE)
lem.usa <- grep("america",colnames(sub.ttm), value=TRUE)
lem.elect <- grep("elect",colnames(sub.ttm), value=TRUE)
lem.rep <- grep("represent", colnames(sub.ttm), value=TRUE)
lem.free <-grep("free[^-]", colnames(sub.ttm), value=TRUE)

lem.vot
lem.equal
lem.right
lem.gov
lem.usa
lem.elect
lem.rep
lem.free

find.coloc<-function(ttm,term , low=0.2){
  l<-low*ttm[term,term]
  sort(ttm[,term][ttm[,term]>l])
}

find.coloc(sub.ttm,"flag")

find.colocs<-function(ttm=ttm,termlist, low=0.1){
  len=dim(ttm)[1]
  t<-len[colnames(ttm)%in%termlist]
  total<-sum(diag(ttm)[t])
  sort(colSums(ttm[t,-t])[colSums(ttm[t,-t])>low*total])
}

find.colocs(sub.ttm,c("flag","voting"))

############################################################
##create categories for each concept
##these first three correspond to freedom house political rights
##voting, pluralism, functioning of govt

##voting and elections
cat.vote<-c("voting",  "ballot", "participation", "suffrage")
cat.election<-c("election","polls","campaign")
cat.maj<-c("majority","plurality","majorities","mob")

##people - sometimes "power of people", sometimes closer to community
cat.people<-c("people", "popular", "public", "citizen")

##partisan pluralism
cat.party<-c("politics", "party", "democrat", "republican", "conservative", "left", "versus")

##functioning of representative government
cat.rep<-c("representation", "republic", "direct","federal")
cat.gov<-c("government")
cat.usgov<-c("senate", "house", "president", "congress", "checks", "balances", "amendments", "veto", "states")
#cat.gov.perf<-c("transparent", "corrupt", "slow","gridlock","gerrymandering","accountability")

"system"
"leader"
"officials"

##
#cat.names<-c("obama", "billclinton", "carter")

##the next few correspond to freedom house civil liberties

##rule of law
cat.legal<-c("constitution","laws", "supremecourt", "courts", "court", "process", "trial","bill", "legal", "illegal")
cat.justice<-c("justice","unjust")

##rights
cat.rights<-c("rights")

##human rights is not in freedom house
##human rights
cat.human<-c("human")

##civil rights
cat.civil<-c("civil", "speech", "minority", "religion", "religious", "church", "arms", "guns", "press","expression",
             "amendement","bill")

##more generic terms - non-freedom house
##liberty appears 5 times without freedom, not independence
cat.free<-c("freedom", "liberty", "independence")

##equality
cat.equal<-c("equality", "egalitarian")
cat.fair<-c("fair")

##choice - visual inspect seems to be linked to freedom and people/voice, rarely elections
grep("choice",free[,4])


##America
cat.usa<-c("america", "unitedstates", "usa", "flag", "founding", "fathers", "washington", "declaration", "red", "1776", "land", "statue", "ellisisland")

##show that the flag is really USA
free[c(30,136,250,277,342,392),2:21]

##greeks
cat.country<-c("athens", "greece", "greek", "greeks", "rocks", "india", "china", "french")

cat.voice<-c("voice", "say", "opinion", "argue", "speaking", "debate", "disagreement", "speech", "expression")
cat.choice<-c("choice", "options", "will")

##economic- not sure about opportunity - often tied to equality
cat.econ<-c("capitalism","money","opportunity","class", "market", "communism", "welfare", "money", "working", "handouts", "tax", "debt", "corporate","prosperous", "monopolies",
            "prosperous","anti-corporatist", "succeed","job")

##power - 15 people, not sure
cat.good<-c("good","hope","happiness","ideal", "pride", "great", "happy", "helpful", "helping", "honest", "love", "supportive", "kind", "kindness", "goodness")

cat.fail<-c("corrupt", "lie", "lies", "war", "wars", "alienating", "chaos", "mob", "shady", "cliquish", "dishonest", "slow", "mean", "flawed", "abuse")

##other words that are not in a category yet but are used >2 times
## haven't done those used 1/2 times

cat.change<-c("modern","progress", "change", "progressive", "old", "young", "growth")

cat.community<-c("unity", "community", "together", "collaboration", "concensus", "everyone", "family", "group") 

#cat.nation<-c("nation", "patriotic", "flag")
cat.conflict<-c("peace", "peaceful", "violence", "civil war", "revolution", "calmer society")
cat.tolerance<-c("respect", "civility", "compromise", "balance", "understanding", "open", "inclusive", "tolerance", "diversity", 
                 "acceptance","accepting")

cat.order<-c("order", "civilization", "society", "civilized")
cat.west<-c("western", "world", "first-world")
#also french and usa
cat.hist<-c("history","forefathers", "tradition", "founding", "fathers", "1776")


cat.rule<-c("power","powerful", "rule", "powers", "empowerment")

#TEST CERTAIN TERMS' FIT WITHIN THEIR COMPONENT###
#set component in question#
catTemp<-cat.human
dfName<-"Human Colocates.csv"

catTestDf<-data.frame(matrix(NA,length(catTemp),2))
names(catTestDf)<-c("term","sim")
catTestDf$term<-catTemp

for(i in 1:nrow(catTestDf)){
  mattt<-data.frame(matrix(NA,1,3))
  names(mattt)<-c("term","coloc","count")
  term<-catTemp[i]
  q<-length(catTemp)
  for(z in 1:q){
    r<-find.coloc(sub.ttm1,catTemp[z])
    n<-length(r)
    temp<-data.frame(term=rep(catTemp[z],n),
                     coloc=names(r),
                     count=r)
    row.names(temp)<-NULL
    mattt<-rbind(mattt,temp)
  }
  mattt<-mattt[2:nrow(mattt),]
  mattt.test<-mattt[mattt$term==term,]
  mattt.rest<-mattt[mattt$term!=term,]
  catTestDf[match(term,catTestDf$term),2]<-sum(mattt.test$coloc %in% unique(mattt.rest$coloc))/nrow(mattt.test)
}

write.csv(catTestDf[order(-catTestDf$sim),],dfName)


#set term to be tested#
term<-cat.community[2]

# catTemp<-gsub("checks and balances","balances",catTemp)
q<-length(catTemp)

for(i in 1:q){
  r<-find.coloc(sub.ttm1,catTemp[i])
  n<-length(r)
  temp<-data.frame(term=rep(catTemp[i],n),
                   coloc=names(r),
                   count=r)
  row.names(temp)<-NULL
  mattt<-rbind(mattt,temp)
}
mattt<-mattt[2:nrow(mattt),]

mattt.test<-mattt[mattt$term==term,]
mattt.rest<-mattt[mattt$term!=term,]

sum(mattt.test$coloc %in% unique(mattt.rest$coloc))/nrow(mattt.test)

r1<-names(find.coloc(sub.ttm,"representation"))
r2<-names(find.coloc(sub.ttm,"representative"))
r3<-names(find.coloc(sub.ttm,"representatives"))
r<-unique(r1,r2)
r<-unique(r,r3)

g<-names(find.coloc(sub.ttm,"government"))

quick<-c("government","senate", "house", "officials", "president", "presidency", "congress", "balances", "amendments", "veto")
not<-cat.rep[!cat.rep%in%quick]

testt<-as.matrix(sub.dtm[,c(1:980)[colnames(sub.dtm)%in%quick]])
sum(ifelse(rowSums(testt)>0,1,0))

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

##I am going to collapse in the dtm
##then recreate a collapsed ttm

cats<- list(voting=cat.vote, majority=cat.maj, party=cat.party, rep=cat.rep, usgov=cat.usgov,
            govt=cat.gov, rights=cat.rights, civil=cat.civil, law=cat.legal, 
            justice=cat.justice, human=cat.human,
            equal=cat.equal, freedom=cat.free, choice=cat.choice, people=cat.people, 
            usa=cat.usa,country=cat.country, west=cat.west, order=cat.order, nation=cat.nation, trad=cat.hist,
            econ=cat.econ, conflict=cat.conflict, rule=cat.rule,
            good=cat.good, fail=cat.fail, voice=cat.voice,
            change=cat.change, community=cat.community, tolerance=cat.tolerance, 
            election=cat.election, fair=cat.fair)

cat.dtm<-terms2cats(sub.dtm1,cats)
cat.dtm[,"rights"]<-ifelse(cat.dtm[,"civil"]==1,1,cat.dtm[,"rights"])
cat.dtm[,"misc"]<-ifelse(cat.dtm[,"misc"]>=1,1,0)
cat.ttm<-t(cat.dtm[,])%*%cat.dtm[,]

##first word only
first.word<-sapply(strsplit(temp$subbed, " "),"[[",1)
first.word<-data.frame(ID=free[,1],subbed=first.word)
corpus1 <- Corpus(DataframeSource(first.word), readerControl = list(reader = myReader))
dtm1<-DocumentTermMatrix(corpus1)
cat.dtm1<-terms2cats(dtm1,cats)
cat.dtm1[,31]<-ifelse(rowSums(cat.dtm1[,1:30])>0,0,1)
cat.dtm1<- colSums(cat.dtm1)
sort(cat.dtm1)
print(sort(cat.dtm1)/432,digits=2)

##1-3 word only
three.word<-paste(sapply(strsplit(temp$subbed, " "),"[[",1), sapply(strsplit(temp$subbed, " "),"[[",2), 
            sapply(strsplit(temp$subbed, " "),"[[",3), " ")
three.word<-data.frame(ID=free[,1],subbed=three.word)
corpus3 <- Corpus(DataframeSource(temp), readerControl = list(reader = myReader))
dtm3<-DocumentTermMatrix(corpus3)
cat.dtm3<-terms2cats(dtm3,cats)
cat.dtm3[,31]<-ifelse(rowSums(cat.dtm3[,1:30])>0,0,1)
cat.dtm3<-ifelse(cat.dtm3>0,1,0)
cat.dtm3<- colSums(cat.dtm3)
sort(cat.dtm3)
print(sort(cat.dtm3)/432,digits=2)

##compre first response only to all respones
sort(cat.dtm1)

##can now factor individual "types" of answers
factanal(cat.dtm,5)

# ##these 
# free$vote<-rapply(apply(free,1,function(x) grep(paste(cat.vote,collapse="|"), x, value=TRUE)),length)
# 
# free$plural<-rapply(apply(free,1,function(x) grep(paste(cat.party,collapse="|"), x, value=TRUE)),length)
# 
# free$rep<-rapply(apply(free,1,function(x) grep(paste(cat.party,collapse="|"), x, value=TRUE)),length)
# 
# free$right<-rapply(apply(free,1,function(x) grep(paste(cat.right,collapse="|"), x, value=TRUE, ignore.case=TRUE)),length)
# 
# free$civil<-rapply(apply(free,1,function(x) grep(paste(cat.civil,collapse="|"), x, value=TRUE, ignore.case=TRUE)),length)
# 
# free$legal<-rapply(apply(free,1,function(x) grep(paste(cat.legal,collapse="|"), x, value=TRUE, ignore.case=TRUE)),length)
# 
# free$human<-rapply(apply(free,1,function(x) grep(paste(cat.human,collapse="|"), x, value=TRUE, ignore.case=TRUE)),length)
# 
# free$free<-rapply(apply(free,1,function(x) grep(paste(cat.free,collapse="|"), x, value=TRUE)),length)
# free$equal<-rapply(apply(free,1,function(x) grep(paste(cat.equal,collapse="|"), x, value=TRUE)),length)
# 
# free$choice<-rapply(apply(free,1,function(x) grep(paste(cat.choice,collapse="|"), x, value=TRUE, ignore.case=TRUE)),length)
# free$fail<-rapply(apply(free,1,function(x) grep(paste(cat.fail,collapse="|"), x, value=TRUE, ignore.case=TRUE)),length)
# 
# free$usa<-rapply(apply(free,1,function(x) grep(paste(cat.usa,collapse="|"), x, value=TRUE, ignore.case=TRUE)),length)
# 
# free$greek<-rapply(apply(free,1,function(x) grep(paste(cat.greek,collapse="|"), x, value=TRUE, ignore.case=TRUE)),length)
# free$choice<-rapply(apply(free,1,function(x) grep(paste(cat.choice,collapse="|"), x, value=TRUE, ignore.case=TRUE)),length)
# 
# free$power<-rapply(apply(free,1,function(x) grep("power", x, value=TRUE, ignore.case=TRUE)),length)
# 
# free$good<-rapply(apply(free,1,function(x) grep(paste(cat.good,collapse="|"), x, value=TRUE, ignore.case=TRUE)),length)
# 
# free$total<-20-apply(apply(free[,2:21],1,is.na),2,sum)

##now turning back into ind level dataframe
defdem<-defdem[1:106]
cat.df<-data.frame(cat.dtm)
cat.df$fh<-rowSums(cat.dtm[,c("voting","party","govt","rights")])
cat.df$fh2<-rowSums(cat.dtm[,c("voting","party","govt","civil","law","human")])
defdem<-cbind(defdem,cat.df)

save.image("~/Dropbox/Democracy Meaning/Survey Re-Analysis/freeResponseData2.RData")

##look at how people map onto space
sub.ddm<-(as.matrix(sub.dtm))%*%t(as.matrix(sub.dtm))
net.words<- graph_from_adjacency_matrix(as.matrix(sub.ddm), mode="directed", weighted=TRUE)
V(net.words)$male<-defdem$gender==1
V(net.words)$white<-defdem$race==6
V(net.words)$partyid<-defdem$party7
V(net.words)$college<-defdem$college
ggnet2( induced_subgraph(net.words, c(1:432)[is.na(V(net.words)$partyid)==FALSE]), 
        node.color=brewer.pal(7,"RdBu")[V(net.words)$partyid[is.na(V(net.words)$partyid)==FALSE]])
ggnet2( induced_subgraph(net.words, c(1:432)[is.na(V(net.words)$partyid)==FALSE]), 
        node.color=c("red","red","red","white","blue","blue","blue")[V(net.words)$partyid[is.na(V(net.words)$partyid)==FALSE]])

##this is t-test for difference
t.test(centralization.betweenness(net.words)$res~V(net.words)$male)
t.test(centralization.closeness(net.words)$res~V(net.words)$male)

##women have higher betweenness centrality based on words?, maybe because equality?

t.test(centralization.betweenness(net.words)$res~V(net.words)$college)
##not college affect between centrality

t.test(centralization.betweenness(net.words)$res~defdem$age==1)
t.test(centralization.betweenness(net.words)$res~V(net.words)$partyid>3)


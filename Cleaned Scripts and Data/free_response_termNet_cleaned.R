#Working directory, library, and data#
setwd("C:/Users/Tim/Dropbox/Democracy Meaning/Survey Re-Analysis/Cleaned Scripts and Data/")
library(igraph)
library(sna)
library(ggplot2)
library(GGally)
load("projectdata3.RData")
dim(defdem)

#Looking for patterns on co belief/shared meaning of words/terms#
#More correlations seem to appear between peripheral/peripheral or core/peripheral concepts#
#Print all t.test results between closed (exclude) and open ended themes#
for (i in 26:42){
  for (j in 107:138){
    print(paste(colnames(defdem)[i],colnames(defdem)[j],
                signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2),sep=", "))
  }
}
#Print all significant t.test results between closed (exclude) and open ended themes#
for (i in 26:42){
  for (j in 107:138){
    if(t.test(defdem[,i]~ defdem[,j])$p.value<0.06){
      print(paste(colnames(defdem)[i],colnames(defdem)[j],
                  signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2),sep=", "))
    }
  }
}
#Print all t.test results between closed (fundamental) and open ended themes#
for (i in seq(43,76,2)){
  for (j in 107:138){
    print(paste(colnames(defdem)[i],colnames(defdem)[j],
                signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2),sep=", "))
  }
}
#Print all significant t.test results between closed (fundamental) and open ended themes#
for (i in seq(43,76,2)){
  for (j in 107:138){
    if(t.test(defdem[,i]~ defdem[,j])$p.value<0.06){
      print(paste(colnames(defdem)[i],colnames(defdem)[j],
                  signif(t.test(defdem[,i]~ defdem[,j])$p.value, digits=2),sep=", "))
    }
  }
}

#Pairwise coocurrance - turn significant co-occurances into graph (tie) format#
term.list<- make_empty_graph(n=35)
V(term.list)$name<-colnames(cat.dtm)[1:35]
V(term.list)$mentions<-colSums(cat.dtm[,1:35])
#Assign colors based on type#
type.inst<-c("voting","elections","government","rights","laws", "power", "history", "structure", "us history")
type.value<-c("choice", "justice","equality","fair","freedom","choice","voice","liberty", "ideals", "patriotic symbols","pos qualities", "history")
type.group<-c("people","community","usa","personification")
type.negative<-c("negative politics")
type.politics<-c("economics", "other gov", "partisanship",  "politicians", "politics", "representation", "other gov")
   
V(term.list)$color<-ifelse(V(term.list)$name%in%type.inst,1,6)
V(term.list)$color<-ifelse(V(term.list)$name%in%type.value,2,V(term.list)$color)
V(term.list)$color<-ifelse(V(term.list)$name%in%type.group,3,V(term.list)$color)
V(term.list)$color<-ifelse(V(term.list)$name%in%type.negative,4,V(term.list)$color)
V(term.list)$color<-ifelse(V(term.list)$name%in%type.politics,5,V(term.list)$color)
V(term.list)$label<-c("5050", "Choice","Community", "Economics", "Elections", "Equality", "exist..", "Fair", "Freedom",
              "Government", "Greece", "heard", "History", "Ideals", "Justice", "Laws", "Liberty",
              "Negative Politics", "Other Gov", "Partisanship", "Patriotic Symbols", "People", "Personification",
              "Politicians", "Politics", "Positive Qualities", "Power", "products.", "Representation", "Rights",
              "Structure", "US History", "USA", "Voice", "Voting")
#V(term.list)$groupr<-c("Institutions","Values","Groups", "Misc.")[V(term.list)$color]
#Pair-wise t.tests, add to list with significance#
##cat.dtm in place of defdem
cats<-colnames(cat.dtm)[colSums(cat.dtm)>1]
for (i in cats) {
  for (j in cats){
    if(i!=j){
    w <- t.test(cat.dtm[,i] ~ cat.dtm[,j])$statistic
    p<-t.test(cat.dtm[,i] ~ cat.dtm[,j])$p.value
    term.list <- add_edges(term.list,c(i,j), weight=w,pvalue=p)
    }
  }
}
#Different subgraphs based on positive or negative relationships#
term.neg <- subgraph.edges(term.list, which(igraph::E(term.list)$pvalue <0.05 & E(term.list)$weight>0))
#term.pos <- subgraph.edges(term.list,c(1:1600)[E(term.list)$pvalue <0.05 & E(term.list)$weight<0])
#term.pos<-subgraph.edges(term.list, eid=weight)[E(term.list)$pvalue <0.05 & E(term.list)$weight<0]
term.pos<-igraph::subgraph.edges(term.list,which(igraph::E(term.list)$pvalue <0.05 & E(term.list)$weight<0))
E(term.pos)$weight<-abs(E(term.pos)$weight)
#Remove specific vertices from network#
term.pos<- delete_vertices(term.pos,c(1:35)[V(term.pos)$name%in%c("5050","exist.","products/consume", "heard.", "exist.")])

#Get adjacency matrices#
pos<-as.matrix(as_adjacency_matrix(term.pos))
neg<-as.matrix(as_adjacency_matrix(term.neg))
all<-as.matrix(as_adjacency_matrix(ugh))


#Plot it out with interactive node placement#
ggplot(pos, arrowhead.cex = 0, vertex.cex = sqrt(V(pos)$mentions)/5, label.cex=1.2,
      vertex.col = c("tomato","skyblue","lightgreen","lightgray")[V(pos)$color],
      vertex.border = "transparent", label =V(pos)$name, label.pos=5, edge.lwd =sqrt(E(pos)$weight)/3,
      interactive=TRUE)
#Plot this network to an image#

library(GGally)

png("PositiveTerm.png",width=8,height=6, units="in", res=300)
ggnet2(term.pos,node.size=V(term.pos)$mentions,node.label=V(term.pos)$label,
       label.size=log(V(term.pos)$mentions),legend.position="bottom",
       max_size=25,color=V(term.pos)$color,palette="Set1", alpha = 0.5) +
guides(size=FALSE)
scale_color_brewer("", palette = "Set1",
                   guide = guide_legend(override.aes = list(size = 6)))
dev.off()


png("NegativeTermNetwork.png",width=8,height=6, units="in", res=300)
ggnet2(neg.simp,node.size=V(term.neg)$mentions,node.label=V(neg.simp)$label,
       label.size=log(V(neg.simp)$mentions),legend.position="bottom",
       max_size=25,color=V(neg.simp)$color,palette="Set1", alpha = 0.5) +
  guides(size=FALSE)
scale_color_brewer("", palette = "Set1",
                   guide = guide_legend(override.aes = list(size = 6)))
dev.off()


###Stopped cleaning here, migrated relevant script above to cleaned analysis script - rest is as it was###

par(mar=c(0,1,0,0))
plot(term.list, edge.arrow.size=.1)
plot(term.neg, edge.arrow.size=.1, main="Negative probabilities of volunteering both terms", vertex.size=V(term.neg)$mentions/10)
V(term.pos)$name2<-V(term.pos)$name
V(term.pos)$name<-c("Voting","Majority","Party","Representation","U.S. Gov't","Gov't",
                    "Rights","Civil Rights","Law","Justice","Human Rights","Equality",
                    "Choice","People","U.S.A.","Country","Nation","Economy","Rule",
                    "Fail","Voice","Change","Community","Tolerance","Election","Fair")
V(term.pos)$color<-c("sky","sky","sky","sky","green","sky","sky","sky",)
plot(term.pos, edge.arrow.size=.1,main="Positive probabilities of Volunteering Both Terms", vertex.size=V(term.pos)$mentions/10)

#size according to degree/frequency mention

library(concoR)
V(term.list)$block<-concor_hca(list(positive=pos,negative=neg),p=2)$block
#V(term.list)$color<-c("red","blue","white","grey")[V(term.list)$block]
plot(blockmodel(neg,V(term.list)$block,plabels=V(term.list)$name), cex.label=.5, 
      main="Blockmodel with negative ties")
plot(blockmodel(pos,V(term.list)$block,plabels=V(term.list)$name), cex.label=.5, 
     main="Blockmodel with positive ties")


##look for pairwise correlations between issues
# temp<-cor(defdem[,102:129])
# temp<-ifelse(temp>.15,temp, NA)
# print(temp, digits=2, na.print="")
# 
# install.packages("LSAfun")
# library(LSAfun)
# 
# terms<-terms/diag(terms)
# temp.ham<-equiv.clust(terms,method="euclidean")

##pairwise coocurrance - turn significant co-occurances into graph (tie) format
##fundamental questions
fund.list<- make_empty_graph(n=17)
V(fund.list)$name<- substring(colnames(defdem)[seq(43,76,2)],5)
V(fund.list)$mentions<-colSums(defdem[,seq(43,76,2)])
fund.pos<-fund.list
fund.neg<-fund.list

for (i in seq(43,76,2)){
  for (j in seq(43,76,2)){
    if(i!=j){
      if(t.test(defdem[,i]~ defdem[,j])$p.value<.05){
        if(t.test(defdem[,i]~ defdem[,j])$statistic > 0){
          fund.list <- add_edges(fund.list,c((i-41)/2,(j-41)/2))
          fund.neg <- add_edges(fund.neg,c((i-41)/2,(j-41)/2))
        }else{
          fund.list <- add_edges(fund.list,c((i-41)/2,(j-41)/2))
          fund.pos <- add_edges(fund.pos,c((i-41)/2,(j-41)/2))
        }
      }}
  }
}

plot(fund.neg,edge.arrow.size=.1,vertex.size=V(fund.neg)$mentions/10, main="Negative probabilities of volunteering both terms")

V(fund.pos)$name2<-V(fund.pos)$name
V(fund.pos)$name<-c("")

plot(fund.pos,edge.arrow.size=.1,vertex.size=V(fund.pos)$mentions/10, main="Positive probabilities of volunteering both terms")

##pairwise coocurrance - turn significant co-occurances into graph (tie) format
##not fundamental questions
not.list<- make_empty_graph(n=17)
V(not.list)$name<- substring(colnames(defdem)[c(26:42)],4)
V(not.list)$mentions<-colSums(defdem[,c(26:42)])
not.pos<-not.list
not.neg<-not.list

for (i in 26:42){
  for (j in 26:42){
    if(i!=j){
      if(t.test(defdem[,i]~ defdem[,j])$p.value<.05){
        if(t.test(defdem[,i]~ defdem[,j])$statistic > 0){
          not.list <- add_edges(not.list,c(i-25,j-25))
          not.neg <- add_edges(not.neg,c(i-25,j-25))
        }else{
          not.list <- add_edges(not.list,c(i-25,j-25))
          not.pos <- add_edges(not.pos,c(i-25,j-25))
        }
      }
    }
  }
}

plot(not.neg,edge.arrow.size=.1,vertex.size=V(not.neg)$mentions/10, main="Negative probabilities of volunteering both terms")
plot(not.pos,edge.arrow.size=.1,vertex.size=V(not.pos)$mentions/10, main="Positive probabilities of volunteering both terms")


##pairwise coocurrance - turn significant co-occurances into graph (tie) format
##all questions
all.list<- make_empty_graph(n=62)
vars<-c(26:42,seq(43,76,2),102:129)
V(all.list)$name<- colnames(defdem)[vars]
V(all.list)$mentions<-colSums(defdem[,vars])
all.pos<-all.list
all.neg<-all.list

for (i in vars){
  for (j in vars){
    if(i!=j){
      if(t.test(defdem[,i]~ defdem[,j])$p.value<.05){
        if(t.test(defdem[,i]~ defdem[,j])$statistic > 0){
          all.list <- add_edges(all.list,c(match(i,vars),match(j,vars)))
          all.neg <- add_edges(all.neg,c(match(i,vars),match(j,vars)))
        }else{
          all.list <- add_edges(all.list,c(match(i,vars),match(j,vars)))
          all.pos <- add_edges(all.pos,c(match(i,vars),match(j,vars)))
        }
      }}
  }
}

plot(induced_subgraph(all.pos,c(18:62)),vertex.size=V(all.pos)$mentions[18:62]/10,
     edge.arrow.size=.1,main="Positive probabilities of volunteering both terms",vertex.label.cex=.8)


##check the association between words

for(i in 1:25){
  print(paste("i:", length(grep("vot",defdem[,i])), length(grep('election',defdem[grep("vot",defdem[,i]),6:25]))))
}

##analysis of content & "coreness" in contestation
##look to see most popular terms (multiple mentions)
sort(diag(sub.ttm))

##does election belong with voting
find.coloc(sub.ttm1, "election", low=.1)
find.coloc(sub.ttm1, "voting", low=.1)

##check for representation
find.coloc(sub.ttm1, "representation", low=.1)


##this function is used to check for overlap, and also co-locates of minor terms in component
check.key<-function(keyword, cat.name, cat.vec){
  print(diag(sub.ttm1)[keyword])
  print(diag(cat.ttm)[cat.name])
  print(diag(sub.ttm1)[keyword]/diag(cat.ttm)[cat.name])
  for(i in 1:length(cat.vec)){
  print(cat.vec[i])
  print(find.coloc(sub.ttm1,cat.vec[i], low=.1))
  ##to check order of terms in component
  }
  print(sort(diag(sub.ttm1)[grep(paste(cat.vec, collapse="|"), colnames(sub.ttm1))]))
}

##check the key word as a proportion of key word categories
check.key("voting", "voting", cat.vote)
table(sub.dtm1[,"voting"],sub.dtm1[,"election"])

check.key("people", "people", cat.people)
check.key("president","usgov",cat.usgov)
check.key("politics","plural",cat.plural) ##just short of 80%, but still strong
check.key("equality","equal",cat.equal) ##equality and fair are synonyms - se for most part - not 80%
check.key("fair","equal",cat.equal) 
check.key("freedom", "free", cat.free)
check.key("justice", "justice", cat.justice)

check.key("rule","rule",cat.rule)
check.key("power","rule",cat.rule)

check.key("rights", "rights", cat.rights)
check.key("human", "human", cat.human)
check.key("civil", "civil", cat.civil)

check.key("constitution","law",cat.legal)
check.key("laws", "law", cat.legal)

check.key("choice","choice",cat.choice)

##usa, america and unitedstates are synonyms, so...
table(rowSums(sub.dtm1[,c("america","usa","unitedstates")]))
sum(table(rowSums(sub.dtm1[,c("america","usa","unitedstates")]))[2:3])/diag(cat.ttm)["usa"] ##93%
temp$subbed[cat.dtm[,"usa"]==1&rowSums(sub.dtm1[,c("america","usa","unitedstates")])<1] ##check other 7% - fine


table(rowSums(cat.dtm[,type.value]))




#Working directory and library#
setwd("~/Google Drive/Text Mental Models/")
library(tm)
library(tidyverse)
library(igraph)

setwd("~/Google Drive/Text Mental Models/CliquePercolationMethod-R/")
source("clique.community.opt.par.R")
source("clique.community.opt.R")
source("clique.community.R")
setwd("~/Google Drive/Text Mental Models/")

giant.component <- function(graph) {
  cl <- igraph::clusters(graph)
  igraph::induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}

symMat<-function(mat) {
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
  mat
}

dichotMat<-function(mat,t){
  mydim <- dim(mat)
  dn<-dimnames(mat)
  mat[mat>=t] <- 1
  mat[mat<t] <- 0
  dim(mat) <- mydim
  dimnames(mat)<-dn
  mat
}

find.coloc<-function(ttm,term,low=0.2){ #Function to find colocates by term in a ttm
  l<-low*ttm[term,term] #Given a lower threshold 
  sort(ttm[,term][ttm[,term]>l])
}

findLHS<-function(qcorpus,keyword,w=5){
  sort(table(unlist(strsplit(quanteda::kwic(qcorpus,keyword,w)$pre,
                             split=" "))))
}

findRLhi<-function(qcorpus,keyword,w=5,hi=.25){
  colocLHS<-findLHS(qcorpus,keyword,w)
  colocRHS<-findRHS(qcorpus,keyword,w)
  colocs<-c(colocLHS,colocRHS)
  keyn<-quanteda::docfreq(quanteda::dfm(qcorpus))[keyword]
  temp<-colocs/keyn
  temp[temp>hi]
}

findLHShi<-function(qcorpus,keyword,w=5,hi=.25){
  colocLHS<-findLHS(qcorpus,keyword,w=w)
  keyn<-quanteda::docfreq(quanteda::dfm(qcorpus))[keyword]
  temp<-colocLHS/keyn
  temp[temp>hi]
}

findRHS<-function(qcorpus,keyword,w=5){
  sort(table(unlist(strsplit(quanteda::kwic(qcorpus,keyword,w)$post,
                             split=" "))))
}

find.coloc.dir<-function(ttm.bin,c=.8){
  colocs<-lapply(seq_len(ncol(ttm.bin)), 
       function(i) rownames(ttm.bin)[ttm.bin[,i]>c])
  len<-rapply(colocs,function(i) length(i))
  return(colocs[len>1])
    #colnames(ttm.bin)[i],sep="."),colocs[[i]]
  #}
}

create.cat<-function(keyword,assoc){
  assign(paste("cat",keyword,assoc,envir = .GlobalEnv))
}

removeSparse<-function(ttm,p=.01, n=432){
  ttm[order(diag(ttm), decreasing=TRUE),order(diag(ttm),decreasing=TRUE)]
  np=p*n
  ttm[diag(ttm)>np,diag(ttm)>np]
}

synEdges<-function(termlist){
  edges<-NULL
  for(i in termlist){
    syns<-termlist[termlist%in%unlist(qdap::syn(c(i)))]
    if(length(syns)>0){
      temp<-matrix(NA,nrow=length(syns),ncol=2)
      temp[,1]<-i
      temp[,2]<-syns
    }else{
      temp<-c(i,"None")
    }
  edges<-rbind(edges,temp)
  }
  return(edges)
}

findPhrases<-function(qcorpus,terms=2,min_count=4){
  phrases<-quanteda::textstat_collocations(qcorpus, method="lambda",
                 size=terms, min_count=min_count)
  phrases%>%
    arrange(desc(count))%>%
    select(collocation,count)
}

nclan <- function(g,n){
  g <- igraph::as.undirected(g)
  igraph::E(g)$weight <- 1 #just in case g has weights - does not modify original graph
  ncliques <- RBGL::kCliques(graph::ugraph(igraph::igraph.to.graphNEL(g))) #get cliques
  n.cand <- ncliques[[n]] #n-clique candidates to be an n-clan
  n.clan <- list() #initializes a list to store the n-clans
  n.clan.i <- 1 #initializes a list pointer
  for (n.cand.i in 1:length(n.cand)){ #loop over all of the candidates
    g.n.cand <- igraph::induced_subgraph(g,n.cand[[n.cand.i]]) #get the subgraph
    if (igraph::diameter(g.n.cand)<=n){ #check diameter of the subgraph
      n.clan[[n.clan.i]] <- n.cand[[n.cand.i]] #add n-clan to the list
      n.clan.i <- n.clan.i+1 #increment list pointer
    }
  }
  return(n.clan) #return the entire list
}

kplex <- function(g,k,m){
  g.sym <- igraph::as.undirected(g) #to make sure that degree functions properly
  g.sym.degmk <- igraph::induced_subgraph(g.sym,igraph::degree(g.sym)>=(m-k)) #makes algorithm faster
  k.cand <- combn(igraph::V(g.sym.degmk)$name,m) #all candidate combinations with m members
  k.plex <- list() #initializes a list to store the k-plexes
  k.plex.i <- 1 #initializes a list pointer
  for (k.cand.i in 1:dim(k.cand)[2]){ #loop over all of the columns
    g.k.cand <- igraph::induced_subgraph(g.sym.degmk,k.cand[,k.cand.i]) #get the subgraph
    if (min(igraph::degree(g.k.cand))>=(m-k)){ #if minimum degree of sugraph is > m=k, k-plex!
      k.plex[[k.plex.i]] <- k.cand[,k.cand.i] #add k-plex to list
      k.plex.i <- k.plex.i+1 #increment list pointer
    }
  }
  return(k.plex) #return the entire list
}

clique.community.2 <- function(graph, k) {
  clq <- igraph::cliques(graph, min=k, max=k)
  edges <- c()
  for (i in seq_along(clq)) {
    for (j in seq_along(clq)) {
      if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
        edges <- c(edges, c(i,j)-1)
      }
    }
  }
  clq.graph <- igraph::simplify(graph(edges))
  igraph::V(clq.graph)$name <- seq_len(igraph::vcount(clq.graph))
  comps <- igraph::decompose.graph(clq.graph)
  
  lapply(comps, function(x) {
    unique(unlist(clq[ igraph::V(x)$name ]))
  })
}

##this function will assign words to syn categories automatically
##assigns most frequent term as category name

synCats<-function(syn.ig,k=4,termfreq){
  kcliperc<-clique.community(syn.ig,k)
  catsize<-rapply(kcliperc, function(i) length(i))
  catname<-NULL
  for(i in 1:length(kcliperc)){
    pos<-match(V(syn.ig)$name[kcliperc[[i]]],names(termfreq))
    catname<-c(catname,names(sort(termfreq[pos],decreasing = TRUE))[1])
  }
  catname.long<-rep(catname,catsize)
  word.pos<-c(unlist(kcliperc))
  word.long<-V(syn.ig)$name[word.pos]
  temp<-data_frame(category=catname.long,word=word.long)
  return(temp)
}





save(giant.component, clique.community, clique.community.2, clique.community.opt, clique.community.opt.par, 
     create.cat, dichotMat, find.coloc, find.coloc.dir, find.colocs, findLHS, findLHShi, findPhrases, 
     findRHS, findRLhi, kplex, myReader, nclan, removeSparse, symMat, synCats, synEdges, terms2cats, 
     file="~/Desktop/R Project/contentfunctions.RData") 
##############################################################
###Read in Cleaned Data, do some minor subbing for stemming###
##############################################################
#this is only necessary because the original spellchecked text was messed up

#Repetition of some basic cleaning functions (to be sure)#
freeClean<-read.csv("spell checked/Spell Checked and Substituted Free Response Vectors2.csv")
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
freeClean$subbed<-gsub("products/consume","products consume",freeClean$subbed)
freeClean$subbed<-gsub("50/50","5050",freeClean$subbed)
freeClean$subbed<-gsub("exist.","exist",freeClean$subbed)
freeClean$subbed<-gsub("heard.","heard",freeClean$subbed)


##############################################################
###Build Corpus with Spell-Checked and Stemmed Data        ###
##############################################################
temp2<-freeClean[,c(2,5)]
colnames(temp2)<-c("doc_id","text")
mycorpus <- VCorpus(DataframeSource(temp2))

##create qcorpus to use in collocate functions
qcorpus<-quanteda::corpus(mycorpus)

#create dtm and ttm
sub.dtm<-DocumentTermMatrix(mycorpus) #Create the document-term matrix
sub.ttm<-t(as.matrix(sub.dtm))%*%as.matrix(sub.dtm) #Create the term-term matrix

##stem words by stemming column names
stem.cols<-stemDocument(colnames(sub.dtm))
stem.cols<-stemCompletion(stem.cols, unlist(strsplit(temp2$text, split=" ")), 
                          type="prevalent")
##the stemming messes up liberty, may do others
stem.cols<-ifelse(stem.cols%in%"",colnames(sub.dtm),stem.cols)
stem.cols<-gsub("liberty","liberal",stem.cols)
stem.cols<-gsub("liberties", "liberty", stem.cols)
stem.cols<-gsub("unitedstates","usa",stem.cols)
stem.cols<-gsub("america","usa",stem.cols)
stem.cols<-gsub("washington d.c", "washington,dc", stem.cols)
stem.cols<-gsub("washington dc", "washington,dc", stem.cols)
stem.cols<-gsub("people,", "people", stem.cols)
stem.cols<-gsub("man,", "man", stem.cols)
stem.cols<-gsub("name,", "name", stem.cols)
stem.cols<-gsub("man,", "man", stem.cols)
stem.cols<-gsub("bottom-", "bottom", stem.cols)
stem.cols<-gsub("capitalism,", "capitalism", stem.cols)
stem.cols<-gsub("bottom-", "bottom", stem.cols)
stem.cols<-gsub("decision-making", "decisions", stem.cols)
stem.cols<-gsub("dictatorship.", "dictatorship", stem.cols)
stem.cols<-gsub("diversity.", "diversity", stem.cols)
stem.cols<-gsub("fair,", "fair", stem.cols)
stem.cols<-gsub("perfect,", "perfect", stem.cols)
stem.cols<-gsub("state,", "state", stem.cols)






##this is same as dtm1 and ttm1
stem.dtm<-as.matrix(sub.dtm)
colnames(stem.dtm)<-stem.cols
stem.dtm<-t(rowsum(t(stem.dtm), group=stem.cols))
stem.dtm<-ifelse(stem.dtm>0,1,0)
stem.ttm<-t(stem.dtm)%*%stem.dtm

#reorder ttm1 matrix by number of mentions
stem.ttm<-stem.ttm[order(diag(stem.ttm), decreasing=TRUE), 
                   order(diag(stem.ttm), decreasing=TRUE)]

#original corpus dtm has multiple mentions recorded (some cells>1)
#Create matrix of how many respondents mentioned each concept at least once#
sub.dtm1<-as.matrix(sub.dtm)
sub.dtm1<-ifelse(sub.dtm1>0,1,0)
sub.ttm1<-t(as.matrix(sub.dtm1))%*%as.matrix(sub.dtm1)

######install wordnet
#go to terminal
#install homebrew (google for instructions)
# use the command below to install wordnet in terminal
# brew install wordnet
#then use this command in terminal to make sure jdk in R works
# sudo R CMD javareconf 
#in R, set the correct path for wordnet, then load library
Sys.setenv(WNHOME = "/usr/local/Cellar/wordnet/")
wordnet::setDict("/usr/local/Cellar/wordnet/3.1/dict/")


##############################################################
### Exploratory analysis to get a sense of categories      ###
##############################################################

#this is a custom function
#terms is the number of words in ngram
#min_count is minimum number of people with exact phrase for return
findPhrases(qcorpus,terms=3,min_count=4)
findPhrases(qcorpus,terms=2,min_count=4)

##create network of synonyms
##maybe use cliques 
syn.edge<-synEdges(colnames(stem.ttm))
syn.ig<-igraph::graph_from_edgelist(syn.edge,directed = TRUE)
syn.ig<-igraph::induced.subgraph(syn.ig,igraph::V(syn.ig)[igraph::degree(syn.ig)<350])
syn.giant<-giant.component(syn.ig)
cli<- igraph::maximal.cliques(syn.ig)
syncat<-synCats(syn.ig,k=4,allterms)
#remove cliques with one node
cli<-cli[rapply(cli, function(i) length(i))>1]

##create symmetric network and then check
g <- igraph::as.undirected(syn.ig,mode="collapse")
kcli<-RBGL::kCliques(igraph::igraph.to.graphNEL(g))

#try clique percolation
#this finds maximal cliques plus relaxes for some missing syn ties
kcliperc<-clique.community(syn.ig,4)
for(i in 1:length(kcliperc)){
  pos<-match(V(syn.ig)$name[kcliperc[[i]]],colnames(stem.ttm))
  print(sort(diag(stem.ttm)[pos], decreasing = TRUE))
}



##############################################################
###Create Categories using multi-steps from Schema.pdf     ###
##############################################################
#find keywords mentioned by at least 10% of respondents
#different threshold could be used based on frequency distribution
#find primary terms and secondary 5% terms
allterms<-diag(stem.ttm)
#terms10<-colnames(stem.ttm)[allterms/nrow(stem.dtm)>=.1]
#terms5<-colnames(stem.ttm)[allterms/nrow(stem.dtm)>=.05]
#k<-length(terms10)

categories<-names(allterms)
names(categories)<-names(allterms)

##create categories interactively with suggestions
##similar to spell check
##necessary information is provided
##"good" and "bad" categories are judgement calls
##some categories can be guesses, but information should guide
##not sure there is a more systematic way
##takes about 30-45 minutes to run through ~100+ words





#interactive category creation. stops if there are commas in the term

for(i in names(allterms[categories=="conflicts"])){
  lhcol<-findLHShi(qcorpus,i,5,.6)
  lrcol<-findRLhi(qcorpus,i,10,.4)
  syns<-allterms[names(allterms)%in%unlist(qdap::syn(c(i)))]
  print(paste("word: ", i, sep=""))
  print(paste("current category: ", categories[i], sep=""))
  print(sprintf("appears in %3.0f (%2.2f%%) of docs", allterms[i], 
                allterms[i]/432))
  print(sprintf("Left Hand Collocates: (%2.0f%%)", .6*100))
  print(lhcol)
  print(sprintf("Left & Right Hand Collocates: (%2.0.f%%)", .4*100))
  print(lrcol)
  print("Synonyms with Frequency:")
  print(syns)
  if(kwic==TRUE){
    quanteda::kwic(qcorpus,i,window=2)
  }
  if(allterms[i]/432){
    print(paste("Suggested keyword category: ",i, sep=""))
  }
  if(length(lhcol)>0 && lhcol>0.75){
    print(paste("Suggested schema keyword: ", 
                names(lhcol), sep=""))
  }
  if(i%in%syncat$word){
    print(paste("Suggested synonym category: ", 
                syncat$category[syncat$word==i], sep=""))
  }
  categories[i]<-readline(prompt="New Category Name: ")
}


##if you write it out, read it back in this way
write.csv(categories, file="finalcats.csv")
temp<-read.csv(file = "finalcats.csv", row.names = 1)
categories<-as.vector(temp[,1])
names(categories)<-names(allterms)


 
##############################################################
###Use Categories to create new DTM and TTM matrices       ###
##############################################################
##this is same as dtm1 and ttm1

cat.dtm<-stem.dtm[,order(match(colnames(stem.dtm),colnames(stem.ttm)))]
##this is the point to select documents (Rs) based on demographics
#rep.dtm<-filter(cat.dtm,cat.dtm$rep==TRUE)
#free.cat.dtm<-subset(cat.dtm,"freedom">0)
#rep.dtm<-cat.dtm[survey$rep==TRUE,]

#cat.dtm<-(stem.dtm[,colSums(stem.dtm)>4])
colnames(cat.dtm)<-categories
cat.dtm<-t(rowsum(t(cat.dtm), group=categories))
cat.dtm<-ifelse(cat.dtm>0,1,0)
cat.ttm<-t(cat.dtm)%*%cat.dtm



dim(cat.dtm) 
dim(cat.ttm) 

##need to load in defdem - it can overwrite other things
##maybe load at top of file
##but for now this works to set up free_response_term_net

save(sub.dtm1, sub.ttm1, mycorpus, cat.ttm.sm, cat.ttm.bin, cat.ttm, cat.dtm,
     stem.dtm, stem.ttm, defdem, categories, qcorpus, syncat, syn.edge, syn.ig, cli,g, kcli,
     file="~/Desktop/R Project/projectdata4.RData")
#save(sub.dtm1, sub.ttm1, mycorpus, categories, cat.ttm, cat.dtm, 
#S     stem.dtm, stem.ttm, defdem, file="projectdata.RData")




##############################################################
###Network visualization and other attempt to cluster      ###
##############################################################
#only keep terms that are said by 1-3% of people
#in this case, 5 people (114 words)
cat.ttm.sm<-removeSparse(cat.ttm,.01,432)
n<-dim(cat.ttm.sm)[1]

#find % cooccurance (in columns)
cat.ttm.bin<-cat.ttm.sm%>%
  sweep(.,2,FUN="/",STATS=diag(.))

#now find collocates at a certain level
#find.coloc.dir(stem.ttm.bin,c=.8)

#create graphs
cat.stat<-network::as.network.matrix(dichotMat(cat.ttm.bin,.25))
cat.ig<-igraph::graph_from_adjacency_matrix(cat.ttm.bin,
                                              diag=FALSE, weighted=TRUE)
cat.ig<-igraph::subgraph.edges(cat.ig,which(igraph::E(cat.ig)$weight>=.5))


g<-igraph::graph_from_adjacency_matrix(dichotMat(cat.ttm.bin,.5), mode="upper")
cl <- RBGL::kCliques(igraph::igraph.to.graphNEL(g))

g<-symMat(cat.ttm.bin)

#what is giant.ig and terms.docs?
plot(cat.ig
     , vertex.size=1, edge.arrow.size=.3, vertex.label.cex=log(cat.dtm)/3)
#plot(terms.ig, vertex.size=1, edge.arrow.size=.3, 
#     vertex.label.cex=log(term.docs)/3) 

#what is this
stem.ttm.bin<-stem.ttm.sm[(k+1):n, (k+1):n]%>%
  sweep(.,1,FUN="/",STATS=diag(.))

for(i in 1:k){
  assoc<-colnames(stem.ttm.sm)[stem.ttm.sm[(k+1):n,i]/
                                 diag(stem.ttm.sm)[(k+1):n]>.85]
  assoc<-c(cat.terms[i],assoc)
  assign(paste("cat",cat.terms[i],sep="."), assoc)
} #what is cat.terms?



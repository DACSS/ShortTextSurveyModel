#Working directory, free response data, remove incomplete responses, make matrix#

free<-read.csv("Cleaned Scripts and Data/freeresponse_withid.csv",header=T,stringsAsFactors=F,na.strings="")
free<-free[is.na(free[,2])==FALSE,]
free2<-as.matrix(free[,2:ncol(free)])

#Function to clean text for spell-checking#
text_process <- function(text){
  text <- iconv(text,to="UTF-8")
  text <- gsub("  "," ",text)
  text <- gsub("  "," ",text)
  text <- gsub("^\\s+|\\s+$", "", text)
  text <- gsub("[[:punct:]]","",text)
  text <- tolower(text)
  text
}
free3<-text_process(free2) #Run on all free response data
d<-rle(sort(free3)) #Run length encoding for frequency
termsCount<-data.frame(term = d$values, #Create frequency dataframe
                       count = d$lengths)
termsCount$term<-as.character(termsCount$term) #Process its content
termsCountSorted <- termsCount[order(-termsCount$count),] #Create new, sorted version

#Begin corpus operations to process the responses#
library(tm) 
corpus<-Corpus(VectorSource(termsCountSorted$term)) #Create corpus
corpus<-tm_map(corpus,stemDocument) #Stem the terms
dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")),
                      stringsAsFactors=F) #Get dataframe of all stemmed terms
df<-data.frame(term=unique(dataframe[,1])) #Only unique values
df[,1]<-as.character(df[,1]) #Processed as characters
for(i in 1:nrow(df)){ #Add count variable 
  df$count[i]<-length(grep(df[i,1],free3))
}
dfSorted<-df[order(-df$count),] #Sort by count

#Write out some relevant files#
write.csv(termsCountSorted,"Unique Terms from Free Response Sorted by Frequency.csv")
write.csv(dfSorted,"Unique Stemmed Terms from Free Response Sorted by Frequency.csv")
hist(termsCountSorted$count[1:50],breaks="FD") #Check distribution of sorted term counts

#Replace NA values with""#
for (i in 2:21){
  free[,i][is.na(free[,i])]<-""
}
m<-list(ID="ID",content="txt") #Hollow list to map content reading
myReader<-readTabular(mapping=m) #Reading function

#Dataframe for parsed content, lowercased, cleaned of stopwords and emojis#
temp<-data.frame(ID=free[,1],txt=apply(free[,2:21],1,function(x) paste(x,collapse=" ")))
temp$txt<-tolower(temp$txt)
temp$txt<-removeWords(temp$txt,stopwords("english"))
temp$txt<-gsub("  "," ",temp$txt)
temp$txt<-gsub(":-\\(","frownie",temp$txt)

#Create corpus using reading function#
mycorpus<-Corpus(DataframeSource(temp),readerControl=list(reader=myReader))
dictCorpus<-mycorpus #Create identical counterpart for manipulation
stem.corpus<-tm_map(mycorpus,stemDocument) #Map stems
stemdtm<-DocumentTermMatrix(stem.corpus) #Create document-term matrix
colnames(stemdtm)<- stemCompletion(colnames(stemdtm),dictCorpus) #Name with function
mydtm<-DocumentTermMatrix(mycorpus) #Create another document-term matrix from original

#Use the qdap package for interactive spell-checking#
install.packages("qdap")
library(qdap)
#Example code below to clarify mechanism of spell-checking#
# x <- "Robots are evl creatres and deserv exterimanitation."
# which_misspelled(x, suggest=FALSE)
# which_misspelled(x, suggest=TRUE)
# which_misspelled(temp$txt[1],suggest=TRUE)
m<-check_spelling_interactive(temp$txt) #Long process - interactive
temp$textChecked<-m #Replace original with checked content

#Specific issues manually corrected#
temp$textChecked<-gsub("independencee","independence",temp$textChecked)
temp$textChecked<-gsub("peoplee","people",temp$textChecked)
temp$textChecked<-gsub("governmenternment","government",temp$textChecked)
temp$textChecked<-gsub("governmenter","govern",temp$textChecked)
temp$subbed<-temp$textChecked

#Caring for hyphenation or shortening among other specific terms#
#Build noun and hyphen replacements in pairs#
proper.nouns <-c("president","President","supreme court","SupremeCourt",
                 "ellis island","EllisIsland","civil war","CivilWar",
                 "magna carte","MagnaCarta","abraham lincoln","AbrahamLincoln",
                 "enron","Enron","iphone","iPhone","indians","Indians",
                 "microsoft","Microsoft","f86 sabre","F86-Sabre")
proper<-data.frame(matrix(proper.nouns,length(proper.nouns)/2,2))
hyphen<-c("free market","free-market","open minded","open-minded","non exclusion",
          "non-exclusion","potato chips","potato-chips")
hyphen<-data.frame(matrix(hyphen,length(hyphen)/2,2))
subs<-rbind(proper,hyphen)
names(subs)<-c("Pre","Post")
for (i in seq(1,length(proper.nouns),2)){ #Cycle through propers noun replacements
  for (j in seq(2,length(proper.nouns),2)){
    temp$subbed<-gsub(proper.nouns[i],proper.nouns[j],temp$subbed)
  }
}
for (i in seq(1,length(hyphen),2)){ #Cycle through hyphen replacements
  for (j in seq(2,length(hyphen),2)){
    temp$subbed<-gsub(hyphen[i],hyphen[j], temp$subbed)
  }
}

#Caring for lemmas which were not addressed in stemming#
temp$subbed<-gsub("vot*\\b","voting",temp$subbed)
temp$subbed<-gsub("equal*\\b","equality",temp$subbed)
temp$subbed<-gsub("right[s]?\\>","rights",temp$subbed) #Note, this gets rid of 'righteous'
temp$subbed<-gsub("gov*\\b", "government",temp$subbed)
temp$subbed<-gsub("america*\\b","america",temp$subbed)
temp$subbed<-gsub("elect*\\b","election",temp$subbed)
temp$subbed<-gsub("represent*\\b","representation",temp$subbed)
temp$subbed<-gsub("free[^-]*\\b", "freedom",temp$subbed)
temp$subbed<-gsub("vot[a-z]*\\b","voting", temp$subbed)
temp$subbed<-gsub("\\b[a-z]*[[:punct:]]*equal[a-z]*\\b","equality", temp$subbed)
temp$subbed<-gsub("right[s]?\\>","rights", temp$subbed) ##note, this gets rid of righteous
temp$subbed<-gsub("gov[a-z]*\\b", "government", temp$subbed)
temp$subbed<-gsub("self[[:punct:]]government", "government", temp$subbed)
temp$subbed<-gsub("government[[:punct:]]*", "government", temp$subbed)
temp$subbed<-gsub("america[a-z]*\\b","america", temp$subbed)
temp$subbed<-gsub("america[[:punct:]]+","america", temp$subbed)
temp$subbed<-gsub("[s]*elect[a-z]*\\b","election", temp$subbed)
temp$subbed<-gsub("represent[a-z]*\\b", "representation", temp$subbed)
temp$subbed<-gsub("free[^-][a-z]*\\b", "freedom", temp$subbed)
temp$subbed<-gsub("freedom[[:punct:]]+", "freedom", temp$subbed)
temp$subbed<-gsub("\\bfree\\b", "freedom", temp$subbed)
temp$subbed<-gsub("freedommark[a-z]*\\b","freedom market", temp$subbed)
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

#Write this out for further use (line 99 forward) in survey_data_generate.R#
write.csv(temp,"Spell Checked and Substituted Free Response Vectors2.csv")
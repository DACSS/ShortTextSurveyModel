#' @export

corpus <- function(termsCountSorted, text){
  data <- Corpus(VectorSource(file$term)) %>%
    tm_map(stemDocument)
  dataframe<-data.frame(text=unlist(sapply(data, `[`, "content")),
                        stringsAsFactors=F) #Get dataframe of all stemmed terms
  df<-data.frame(term=unique(dataframe[,1])) #Only unique values
  df[,1]<-as.character(df[,1]) #Processed as characters
  for(i in 1:nrow(df)){ #Add count variable 
    df$count[i]<-length(grep(df[i,1],text))
  }
  dfSorted<-df[order(-df$count),] #Sort by count
  write.csv(dfSorted,"Unique Stemmed Terms from Free Response Sorted by Frequency.csv")
  dfSorted
}
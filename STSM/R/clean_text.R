#' @export

text_process <- function(text){
  text <- iconv(text, to = "UTF-8")
  text <- gsub("  ", " ", text)
  text <- gsub("^\\s+|\\s+$", "", text)
  text <- gsub("[[:punct:]]","",text)
  text <- tolower(text)
  text
}

clean_text <- function (free){
  free <- text_process(free)
  freq <- rle(sort(free))
  termsCount <- data.frame(term = freq$values, 
                           count = freq$lengths)
  termsCount$term <- as.character(termsCount$term)
  termsCountSorted <- termsCount[order(-termsCount$count),]
  write.csv(termsCountSorted,"Unique Terms from Free Response Sorted by Frequency.csv")
  hist(termsCountSorted$count[1:50],breaks="FD") #Check distribution of sorted term counts
  termsCountSorted
}
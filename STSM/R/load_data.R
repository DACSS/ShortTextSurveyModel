################################################################################
################################################################################
#     SHORT TEXT SURVEY MODELS CLEANED CODE FOR SURVEY DATA GENERATE CLEANED
#
#
#
#
#
# Libraries Included
## library(tidyverse)
## library(tm)
################################################################################

#' @export

load_file <- function(file_location){
  free <- read.csv(file_location)
  free <- free %>%
    tidyverse::filter(is.na(free[,2]) == FALSE) %>%
    as.matrix(free[,2:ncol(free)])
  free
}


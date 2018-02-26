##if you just want to replicate using my seattleData.RData
##and LAData.RData:
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#set up environment and functions
source_lines("environment.R",c(2:8,101:110))
load("seattleData.RData")
#clean data
source("cleandata.R")
#train and test models
source("models.R")
#test models on LA data
load("LAData.RData")
source("LAtest.R")
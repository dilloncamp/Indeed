library(RSelenium)
library(magrittr)
library(tm)
library(SnowballC)
library(wordcloud)
library(caret)
library(dplyr)
library(stringr)

# RSelenium::startServer() if required
require(RSelenium)

remDR <- rsDriver(port = 4444L, browser = c("chrome", "firefox", "phantomjs",
                                            "internet explorer"), version = "latest", chromever = "latest",
                  geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
                  verbose = TRUE, check = TRUE)
cprof <- getChromeProfile("C:/Users/dillo/AppData/Local/Google/Chrome/User Data/", "Profile 1")
remDr <- remoteDriver(browserName = "chrome", extraCapabilities = cprof)
remDr$open()


jobTitle <- c()
jobDesc <- c()
jobLev <- c()
jobComp <- c()
number <- 25
findJobs <- function(url,jobLevel){
  jobTitle <- c()
  jobDesc <- c()
  jobLev <- c()
  jobComp <- c()
  number <- 25
  remDr$navigate(url)
  ##entry level jobs
  stopLooking <- 0
  totalJobs <- length(jobDesc) + 1
  while(!stopLooking){
    webElem <- remDr$findElements(using = 'css selector', value = ".jobtitle .turnstileLink")
    jobNum <- length(unlist(webElem))
    job_counter <- 1
    start_job <- job_counter
    end_job <- min(start_job + 9,length(webElem))
    for(job in (start_job):(end_job)){
  
      
      elem <- webElem[[job]]
      jobTitle[totalJobs] <- unlist(elem$getElementText())
      elem$clickElement()
      
      
      
      webElem2 <- tryCatch({
        suppressMessages({
          webElem2 <- remDr$findElement(using = 'css selector', value = "#vjs-content")
          content <- webElem2$getElementText()
          content
        })
      }, 
      error = function(e) {
        FALSE
      }
      )
      
      while(isTRUE(is.logical(webElem2))){
        Sys.sleep(runif(1,.5,2))
        webElem2 <- tryCatch({
          suppressMessages({
            webElem2 <- remDr$findElement(using = 'css selector', value = "#vjs-content")
            content <- webElem2$getElementText()
            content
          })
        }, 
        error = function(e) {
          FALSE
        }
        )
      }
      jobDesc[totalJobs] <- unlist(webElem2)
      jobLev[totalJobs] <- jobLevel #one denotes data scientist
      totalJobs <- totalJobs +1
    }
    webElem3 <- remDr$findElements(using = 'css selector', value = ".np")
    for(elements in 1:length(unlist(webElem3))){
      stopLooking <- ifelse(webElem3[[elements]]$getElementText()=="Next »",0,1)
      elementNum <- if(webElem3[[elements]]$getElementText()=="Next »"){
        number <- elements
      }
    }
    if(stopLooking == 0){
      webElem3[[number]]$clickElement()
      Sys.sleep(runif(1,.5,2))
    }
    
    
    Sys.sleep(runif(1,.5,2))
  }
  output <- cbind(jobTitle,jobDesc,jobLev)
  output
}

Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  return(temp)
}


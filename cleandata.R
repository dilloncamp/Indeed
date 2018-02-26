#clean dataScience
uniqueIndex <- which(duplicated(dataScience[,"jobDesc"])==FALSE)
dataScienceClean <- dataScience[uniqueIndex,]
dataScienceClean <- as.data.frame(dataScienceClean,stringsAsFactors = FALSE)
dataScienceClean$jobLev <- factor(dataScienceClean$jobLev)
rm(uniqueIndex)

#clean busAnalyst
uniqueIndex <- which(duplicated(businessAnalyst[,"jobDesc"])==FALSE)
businessAnalystClean <- businessAnalyst[uniqueIndex,]
businessAnalystClean <- as.data.frame(businessAnalystClean,stringsAsFactors = FALSE)
businessAnalystClean$jobLev <- factor(businessAnalystClean$jobLev)
rm(uniqueIndex)

#clean busIntAnalyst
uniqueIndex <- which(duplicated(businessIntAnalyst[,"jobDesc"])==FALSE)
businessIntAnalystClean <- businessIntAnalyst[uniqueIndex,]
businessIntAnalystClean <- as.data.frame(businessIntAnalystClean,stringsAsFactors = FALSE)
businessIntAnalystClean$jobLev <- factor(businessIntAnalystClean$jobLev)
rm(uniqueIndex)

#clean dataAnalyst
uniqueIndex <- which(duplicated(dataAnalyst[,"jobDesc"])==FALSE)
dataAnalystClean <- dataAnalyst[uniqueIndex,]
dataAnalystClean <- as.data.frame(dataAnalystClean,stringsAsFactors = FALSE)
dataAnalystClean$jobLev <- factor(dataAnalystClean$jobLev)
rm(uniqueIndex)

#combine busAnalystClean, businessIntAnalystClean, dataAnalystClean
#check for duplicates
tempJobData <- rbind(businessAnalystClean, businessIntAnalystClean, dataAnalystClean)
tempJobData <- as.data.frame(tempJobData,stringsAsFactors=FALSE)
tempJobData$jobLev <- factor(tempJobData$jobLev)
uniqueIndex <- which(duplicated(tempJobData$jobDesc)==FALSE)
tempJobData <- tempJobData[uniqueIndex,]
rm(uniqueIndex)

#combine dataScienceClean with tempJobData and remove any duplicates
jobData <- rbind(dataScienceClean,tempJobData)
jobData <- as.data.frame(jobData,stringsAsFactors=FALSE)
jobData$jobLev <- factor(jobData$jobLev)
dupIndex <- which(duplicated(jobData$jobDesc) | duplicated(jobData$jobDesc, fromLast = TRUE) & jobData$jobLev==1)
jobData <- jobData[-dupIndex, ]
rm(dupIndex)


for(row in 1:nrow(jobData)){
  jobData$jobDesc[row] <- Clean_String(jobData$jobDesc[row])
  
}

#randomly shuffle data
set.seed(123)
jobData <- jobData[sample(nrow(jobData)),]
jobData <- jobData[sample(nrow(jobData)),]
dataInd <- which(jobData$jobLev==1)

data_corpus <- VCorpus(VectorSource(jobData$jobDesc[dataInd]))
data_corpus_clean <- tm_map(data_corpus, content_transformer(tolower))
data_corpus_clean <- tm_map(data_corpus_clean, removeNumbers)
data_corpus_clean <- tm_map(data_corpus_clean, removeWords, c(stopwords("english"),"will"))
data_corpus_clean <- tm_map(data_corpus_clean, removePunctuation)
#data_corpus_clean <- tm_map(data_corpus_clean, stemDocument)
data_corpus_clean <- tm_map(data_corpus_clean, stripWhitespace)
dataWordcloud <- wordcloud(data_corpus_clean, scale = c(4,.25),max.words = 35, random.order = FALSE)



other_corpus <- VCorpus(VectorSource(jobData$jobDesc[-dataInd]))
other_corpus_clean <- tm_map(other_corpus, content_transformer(tolower))
other_corpus_clean <- tm_map(other_corpus_clean, removeNumbers)
other_corpus_clean <- tm_map(other_corpus_clean, removeWords, c(stopwords("english"),"will"))
other_corpus_clean <- tm_map(other_corpus_clean, removePunctuation)
#other_corpus_clean <- tm_map(other_corpus_clean, stemDocument)
other_corpus_clean <- tm_map(other_corpus_clean, stripWhitespace)
otherWordCloud <- wordcloud(other_corpus_clean, scale = c(4,.25),max.words = 35, random.order =FALSE)



job_corpus <- VCorpus(VectorSource(jobData$jobDesc))
job_corpus_clean <- tm_map(job_corpus, content_transformer(tolower))
job_corpus_clean <- tm_map(job_corpus_clean, removeNumbers)
job_corpus_clean <- tm_map(job_corpus_clean, removeWords, c(stopwords("english"),"will"))
job_corpus_clean <- tm_map(job_corpus_clean, removePunctuation)
wordcloud(job_corpus_clean, scale = c(4,.25),max.words = 50, random.order =FALSE)
job_corpus_clean <- tm_map(job_corpus_clean, stemDocument)
job_corpus_clean <- tm_map(job_corpus_clean, stripWhitespace)


job_dtm <- DocumentTermMatrix(job_corpus_clean)
job_labels <- jobData[,"jobLev"]


smp_size <- floor(0.70 * nrow(job_dtm))
set.seed(123)
train_ind <- sample(seq_len(nrow(job_dtm)), size = smp_size)
job_dtm_train <- job_dtm[train_ind,]
job_dtm_test <- job_dtm[-train_ind,]
job_train_labels <- jobData[train_ind,"jobLev"]
job_test_labels <- jobData[-train_ind,"jobLev"]
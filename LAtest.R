load("LAData.RData")

##DATA PROCESSING##

#clean LAdataScience
uniqueIndex <- which(duplicated(LAdataScience[,"jobDesc"])==FALSE)
LAdataScienceClean <- LAdataScience[uniqueIndex,]
LAdataScienceClean <- as.data.frame(LAdataScienceClean,stringsAsFactors = FALSE)
LAdataScienceClean$jobLev <- factor(LAdataScienceClean$jobLev)

rm(uniqueIndex)

#clean busAnalyst
uniqueIndex <- which(duplicated(LAbusinessAnalyst[,"jobDesc"])==FALSE)
LAbusinessAnalystClean <- LAbusinessAnalyst[uniqueIndex,]
LAbusinessAnalystClean <- as.data.frame(LAbusinessAnalystClean,stringsAsFactors = FALSE)
LAbusinessAnalystClean$jobLev <- factor(LAbusinessAnalystClean$jobLev)
rm(uniqueIndex)

#clean busIntAnalyst
uniqueIndex <- which(duplicated(LAbusinessIntAnalyst[,"jobDesc"])==FALSE)
LAbusinessIntAnalystClean <- LAbusinessIntAnalyst[uniqueIndex,]
LAbusinessIntAnalystClean <- as.data.frame(LAbusinessIntAnalystClean,stringsAsFactors = FALSE)
LAbusinessIntAnalystClean$jobLev <- factor(LAbusinessIntAnalystClean$jobLev)
rm(uniqueIndex)

#clean dataAnalyst
uniqueIndex <- which(duplicated(LAdataAnalyst[,"jobDesc"])==FALSE)
LAdataAnalystClean <- LAdataAnalyst[uniqueIndex,]
LAdataAnalystClean <- as.data.frame(LAdataAnalystClean,stringsAsFactors = FALSE)
LAdataAnalystClean$jobLev <- factor(LAdataAnalystClean$jobLev)
rm(uniqueIndex)

#combine busAnalystClean, businessIntAnalystClean, dataAnalystClean
#check for duplicates
tempJobData <- rbind(LAbusinessAnalystClean, LAbusinessIntAnalystClean, LAdataAnalystClean)
tempJobData <- as.data.frame(tempJobData,stringsAsFactors=FALSE)
tempJobData$jobLev <- factor(tempJobData$jobLev)
uniqueIndex <- which(duplicated(tempJobData$jobDesc)==FALSE)
tempJobData <- tempJobData[uniqueIndex,]
rm(uniqueIndex)

#combine dataScienceClean with tempJobData and remove any duplicates
LAjobData <- rbind(LAdataScienceClean,tempJobData)
LAjobData <- as.data.frame(LAjobData,stringsAsFactors=FALSE)
LAjobData$jobLev <- factor(LAjobData$jobLev)
dupIndex <- which(duplicated(LAjobData$jobDesc) | duplicated(LAjobData$jobDesc, fromLast = TRUE) & LAjobData$jobLev==1)
LAjobData <- LAjobData[-dupIndex, ]
rm(dupIndex)


for(row in 1:nrow(LAjobData)){
  LAjobData$jobDesc[row] <- Clean_String(LAjobData$jobDesc[row])
  
}

#randomly shuffle data
set.seed(123)
LAjobData <- LAjobData[sample(nrow(LAjobData)),]
LAjobData <- LAjobData[sample(nrow(LAjobData)),]


LAjob_corpus <- VCorpus(VectorSource(LAjobData$jobDesc))
LAjob_corpus_clean <- tm_map(LAjob_corpus, content_transformer(tolower))
LAjob_corpus_clean <- tm_map(LAjob_corpus_clean, removeNumbers)
LAjob_corpus_clean <- tm_map(LAjob_corpus_clean, removeWords, c(stopwords("english"),"will"))
LAjob_corpus_clean <- tm_map(LAjob_corpus_clean, removePunctuation)
LAjob_corpus_clean <- tm_map(LAjob_corpus_clean, stemDocument)
LAjob_corpus_clean <- tm_map(LAjob_corpus_clean, stripWhitespace)


LAjob_dtm <- DocumentTermMatrix(LAjob_corpus_clean, 
                          control = list(dictionary=Terms(job_dtm_freq)))

LAjob_labels <- LAjobData[,"jobLev"]

LAjob_train_labels <- LAjobData[,"jobLev"]
LAjob_test_labels <- LAjobData[,"jobLev"]


LAjob_test <- apply(LAjob_dtm, MARGIN = 2, convert_counts)
##END DATA PROCESSING##

##BEGIN PREDICTIONS USING MODEL TRAINED ON SEATTLE DATA##

#naive bayes results
LAnb_pred <- predict(job_nb,LAjob_test)
LAcmnb <- confusionMatrix(LAnb_pred, LAjob_test_labels, positive="1")
##confusion table
LAcmnb$table
##Accuracy Percentage
LAcmnb$overall[1]

#knn results
LAknn_pred <- predict(job_knn,LAjob_test)
LAcmknn <- confusionMatrix(LAknn_pred, LAjob_test_labels, positive="1")
LAcmknn$table
LAcmknn$overall[1]

#rf results
LArf_pred <- predict(job_rf,LAjob_test)
LAcmrf <- confusionMatrix(LArf_pred, LAjob_test_labels, positive="1")
LAcmrf$table
LAcmrf$overall[1]

#lasso results
LAlasso_pred <- predict(job_lasso,LAjob_test)
LAcmlasso <- confusionMatrix(LAlasso_pred, LAjob_test_labels, positive="1")
LAcmlasso$table
LAcmlasso$overall[1]

#ridge results
LAridge_pred <- predict(job_ridge,LAjob_test)
LAcmridge <- confusionMatrix(LAridge_pred, LAjob_test_labels, positive="1")
LAcmridge$table
LAcmridge$overall[1]

##END PREDICTIONS##

##PRINT ACCURACY IN ORDER##

LAaccuracy <- data.frame("Model" = c("Naive Bayes","KNN","Random Forest","LASSO","Ridge"),
                    "Accuracy" = as.numeric(c(LAcmnb$overall[1],LAcmknn$overall[1], LAcmrf$overall[1],
                      LAcmlasso$overall[1],LAcmridge$overall[1])),stringsAsFactors = FALSE)

LAaccuracy <- LAaccuracy %>% arrange(desc(Accuracy))
print(LAaccuracy)
  #find words that appear 7 times
  job_freq_words <- findFreqTerms(job_dtm_train, 7)
  #str(job_freq_words)
  
  job_dtm_freq <- job_dtm[ , job_freq_words]
  job_dtm_freq_train <- job_dtm_train[ , job_freq_words]
  job_dtm_freq_test <- job_dtm_test[ , job_freq_words]
  
  #convert counts to "yes" or "no"
  convert_counts <- function(x) {
    x <- ifelse(x > 0, x, 0)
  }
  
  job <- apply(job_dtm_freq, MARGIN = 2, convert_counts)
  job_train <- apply(job_dtm_freq_train, MARGIN = 2, convert_counts)
  job_test <- apply(job_dtm_freq_test, MARGIN = 2, convert_counts)
  
  #naive bayes
set.seed(123)
train_control <- trainControl(method="cv",number=10,verboseIter = TRUE,savePredictions = TRUE)
job_nb <- train(job_train, job_train_labels,method="nb",trControl = train_control)

#knn
set.seed(123)
job_knn <- train(job_train, job_train_labels,method="knn",trControl = train_control)


#random forest
set.seed(123)
job_rf <- train(job_train, job_train_labels,method="rf",trControl = train_control)


#bagged tree
set.seed(123)
#job_bagged <- train(job_train, job_train_labels,method="adaboost",trControl = train_control)


#linear lasso
set.seed(123)
job_lasso <- train(job_train, job_train_labels,method="glmnet",trControl = train_control,
                    tuneGrid=expand.grid(
                      .alpha=1,
                      .lambda=seq(0, 100, by = 0.1)))

#linear ridge
set.seed(123)
job_ridge <- train(job_train, job_train_labels,method="glmnet",trControl = train_control,
                    tuneGrid=expand.grid(
                      .alpha=0,
                      .lambda=seq(0, 100, by = 0.1)))



#naive bayes results
nb_pred <- predict(job_nb,job_test)
cmnb <- confusionMatrix(nb_pred, job_test_labels, positive="1")
cmnb$table
cmnb$overall[1]

#knn results
knn_pred <- predict(job_knn,job_test)
cmknn <- confusionMatrix(knn_pred, job_test_labels, positive="1")
cmknn$table
cmknn$overall[1]

#rf results
rf_pred <- predict(job_rf,job_test)
cmrf <- confusionMatrix(rf_pred, job_test_labels, positive="1")
cmrf$table
cmrf$overall[1]

#lasso results
lasso_pred <- predict(job_lasso,job_test)
cmlasso <- confusionMatrix(lasso_pred, job_test_labels, positive="1")
cmlasso$table
cmlasso$overall[1]

#ridge results
ridge_pred <- predict(job_ridge,job_test)
cmridge <- confusionMatrix(ridge_pred, job_test_labels, positive="1")
cmridge$table
cmridge$overall[1]

##All model accuracy
Accuracy <- data.frame("Model" = c("Naive Bayes","KNN","Random Forest","LASSO","Ridge"),
                         "Accuracy" = as.numeric(c(cmnb$overall[1],cmknn$overall[1], cmrf$overall[1],
                                                   cmlasso$overall[1],cmridge$overall[1])),stringsAsFactors = FALSE)

Accuracy <- Accuracy %>% arrange(desc(Accuracy))
print(Accuracy)
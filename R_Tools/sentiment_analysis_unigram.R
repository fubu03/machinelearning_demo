#Sinstall.packages("RTextTools")
#install.packages("e1071")
#install.packages(tm)
library(tm)
library(RTextTools)
library(e1071)
library(gmodels)

#supervised postitve tweets
pos_tweets<-rbind(
  c('I love this movie', 'positive'),
  c('This view is great', 'positive'),
  c('I feel amazing this morning', 'positive'),
  c('I am very excited about the award show', 'positive'),
  c('He is my best buddy', 'positive')
)

#supervised negative tweets
neg_tweets<-rbind(
  c('I do not like this movie at all', 'negative'),
  c('This view is so horrible', 'negative'),
  c('I feel very tired this morning', 'negative'),
  c('I am not looking forward to the award show', 'negative'),
  c('He is my enemy', 'negative')
)

#natural language texts
test_tweets<-rbind(
  c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative')
)

#combining them into one matrix
tweets<-rbind(pos_tweets, neg_tweets, test_tweets)
View(tweets)

# build dtm using RTexttools package
tweets_dtm<-create_matrix(tweets[,1],language="english",removeStopwords=FALSE
                               ,removeNumbers=FALSE,stemWords=FALSE)

#training naive bayes - converting vectors to factors
#findFreqTerms(tweets_dtm)

tweet_dtm_m<-as.matrix(tweets_dtm)
str(tweet_dtm_m)
classifier<-naiveBayes(tweet_dtm_m[1:10,],as.factor(tweets[1:10,2]))

#step further to test the accuracy

predicted<-predict(classifier,tweet_dtm_m[11:15,])

table(tweets[11:15,2],predicted)
CrossTable(tweets[11:15,2],predicted,prop.chisq = F,dnn = c('Actual','Predicted'))
recall_accuracy(tweets[11:15,2],predicted)
  
# now trying same with other algorithms using RTextTools Package


# building a container of data to clearly specify
#our response variable, training set and testing set

?create_container
container<-create_container(tweets_dtm,as.numeric(as.factor(tweets[,2])),
                            trainSize = 1:10,testSize = 11:15,virgin = F)


#training model on different algos


models<-train_models(container,algorithms = c("MAXENT","RF","BAGGING","TREE"))
results = classify_models(container, models)


#Accuracy
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])

#Recall

recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"BAGGING_LABEL"])


# model summary

analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)


#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#                doing sentiment analysis for tweeter tweets
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

## LOADING DATA

setwd("G:/R/workspace/Twitter-Sentimental-Analysis-master")
list.files()
happy<-readLines("./happy.txt")
sad<-readLines("sad.txt")

happy_test<-readLines("happy_test.txt")
sad_test<-readLines("sad_test.txt")

tweet<-c(happy,sad)

tweet_test<-c(happy_test,sad_test)

tweet_all<-c(tweet,tweet_test)

sentiments<-c(rep("happy",length(happy) ), rep("sad",length(sad)))
sentiments_test<-c(rep("happy",length(happy_test) ), rep("sad",length(sad_test)))
sentiments_all<-as.factor(c(sentiments,sentiments_test))


#trying Naive Bayes first

matrix<-create_matrix(tweet_all,language = "english",removeNumbers = T,
                      removeStopwords = T,stemWords = F)

matrix1<-as.matrix(matrix)
matrix1<-matrix1[order(runif(1:180)),]
View(matrix1)
summary(matrix1)
table(matrix1)
classifier<-naiveBayes(matrix1[1:160],as.factor(sentiments_all[1:160]))
predicted<-predict(classifier,matrix1[161:180,])

predicted

table(sentiments_test,predicted)
recall_accuracy(sentiments_test,predicted)


#trying other methods


matrix2<-create_matrix(tweet_all,language = "english",removeStopwords = T,
                       removeNumbers = T,stemWords = F)
View(matrix2)
typeof(sentiments_all)
sentiments_test
container<-create_container(matrix2,as.numeric(sentiments_all),
                            trainSize = 1:160,testSize = 161:180,virgin = F)



models<-train_models(container,algorithms = c("MAXENT","RF","TREE",
                                              "BOOSTING","BAGGING"))
#testing model

results<-classify_models(container,models)
table(as.numeric(sentiments_all[161:180]),
      results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(sentiments_all[161:180]),
                results[,"FORESTS_LABEL"])

#Here we also want to get the formal test results, including:
# analytics@algorithm_summary: Summary of precision, recall, f-scores,
#and accuracy sorted by topic code for each algorithm

# analytics@label_summary: Summary of label (e.g. Topic) accuracy
# analytics@document_summary: Raw summary of all data and scoring
# analytics@ensemble_summary:ensemble precision/coverage.

# formal tests

analytics<-create_analytics(container,results)
summary(analytics)  
N=3
cross_MAXENT = cross_validate(container,N,"MAXENT")

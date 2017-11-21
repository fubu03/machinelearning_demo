# spam ham filtering

sms_raw<-read.csv("sms_spam.csv",stringsAsFactors = F)
str(sms_raw)
#currently categorical so convrting into factor
sms_raw$type<-factor(sms_raw$type)
table(sms_raw$type)

str(sms_raw$type)

#data preparation
install.packages("tm")
library(tm)

#creating a corpus - collection of text documents

sms_corpus<-Corpus(VectorSource(sms_raw$text))
str(sms_corpus)
#for more info on corpus
print(vignette("tm"))

print(sms_corpus)

inspect(sms_corpus[1:3])

#mapping common words

tm_map()
corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)

#removing stop words like to , and but.

corpus_clean<-tm_map(corpus_clean,removeWords,stopwords())

#removing punctuation

corpus_clean<-tm_map(corpus_clean,stripWhitespace)

inspect(corpus_clean[1:3])

#tokenization using documentermmatrix()

sms_dtm<-DocumentTermMatrix(corpus_clean)
str(sms_dtm)
sms_dtm
#------- Now creating test and train data ---------#

#splitting raw data frame
sms_raw_train<-sms_raw[1:4169,]
sms_raw_test<-sms_raw[4170:5559,]

#now document term matrix
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5559,]

#finally the corpus

sms_corpus_train<-corpus_clean[1:4169]
sms_corpus_test<-corpus_clean[4170:5559]

#to conform subsets are representing complete data sets, lets check the
#proportion of spam

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))


#visualizing word cloud

install.packages("wordcloud")
library(wordcloud)

#sample world cloud
wordcloud(sms_corpus_train,min.freq = 50, random.order = FALSE)

#now doing what we are here for:


spam<-subset(sms_raw_train,type == "spam")
ham<-subset(sms_raw_train,type == "ham")

View(spam)
# now word cloud can be created here as well by first converting into corpus 
#or using just the text column

wordcloud(spam$text,max.words = 40,scale = c(3,0.5))
wordcloud(ham$text,max.words = 40,scale = c(3,0.5))

#removing extra features - feature reduction
#removing words whose frequency is less than 0.1 percent

findFreqTerms()
#takes adtm and returns character vector

findFreqTerms(sms_dtm_train,5)

#we'll use dictionary() to save it for later use
#dictionary() no longer in use
sms_dict<-findFreqTerms(sms_dtm_train,5)

sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary = sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary = sms_dict))

#naive bayes works on categorical data, but dtms are in nos, so using a code 
#to convert all


convert_counts<-function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x,levels = c(0,1),labels = c("No", "Yes"))
  return(x)
}

#applying this function() in each column of our sparse matrix using apply()

sms_train<-apply(sms_train,MARGIN = 2,convert_counts)
sms_test<-apply(sms_test,MARGIN = 2,convert_counts)
View(sms_train)


# training the model on data

#e1071 package
library(e1071)
install.packages("e1071")


sms_classifier<-naiveBayes(sms_train,sms_raw_train$type)

#evaluating performance

sms_test_pred<-predict(sms_classifier,sms_test)

#compairing predicted values to the actual values using crossTable()

library(gmodels)
CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = F,prop.t = F)

#changing column names using dnn inbuilt 

CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = F,prop.t = F,dnn=c('predicted','actual'))

#improving model performance
#again training model
#using laplace estimator

sms_classifier2<-naiveBayes(sms_train,sms_raw_train$type,laplace = 1)
sms_test_pred2<-predict(sms_classifier2,sms_test)

CrossTable(sms_test_pred2,sms_raw_test$type,prop.chisq = F,prop.t = F,
           dnn = c('predicted','actual'))

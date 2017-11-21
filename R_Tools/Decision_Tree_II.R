# for finding poisonous mushrooms using rule learners

mushrooms<-read.csv("mushrooms.csv",stringsAsFactors = TRUE)


str(mushrooms$veil_type)
View(mushrooms)

#dropping this useless feature

mushrooms$veil_type<-NULL
table(mushrooms$type)

#using 1R over ZeroR

install.packages("RWeka")
library(RWeka)

mushrooms_1R<-OneR(type ~ .,data=mushrooms)
summary(mushrooms_1R)


#improving performance

?JRip()


mushrooms_JRip<-Jrip(type~.,data=mushrooms)

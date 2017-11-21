#apriori algorithm


groceries<-read.csv("groceries.csv")
View(groceries)
# but this will give very unsual data, which is not good

install.packages("arules")
library(arules)
groceries<-read.transactions("groceries.csv",sep = ",")
groceries
summary(groceries)

#read and understand the table

inspect(groceries[1:5])
itemFrequency(groceries[,1:3])
# this gives us the support level.


#NOW
#visualizing item support
itemFrequencyPlot(groceries,support=0.1)
itemFrequencyPlot(groceries,topN=20)

#visualizing entire sparse matrix

image(groceries[1:5])
image(sample(groceries,100))

#training the model on data

library(arules)
apriori(groceries)

#finally fianding rules+ look for diary while reffereing


grocery_rules<-apriori(groceries,parameter = list(support= 0.006,confidence=0.25,minlen=2))

grocery_rules

# lets see the rules now
summary(grocery_rules)
inspect(grocery_rules[1:5])

# improving performance


library(dplyr)
typeof(grocery_rules)
inspect(sort(grocery_rules, by= "lift")[261:270])

soda_rules<-subset(grocery_rules,items %in% "soda")
inspect(soda_rules)

#either soda or sausage
#  %pin%
#  %ain%
#  %in% c("soda","sausage")
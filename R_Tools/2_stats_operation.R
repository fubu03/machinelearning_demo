#starting with dummy data

usedcars<-read.csv("usedcars.csv",stringsAsFactors = F)

usedcars
View(usedcars)
str(usedcars)
attach(usedcars)
color
str(color)
summary(year)
summary(usedcars[c("price","mileage")])
summary(price)

# quartiles and Five number summary

summary()
min()
max()

#total range
range()
diff()
eval(21992-3800)
#or
range(price)
diff(range(price))

#inter quartile range
IQR()
#or
IQR(price)
eval(14904-10995)
#diff of 0.5 because R rounds up values in summary operation.

#To handle ties
?quantile()

#--------------------------
#Visualizing operations
#--------------------------

?boxplot()

boxplot(price,main="BoxPlot of Used Car Prices", ylab="Price($)")
boxplot(price,main="BoxPlot of Used Car Mileage", ylab="Odometer (mi.)")
#notch and varwidth options in box plot

hist(price,main="histogram of Used Car Prices", xlab="Price($)")

#for st.d and var

sd()
var()

#for categorical data
table(year)
table(model)

#performing calculations on them - 
#1.table proportions

prop.table()

model_table<-table(model)
prop.table(model_table)
#clarifying results

color_table<-table(color)
prop.table(color_table)
color_pct<-prop.table(color_table)*100
round(color_pct,digits = 1)


#2. measuring Central tendency - the mode

table(model)

#3. Scatter plot - for two attributes
plot()

plot(x=mileage,y=price,main="Scatter Plot of Prics vs Mileage",
     xlab = "Used car odomoter(.mi)",
     ylab = "Used Car Price($)")

#negative scatter plot- reducing - so neative corr
?subset()
x<-subset(usedcars,select = c(price,mileage))
View(x)
cor(x,method = "pearson")
#negative correlaion


#using cross table function for examining relationships
install.packages("gmodels")

library(gmodels)

CrossTable()

#first reducing no. of characters in color - not all reqd.
#because we really are interested in is whether or not the car color is conservative.


#splitting into groups for colors
usedcars$conservative<-usedcars$color %in% c("Black","Gray","Silver","White")
View(usedcars)
attach(usedcars)
table(conservative)
CrossTable(conservative)
#using cross table to check proprtion of cars varied by color
CrossTable(x=model,y=conservative,chisq = T)

#till chapter two of machine learning using R - by Bretz











































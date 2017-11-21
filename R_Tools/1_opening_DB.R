#opening a database connection

install.packages("RODBC")
library(RODBC)
print(vignette("RODBC"))

#opening a odbc connection with dsn name "my_dsn"

mydb<-odbcConnect("my_dsn",uid = "username",pwd = "password")

# use sqlQuery function
?sqlQuery()

paient_query<-"select * from patient_data where alive = 1"
patient_data<-"sqlQuery(channel = mydb,query = patient_query,stringsAsFactors=FALSE)"

#data frame will be created in patient_data

odbcClose(mydb)


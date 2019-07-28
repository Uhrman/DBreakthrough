require(tidyverse)
library(DBI)
library(RMariaDB)
library(stringr)

### загрузка данных
persons <- read_delim(file = "data/DSOP/DSOP.csv", delim = ";", col_names = TRUE, quote = "\"", na = c("?", "", "NA"), quoted_na=TRUE)
person_id <- 1

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00427/Datasets_Healthy_Older_People.zip"
td <- tempdir()
print(td)
tf <- tempfile(tmpdir=td, fileext=".zip")
download.file(url, tf)
calls <- read_csv(unz(tf, str_c("S1_Dataset/", persons$fn[person_id])), col_names = FALSE)

con <- dbConnect(RMariaDB::MariaDB(), dbname = "rsx", 
                 username = "root", password = "admin", host = "localhost")
dbListTables(con)

query <- "DELETE FROM rsensors"
rsDelete <- dbSendQuery(con, query)
dbClearResult(rsDelete)

pers <- str_sub(persons$fn[person_id], start = -1L, end = -1L)
ifelse (length(calls) == 0, r <- NULL, r <- seq(1:nrow(calls)))
for (i in r) {
    query<-str_c(
    "INSERT INTO rsensors(t, ax, az, ay, dummy1, dummy2, dummy3, dummy4, activity, person, gender) VALUES(", 
    calls$X1[i], ", ", calls$X2[i], ", ", calls$X3[i], ", ", calls$X4[i], ", ", 
    calls$X5[i], ", ", calls$X6[i], ", ", calls$X7[i], ", ", calls$X8[i], ", ", calls$X9[i], ", '",
    persons$fn[i], "', '", pers, "');", sep = "")
    rsInsert <- dbSendQuery(con, query)
    dbClearResult(rsInsert)  
}

dbDisconnect(con)

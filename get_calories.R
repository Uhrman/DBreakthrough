library(tidyverse)
library(data.table)
library(DBI)
library(RMariaDB)
library(stringr)

con <- dbConnect(RMariaDB::MariaDB(), dbname = "rsx", 
                 username = "root", password = "admin", host = "localhost")

query<-str_c("SELECT * FROM rsensors;", sep="")
rs = dbSendQuery(con,query)
calories<-as.data.table(dbFetch(rs))

calories[ , d_t := t - shift(t, 1L, type="lag")]
calories[ , d_ax := ax - shift(ax, 1L, type="lag", fill = 0)]
calories[ , d_az := az - shift(az, 1L, type="lag", fill = 0)]
calories[ , d_ay := ay - shift(ay, 1L, type="lag", fill = 0)]
calories[ , vx := 0]
calories[ , vz := 0]
calories[ , vy := 0]
calories[ , vx := shift(vx, 1L, type="lag", fill = 0) + ax*d_t]
calories[ , vz := shift(vz, 1L, type="lag", fill = 0) + az*d_t]
calories[ , vy := shift(vy, 1L, type="lag", fill = 0) + ay*d_t]
calories[ , vxa := (shift(vx, 1L, type="lag", fill = 0) + vx)/2]
calories[ , vza := (shift(vz, 1L, type="lag", fill = 0) + vz)/2]
calories[ , vya := (shift(vy, 1L, type="lag", fill = 0) + vy)/2]
calories[ , x := 0]
calories[ , z := 0]
calories[ , y := 0]
calories[ , x := shift(x, 1L, type="lag", fill = 0) + vxa*d_t]
calories[ , z := shift(z, 1L, type="lag", fill = 0) + vza*d_t]
calories[ , y := shift(y, 1L, type="lag", fill = 0) + vya*d_t]
calories[ , s := sqrt(x^2 + z^2+ y^2)]  
# (0,5 + 0,09 * %%уклона при подъеме) вес человека (кг) х расстояние (м) = сожженные каллории (не кКал!)
calories[ , cals := (0.5 + z/sqrt(x^2 + y^2)*0.09)*s*persons$weight[person_id]]
print(str_c("Пациент ",calories$person[1], ":"))
print(str_c(" - за период времени ", as.character(round(max(calories$t)/60, 1)), 
            " минут(ы) потрачено ", as.character(round(sum(calories$cals, na.rm = TRUE)/1000, 2)), " кКал"))

dbDisconnect(con)
  
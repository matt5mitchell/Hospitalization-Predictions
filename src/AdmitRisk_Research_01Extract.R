##############
## RESEARCH ##
##############
## Evaluate effect of subgroups on risk prediction

###########
## Setup ##
###########

#Libraries
if(!require("RODBC")){
  install.packages("RODBC")
}
library(RODBC)


#Working directory
setwd("working directory goes here")

#ODBC Connection
dbhandle <- odbcDriverConnect("connection string goes here")


#################
## Import Data ##
#################

#Set dates
refdatekey <- 20170101
diagsnapshotdatekey <- c(20161231, 20170131, 20170228, 20170331, 20170430, 20170531, 20170630, 20170731, 20170831, 20170930, 20171031, 20171130)

#Import views from DW
RAppts <- sqlQuery(dbhandle, 
                   "select * from appts")

RHosp <- sqlQuery(dbhandle, 
                  "select * from hosp")

#Active PC patients from program enrollment table as of reference date
client <- sqlQuery(dbhandle, paste0("select * from enrollments
                                    where StartDateKey <=", 
                                    refdatekey, 
                                    "and UnenrolledDateKey >=", 
                                    refdatekey)
)

#Diagnosis snapshots
diag <- list()
for (i in 1:12) {
diag[[i]] <- sqlQuery(dbhandle, paste0(
                  "select * from diag
                  where SnapshotDateKey =", diagsnapshotdatekey[i]))
}

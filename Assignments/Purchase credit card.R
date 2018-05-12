# The dataset comes from: https://catalog.data.gov/dataset/purchase-card-pcard-fiscal-year-2014
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(lubridate)
pcard<-read_csv("/Users/chriskuo/downloads/purchase_credit_card.csv", col_types = cols())
colnames(pcard)<-c("Year_Month","Agency_Number","Agency_Name",
                   "Cardholder_Last_Name", "Cardholder_First_Initial",
                   "Description","Amount","Vendor","Transaction_Date",
                   "Posted_Date","Category")
colnames(pcard)     
head(pcard$Transaction_Date)

###############################################
# Create variables from transaction date/time #
###############################################
# https://cran.r-project.org/web/packages/lubridate/lubridate.pdf

s<-as_date(pcard$Transaction_Date,format='%m/%d/%Y')
summary(hour(s))
pcard$transaction_date<-as_date(pcard$Transaction_Date,format='%m/%d/%Y')
pcard$trnx_year<-year(pcard$transaction_date)
pcard$trnx_month<-month(pcard$transaction_date)
pcard$trnx_day<-day(pcard$transaction_date)
pcard$trnx_week<-week(pcard$transaction_date)

pcard_date <- pcard %>% group_by(Agency_Number,transaction_date) %>% 
  mutate(lag_year = lag(trnx_year),
         lag_month = lag(trnx_month),
         lag_day = lag(trnx_day)
  )  

d<-pcard_date[100000:101000,]



data[, lag.value:=c(NA, value[-.N]), by=groups]


summary(pcard$trnx_day)
# Q. Do you see unusually frequent transactions in a month, a week or a day?

##################################
# Running total by Agency_Number #
##################################

ccDol <- pcard %>% group_by(Agency_Number) %>% 
  summarise(agent_amt = sum(Amount),
            agent_count = n() ) %>% 
  arrange(desc(agent_amt)) 
head(ccDol)

allDol <- pcard %>%   summarise(all_amt = sum(Amount),all_count = n() ) 
a<-merge(ccDol,allDol,all=TRUE)

agent_Dol <- a %>% mutate(agent_dolPercent = agent_amt / all_amt * 100,
                      agent_countPercent = agent_count / all_count *100,
                      agent_avg = agent_amt / agent_count) %>% select(-all_amt,-all_count)
agent_Dol

g <- ggplot(ccSum, aes(x=Agency_Number,y=total_amt))
# Number of cars in each class:
g +  geom_bar(stat="identity")
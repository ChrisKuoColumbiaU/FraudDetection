
library(dplyr)
#library(DataExplorer)
#library(xda)
library(ggplot2)
library(plotly)

ccard <- read.csv("/Users/chriskuo/Downloads/purchase_credit_card.csv")

dim(ccard)
summary(ccard)
colnames(ccard)

# Count of agencies
# Spent by agency
# Count by merchant.Category.Code
colnames(ccard)<-c('Year_Month', 'Agency_Number', 'Agency_Name', 'Cardholder_Last_Name',
      'Cardholder_First_Initial', 'Description', 'Amount', 'Vendor', 'Transaction_Date',
      'Posted_Date', 'Merchant_Category')

nrow(ccard)
head(ccard)
table(ccard$Year_Month)

# Calculate the average amount by agency_name and erchant category
avg_agency <- ccard %>% group_by(Agency_Name, Merchant_Category) %>%
       summarise( mean_category_amount = mean(Amount),
                  mean_count_trans =n()
                )
head(avg_agency)

# Append the average statistics back to the data to derive the ratios.
# Select the most recent 4 transactions 
per_agency_category <- ccard %>% group_by(Agency_Name, Merchant_Category, Year_Month) %>%
       summarise( max_amount = max(Amount),
                  mean_amount = mean(Amount),
                  count_trans =n()
                ) %>%
       left_join(avg_agency, by=c('Agency_Name','Merchant_Category')) %>%
       mutate( max_amount_ratio = max_amount / mean_category_amount,
               mean_amount_ratio = mean_amount / mean_category_amount,
               mean_count_ratio  = count_trans / mean_count_trans
       ) %>% select(-mean_category_amount,-mean_count_trans, -max_amount, -mean_amount, -count_trans) %>%
        top_n(-4)  # Use top_n(xx) to select the top xx rows, and top_n(-xx) for the bottom xx rows

per_agency_category[1:100,]

# Use "dcast" in Library "reshape2" to organize the data so each row is a merchant category of an agent.
library(reshape2)
per_agency_category$Year_Month <- paste("Y",per_agency_category$Year_Month,sep="_")
wide <- dcast(per_agency_category, Agency_Name + Merchant_Category ~ Year_Month)
wide=as.matrix(wide)
wide[is.na(wide)] <-0
wide=as.data.frame(wide)
head(wide)

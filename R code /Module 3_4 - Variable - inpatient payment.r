
#install.packages("zipcode")
library(dplyr)
#library(DataExplorer)
#library(xda)
library(ggplot2)
library(plotly)
library(data.table)
library(zipcode)
suppressPackageStartupMessages(library(tidyverse))
library(stringr)
#library(ggthemes)
suppressPackageStartupMessages(library(maps))

payment <- read.csv("/Users/chriskuo/Downloads/inpatientCharges.csv")

payment %>% arrange(Provider.Id, DRG.Definition) 
head(payment)

# Convert the average to numeric
p1 <- strsplit(x = as.character(payment$Average.Covered.Charges),split = "$",fixed = T)
payment$Average.Covered.Charges <- as.numeric(sapply(p1,"[[",2))
p1 <- strsplit(x = as.character(payment$Average.Total.Payments),split = "$",fixed = T)
payment$Average.Total.Payments <- as.numeric(sapply(p1,"[[",2))
p1 <- strsplit(x = as.character(payment$Average.Medicare.Payments),split = "$",fixed = T)
payment$Average.Medicare.Payments <- as.numeric(sapply(p1,"[[",2))
rm("p1")

#summary(payment)
colnames(payment)

DRG_State_avg <- payment %>% group_by(DRG.Definition, Provider.State) %>%
       summarise( mean_total_discharges          = mean(Total.Discharges),
                  mean_average_covered_charges   = mean(Average.Covered.Charges),
                  mean_average_total_payments    = mean(Average.Total.Payments),
                  mean_average_medicare_payments = mean(Average.Medicare.Payments),                 
                  mean_count_trans =n()
                )
head(DRG_State_avg)

# Append the average statistics back to the data to derive the ratios.
per_provider <- payment %>% 
       left_join(DRG_State_avg, by=c("DRG.Definition","Provider.State")) %>%
       mutate( 
            ratio_total_discharges          = Total.Discharges/mean_total_discharges,
            ratio_average_covered_charges   = Average.Covered.Charges/mean_average_covered_charges,
            ratio_average_total_payments    = Average.Total.Payments/mean_average_total_payments,
            ratio_average_medicare_payments = Average.Medicare.Payments/mean_average_medicare_payments) %>%
       select(-mean_total_discharges,
              -mean_average_covered_charges,
              -mean_average_total_payments,
              -mean_average_medicare_payments,
              -mean_count_trans,
              -Total.Discharges,
              -Average.Covered.Charges,
              -Average.Total.Payments,
              -Average.Medicare.Payments
           ) %>% arrange(Provider.Id, DRG.Definition) %>%
           select(Provider.Id,DRG.Definition,Provider.Name,Provider.State,ratio_total_discharges,ratio_average_covered_charges,ratio_average_total_payments,ratio_average_medicare_payments)
    
head(per_provider)

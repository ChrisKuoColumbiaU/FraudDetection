
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

summary(payment)

# Convert the average to numeric
p1 <- strsplit(x = as.character(payment$Average.Covered.Charges),split = "$",fixed = T)
payment$Average.Covered.Charges <- as.numeric(sapply(p1,"[[",2))
p1 <- strsplit(x = as.character(payment$Average.Total.Payments),split = "$",fixed = T)
payment$Average.Total.Payments <- as.numeric(sapply(p1,"[[",2))
p1 <- strsplit(x = as.character(payment$Average.Medicare.Payments),split = "$",fixed = T)
payment$Average.Medicare.Payments <- as.numeric(sapply(p1,"[[",2))
rm("p1")

summary(payment)
colnames(payment)

payment <- as.data.table(payment)
av1 <- payment[,Average.Covered.Charges,by=Provider.State]
ggplot(data = av1,mapping = aes(y = Provider.State,x = Average.Covered.Charges,colour=Average.Covered.Charges))+
  geom_point()

av1 <- payment[,Average.Total.Payments,by=Provider.State]

ggplot(data = av1,mapping = aes(y = Provider.State,x = Average.Total.Payments,colour=Average.Total.Payments))+
  geom_point()

V1 <- payment %>% # aggregate procedures for each hospital
  group_by(Provider.Id, Provider.Zip.Code, Provider.Name) %>% # keep zip & name
  summarise(procSum = sum(Total.Discharges) ) 

# merge aggregated hospital data with zipcode, copy lat+lon for each hospital
V2 <- merge(V1,zipcode, by.x= "Provider.Zip.Code", by.y= "zip")

g <- list( 
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  subunitwidth = 1,
  countrywidth = 1
)
V1 <- payment %>% # aggregate procedures for each hospital
  group_by(Provider.Id, Provider.Zip.Code, Provider.Name) %>% # keep zip & name
  summarise(procSum = sum(Total.Discharges) ) 

V2 <- merge(V1,zipcode, by.x= "Provider.Zip.Code", by.y= "zip")

head(V2)
g <- list( 
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  subunitwidth = 1,
  countrywidth = 1
)
k<-plot_ly(V2, lon = longitude, lat = latitude , 
        text = paste(Provider.Name, city,procSum, sep = "\n"),
        marker = list(size = sqrt(procSum/50) + 1, line = list(width = 0)),
        type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = 'aggregated procedure counts at US hospitals', geo = g)
setwd("/Users/chriskuo/Downloads")
embed_notebook(k)

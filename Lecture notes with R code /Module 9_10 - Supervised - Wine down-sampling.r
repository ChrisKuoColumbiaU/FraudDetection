
# Get the "Wine" dataset
#install.packages("rattle.data")
library(rattle.data)
head(wine)
table(wine$Type)

# Take only Type='1'
temp <-wine[wine$Type=='1',]
# Generate 1,000 row samples with replacement. Note if replace=False, it won't work.
temp2<-temp[sample(nrow(temp), 4000,replace=T), ]
dim(temp2)
# Put together
wine2<-rbind(wine[wine$Type=='2',],temp2)
# Shuffle the rows
wine2 <- wine2[sample(nrow(wine2)),]
dim(wine2)
# Change the target value to 1 and 0.
wine2$Type <-as.factor(ifelse(wine2$Type=='1','0','1'))
head(wine2)
#mean(as.numeric(wine2$Type))
table(wine2$Type)

group1<-wine2[wine2$Type=='1',]
group0 <-wine2[wine2$Type=='0',]
df2 <-group0[sample(nrow(group0), 800), ]
df<-rbind(df2,group1)
#mean(as.numeric(df$Type))
table(df$Type)

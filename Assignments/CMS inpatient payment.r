library(dplyr)
library(stringr)
library(maps)
library(ggplot2)

charges <- read.csv('/Users/chriskuo/downloads/Inpatient_Payment.csv')
print("Data size in RAM:")
print(object.size(charges), units = 'Mb')

print("Data.frame dimensions:")
print(dim(charges))

head(charges)

names(charges) <- c('drg_def', 'prov_id', 'prov_name', 'prov_address', 'prov_city', 'prov_state', 
                    'prov_zip', 'referral_reg', 'total_discharges', 'mean_covered_charges',
                    'mean_total_payments', 'mean_medicare_payments')


remove_dollar <- function(x) {
  as.numeric(str_replace(x, '\\$', ''))
}

charges <- charges %>% 
  mutate_at(vars(mean_covered_charges, mean_total_payments, mean_medicare_payments), 
            remove_dollar)
head(charges)


charges %>%
  group_by(drg_def) %>%
  summarize(n = n()) %>%
  arrange(-n) %>% # Use arrange to sort in descending order
  head(10)


# https://www.cms.gov/icd10manual/fullcode_cms/P0002.html
procDF <- charges %>% # aggregate  procedures from all hospitals
  group_by(drg_def) %>% 
  summarise(procSum = sum(total_discharges))


ggplot(data=procDF, aes(x=drg_def, y=procSum))

# geom_bar is designed to make it easy to create bar charts that show counts
g <- ggplot(procDF, aes(drg_def,procSum))
g + geom_bar(stat = "identity")


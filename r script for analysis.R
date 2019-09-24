rm(list = ls())

#load package
library(tidyverse)
library(tidyr)
library(broom)
library(purrr)


#setworking directory and load the data
setwd("C:/stata assignments/complete/interview questions/laterite data/Laterite Analytical Assessment")

data <- read_csv("laterite_education_data.csv", col_names = T,cols(
  X1 = col_double(),
  province = col_factor(),
  district = col_factor(),
  ur2012 = col_factor(),
  weight = col_double(),
  Consumption = col_double(),
  s1q1 = col_factor(),
  s1q3y = col_double(),
  s1q13 = col_factor(),
  s1q14 = col_factor(),
  s3q4 = col_factor(),
  s4aq6a = col_character(),
  s4aq6b = col_character(),
  s4aq8 = col_factor(),
  s4aq9 = col_factor(),
  s4aq11h = col_double(),
  s4aq12 = col_factor(),
  s4aq14 = col_double(),
  s4aq15 = col_factor(),
  s4aq17 = col_factor(),
  s4bq3 = col_factor(),
  s4bq4 = col_factor(),
  s4bq5 = col_factor(),
  s6aq2 = col_factor()
))

## rename the variables
names(data)
r_data <- data %>% rename(Sex = s1q1, 
                          Age=s1q3y, 
                          region_class = ur2012,
                          Father_alive=s1q13,
                          Mother_alive = s1q14, 
                          health_prob=s3q4,
                          Grade_2012=s4aq6a ,
                          Grade_2013=s4aq6b , 
                          sch_attended_prev_yr=s4aq8  , 
                          prob_in_sch=s4aq9,
                          edu_expenses=s4aq11h,
                          paid_edu_expenses_year_end=s4aq12 , 
                          sch_days_missed=s4aq14  , 
                          why_not_attending_sch=s4aq15 ,
                          why_leave_sch=s4aq17 ,
                          can_read=s4bq3 ,
                          can_write=s4bq4,
                          can_calculate=s4bq5 ,
                          farm_work=s6aq2  )


## Define the missing values as NAs then remove them from the two variables.

r_data <- r_data %>% na_if("") %>% filter(!is.na(Grade_2012),!is.na(Grade_2013))

#create dummy variables,i.e grade repeat and dropout outcome

r_data$repeated <- factor(r_data$Grade_2012==r_data$Grade_2013)
r_data$dropout <- factor(!(r_data$Grade_2012%in%c("Not in class"))&r_data$Grade_2013%in%c("Not in class"))


## how grade repetition varies by grade in Primary Education 
comparis <- r_data %>% filter(!(Grade_2012%in%c("Not in class")))
table(r_data$Grade_2012,r_data$repeated)->comparison_repetition_in_classes_2012
prop.table(comparison_repetition_in_classes_2012,1)*100 

ggplot(comparis, aes(x=repeated))+ geom_bar(position = "dodge")+facet_wrap(~Grade_2012)
ggplot(comparis, aes(x=Grade_2012,fill =repeated))+ geom_bar(position = "stack")

## dropout by gender

var.test(dropout_gender[1,],dropout_gender[2,])
t.test(dropout_gender[1,],dropout_gender[2,], var.equal = T)# since t.test  for which variance is same showes a p-value of 0.9765 using Welch two sample test, which is > 0.05

## variables contributing to repeat in primary
##our response variable would be repeating which is binary, so we recode into 1 if repeated is true and 0 if otherwise.
r_dara <- r_data %>% mutate(repeated = ifelse(repeated %in% c('TRUE'), 1L, 0L))
r

## we use a binary logistic model to get p-values

coefficients_of_vars_plus_pvalues <- r_dara %>%  group_by(region_class) %>% nest(-region_class) %>% mutate(models = map(data, ~lm(repeated~weight+Consumption+Sex+Age+Father_alive+Mother_alive+health_prob+sch_attended_prev_yr+prob_in_sch+Grade_2013+Grade_2012+paid_edu_expenses_year_end+why_not_attending_sch, .))) %>% mutate(tidied = map(models, tidy)) %>% unnest(tidied) %>% filter(region_class %in%c("Rural","Urban")) %>% select(-statistic) %>% mutate(p.adjusted = p.adjust(p.value)) 

#export the table
#write.csv(coefficients_of_vars_plus_pvalues,"all_p.values.csv",row.names = F)

## Get the significant variables i.e that have p-value < 0.5
coefficients_of_vars <- r_dara %>%  group_by(region_class) %>% nest(-region_class) %>% mutate(models = map(data, ~lm(repeated~weight+Consumption+Sex+Age+Father_alive+Mother_alive+health_prob+sch_attended_prev_yr+prob_in_sch+Grade_2013+Grade_2012+paid_edu_expenses_year_end+why_not_attending_sch, .))) %>% mutate(tidied = map(models, tidy)) %>% unnest(tidied) %>% filter(region_class %in%c("Rural","Urban")) %>% select(-statistic) %>% mutate(p.adjusted = p.adjust(p.value)) %>% filter(p.adjusted < .05)

#export the table
#write.csv(coefficients_of_vars,"significant_p.values at 0.5.csv",row.names=F)


# Sort for the variables the highest causes of repetition in class
causes <- coefficients_of_vars %>%  filter(region_class=="Rural",term != "(Intercept)") %>% arrange(desc(estimate))

#write.csv(causes,"arranged causes of repetition in rural schools.csv", row.names = F)

# Sort for the variables the highest causes of repetition in class
causes2 <- coefficients_of_vars %>%  filter(region_class=="Urban",term != "(Intercept)") %>% arrange(desc(estimate))

#write.csv(causes2,"arranged causes of repetition in urban schools.csv", row.names = F)

#drop out strength and weakness

r_dara <- r_data %>% mutate(dropout = ifelse(repeated %in% c('TRUE'), 1L, 0L))

coefficients_of_vars_for_drop_out <- r_dara %>%  group_by(region_class) %>% nest(-region_class) %>% mutate(models = map(data, ~lm(dropout~weight+Consumption+Sex+Age+Father_alive+Mother_alive+health_prob+sch_attended_prev_yr+prob_in_sch+Grade_2013+Grade_2012+paid_edu_expenses_year_end+why_not_attending_sch, .))) %>% mutate(tidied = map(models, tidy)) %>% unnest(tidied) %>% filter(region_class %in%c("Rural","Urban")) %>% select(-statistic) %>% mutate(p.adjusted = p.adjust(p.value)) %>% filter(p.adjusted < .05)

m <- lm(dropout~weight+Consumption+Sex+Age+Father_alive+Mother_alive+health_prob+sch_attended_prev_yr+prob_in_sch+Grade_2013+Grade_2012+paid_edu_expenses_year_end+why_not_attending_sch,r_daa)
par(mfrow = c(2, 2))
plot(m)


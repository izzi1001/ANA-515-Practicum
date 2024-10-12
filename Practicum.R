# Practicum 11.2024
# Mental Health in Tech Workers
# Iris Zhang

library(tidyverse)
library(dplyr)
library(stringr)

#Use read_csv to read the raw data in, which is from the tidyverse package
path<-"/Users/irisZ/Desktop/ANA515/Practicum"
setwd(path)
df1<-read_csv("Sheet1.csv")
df2<-read_csv("Sheet2.csv")

#take a quick look at the datasets
str(df1)
str(df2)

#combine the two datasets
df <- bind_rows(df1, df2)
str(df)
glimpse(df)

#we can already see some inconsistencies of in the categorical variables in 
# Gender, for example, sometimes it's spelled as "Male", and sometimes it's "M".
# We'll go in and clean those up. We'll also take a look at the numerical variables
# for anomalies. 

summary(df)

#clean up the column names to lower cases
df<-rename_all(df,tolower)

table(df$gender) 
unique(df$country) # country entries look OK
unique(df$state) #sometimes the States are spelled out which we'll correct for

# clean up the inconsistent naming conventions in state and gender
#For convenience, we group genders to "Male", "Female" and "Other"

df_clean<-df %>%
  mutate(state=if_else(state=="California","CA",if_else(state=="New York","NY",state))) %>%
  mutate(state=if_else(state=="Texas","TX", state))%>%
  mutate(gender=str_to_title(gender))%>%
  mutate(gender=if_else(grepl("Female|Femake|Woman|Femail", gender), "Female",
                          if_else(grepl("Male|Man|Mail|Malr|Msle", gender), "Male", "Other")))

          #mutate(gender=, gender))

#check the results
table(df$gender) #looks about right
unique(df_clean$state) #looks about right

#We want to take a look at the frequency tables for the rest of the categorical variables
categorical_vars <- sapply(df, function(col) is.factor(col) || is.character(col))
cat_columns<-names(df)[categorical_vars]
length(cat_columns)
cat_columns<-cat_columns[5:25] 

#Create tables for all categorical variables
for (col_name in cat_columns) {
  cat("Frequency table for", col_name, ":\n")
  print(table(df[[col_name]]))
  cat("\n")
}

#clean up the "N" and "Y" into "No" and "Yes" 
df_clean<-df_clean %>%
  mutate(across(cat_columns, ~str_to_title(.))) %>%  #converting all answers to title case
  mutate_at(vars(treatment, phys_health_consequence), ~ ifelse(. == "N", "No", .))%>%
  mutate_at(vars(treatment, phys_health_consequence), ~ ifelse(. == "Y", "Yes", .))%>%
  
  #grouping the "Not Sure" and "Don't Know" answers together
  mutate_at(vars(benefits, seek_help,anonymity), ~ ifelse(. == "Not Sure", "Don't Know", .))%>%
  
  #clean up remote_work
  mutate(remote_work = case_when(
      remote_work=="1" ~ "Yes",
      remote_work=="2" ~ "No", 
      TRUE ~ remote_work  # Default to keeping the original if no match
  )) %>%
 
  #looks like no_employees got confused as a date entry, we'll clean this up
  mutate(no_employees = case_when(
    str_detect(no_employees, "25-Jun|44372") ~"6-25",
    str_detect(no_employees, "5-Jan|44201") ~ "1-5", 
    TRUE ~ no_employees  # Default to keeping the original if no match
  ))
  
#check the results
for (col_name in cat_columns) {
  cat("Frequency table for", col_name, ":\n")
  print(table(df_clean[[col_name]]))
  cat("\n")
}

export the clean dataset to to csv
write_csv(df_clean, "Practicum Cleaned Dataset.csv")
#No getting back to the age as a numerical variable, there seems to be outliers

df_clean<-df_clean %>%
  filter(age >0 & age<=100) #using filter() for a change, but in practice, I may leave the outliers as NA
#check the result 
summary(df_clean$age)

#now we turn to some exploratory plots
ggplot(df_clean, mapping = aes (x=age))+
  #geom_point()
  geom_histogram(binwidth =1)

#now we turn to some exploratory plots
ggplot(df_clean)+
  geom_bar(aes(obs_consequence, fill=family_history))+
  facet_wrap(~gender)

ggplot(df_clean, mapping = aes (family_history, obs_consequence))+
  geom_point(aes(color = gender),position = "jitter") +
  facet_wrap( ~ no_employees)

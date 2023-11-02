#### Practice R Program and Data Cleaning, Manipulation, and Visualization


# Clean Session ------------------------------------------------------------#
# this command clears the Global Environment and functions 
# (also good to clear up space)

rm(list = ls())

# Set Up -------------------------------------------------------------------#

# To check your current working directory:
getwd()

# Set your working directory to your preferred folder
setwd("C:/Users/erin.mcdevitt/OneDrive - Rock Creek Group/research_personal/r_practice")
# e.g. setwd("C:/Users/erin.mcdevitt/OneDrive - Rock Creek Group/research_personal/r_practice")


# Install relevant packages
install.packages('tidyverse')

#Load Libraries

library(tidyverse)


# Import Data: read.csv() --------------------------------------------------#

# Read in the csv file

sca <- read.csv("survey.csv")

# Data Exploration: head(), glimpse(), filter() ----------------------------#

# Use head() to look at first few rows of data for sca

head(sca)

# Now try glimpse() to see the variables

glimpse(sca)

# Notice the differences between head() and glimpse and what you'd use them 
# for when looking at the data


# Now open up the data in a new tab using View()

View(sca)

######
# Let's take a subset of this data using filter() and select()
######

# Create a new dataframe filtered to the most recent survey (June 2023)

sca_subset <- sca %>% filter(YYYYMM == 202306) #to select all dates EXCEPT 202306, use YYYYMM != 202306

# Select the following variables: INCOME, AGE

sca_subset <- sca_subset %>% select(c(INCOME, AGE))

# Use ggplot to make a simple scatter plot
ggplot(data = sca_subset, 
       aes(x = AGE, y = INCOME)) +
  geom_point() +
  labs(x = "Age",
       y = "Income")

######
## Practice 
## Recreate the above steps in lines 54 - 68 using 
# YYYYMM = 202212
# Select the following variables: EDUC, SEX
######



# Data Manipulation: filter(), mutate(), rename(), ifelse() -----------------#

# Let's go back to our original dataset and make some changes. 

######

# F1) create a new data frame called sca_filtered; using filter() select December 2022 data

#Actual Answer - DELETE
sca_filtered <- sca %>% filter(YYYYMM == 202306)

######

# 2) select the following variables: 
# CASEID
# PAGO: Personal Finances One Year Ago
# PEXP: Personal Finances between next year
# BAGO: Economy Better/Worse Than One Year Ago
# BEXP: Economy Better/Worse Next Year
# BUS12: Economy Good/Bad Next Year 
# BUS5: Economy Good/Bad Next 5 Years
# DUR: Durables Buying Attitudes
# AGE: Age of Respondent
# INCOME: Income of Respondent
# SEX: Sex of Respondent


#Actual Answer - DELETE
sca_filtered <- sca_filtered %>% select(c(YYYYMM, CASEID, PAGO, PEXP, BUS12, BUS5, DUR, AGE, INCOME, SEX, EDUC, REGION))

######

# 3) change the names using rename()

#Example: 
#sca_filtered <- sca_filtered %>% rename(CASEID = id)

#Actual Answer - DELETE
sca_filtered <- sca_filtered %>% rename(
  date = YYYYMM,
  id = CASEID, 
  pers_finance1 = PAGO, 
  pers_finance2 = PEXP,
  bus_cond1 = BUS12, 
  bus_cond2 = BUS5,
  durable = DUR, 
  inc = INCOME, 
  sex = SEX, 
  educ = EDUC, 
  age = AGE, 
  region = REGION) 

#####

# 4) Use mutate() to recode the variable SEX so that 1 = "Male" and 2 = "Female"

#Actual Answer - DELETE
sca_filtered <- sca_filtered %>% mutate(sex = ifelse(sex == 1, "Male", "Female"))


#5) The pipe operator %>% combines steps 1-5 into a neat function 


pipe_example <- sca %>% filter(YYYYMM == 202306) %>%
  select(c(YYYYMM, CASEID, PAGO, PEXP, BUS12, BUS5, DUR, AGE, INCOME, SEX, EDUC)) %>%
  rename(
    date = YYYYMM,
    id = CASEID, 
    pers_finance1 = PAGO, 
    pers_finance2 = PEXP,
    bus_cond1 = BUS12, 
    bus_cond2 = BUS5,
    durable = DUR, 
    inc = INCOME,  
    sex = SEX, 
    educ = EDUC, 
    age = AGE) %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female"))
  

#####

# Data Explorations: summary(), mean(), median() ----------------------------#

#Summary() helps us understand our variables by providing stats on min, max, 1st Qtrl, median, mean, 3rd Qtrl
#of the data

summary(sca_filtered)

# Use a ggplot to show a histogram of income distribution & frequency
# Use geom_histogram and set the number of bins ("groups") bywhich you breakdown 
# income categories 
# Add x and y axes labels using labs()

ggplot(sca_filtered, aes(x = inc)) +
  geom_histogram(bins = 25) + #default bins is 30
  labs(x = "Income", 
       y = "Frequency", 
       title = "Income Distribution",
       caption = "Source: 2023 SCA.")



# Add in another dimension with "fill" to see breakdown by sex
# and rename the legend

ggplot(sca_filtered, aes(x = inc, fill = sex)) +
  geom_histogram(bins = 25) + #default bins is 30
  labs(x = "Income", 
       y = "Frequency", 
       fill = "Gender",
       caption = "Source: 2023 SCA.")
#####

## Practice 
## 1) Create a new dataset called "sca_sample" with the following filters:
##   inc >= 50,000
##   age >= 25 & age <= 55
##   Keep all non missing education values (hint: look up is.na)

## 2) Summarize the findings using summary()

## 3) Assign educ with the following values:
### educ in 1, 2, 3, or 4, then educ is "No College Degree"
### educ equals  or 6 then educ is "College Degree or higher"

## Bonus: how would you assign unique descriptors to each educ value?


## 4) Create a histogram of income distribution by education, change the bin # 


######

sca_sample <- sca_filtered %>% filter(inc >= 50000, age >= 25 & age <= 55, !is.na(educ)) 

summary(sca_sample)

#Note: $ is used to select a specific column within a data.frame

sca_sample <- sca_sample %>% mutate(
  educ = ifelse(educ < 5, "No College Degree", "College Degree or higher")
)  


ggplot(sca_sample, aes(x = inc, fill = educ)) +
  geom_histogram(bins = 25) + #default bins is 30
  labs(x = "Income", 
       y = "Frequency", 
       fill = "Education",
       caption = "Source: 2023 SCA.")



# Data Explorations: summarize() --------------------------------------------#


# Create a new dataset that only looks at male data. Can be done either with
# filter() and use mean() median() for summary statistics

male_data <- filter(sca_filtered, sex == "Male") 

# Some summary stats: mean() and median()

mean(male_data$inc, na.rm = T) #ignore NA values
median(male_data$inc, na.rm = T) #ignore NA values

# Can also use the summarize() funtion to calculate mean and median

male_data %>% 
  summarise(mean_inc = mean(inc, na.rm = T), 
            median_inc = median(inc, na.rm = T))

# Another way to get a good idea of the data is to use group_by()
# In the case below, we can group by education levels to create some
# interesting summary statistics

male_data %>% 
  group_by(educ) %>%
  summarise(mean_inc = mean(inc, na.rm = T), 
            median_inc = median(inc, na.rm = T))


######

## Practice 
## 1) Do the same manipulations above for female data frame




######

# Now - what if we wanted to assign more than two values to a variable like education?
# Ifelse statements can get messy very quickly, so we can use case_when as an alternative!
# Let's go back to the main sca_filtered data


#First, how many values are in the educ variable?

unique(sca_filtered$educ)


#Now, recod the numeric education level 

sca_filtered <- sca_filtered %>%
  mutate(educ = case_when(educ < 2 ~ "No HS Diploma", 
                          educ == 3 ~ "HS Diploma", 
                          educ == 4 ~ "Some College",
                          educ == 5 ~ "College Degree", 
                          educ == 6 ~ "College Degree or More")) 

######

## Practice
## 1) With sca_filtered dataframe, recode the variable for REGION:
##  1 West
##  2 North Central
##  3 Northeast
##  4 South


######

# Data Explorations: Visualization 1 ----------------------------------------#


#Create a new data frame with median income by education and sex

wage_income <- sca_filtered %>% filter(!is.na(educ)) %>%
  group_by(educ, sex) %>%
  summarise(mean_inc = mean(inc, na.rm = T), 
            median_inc = median(inc, na.rm = T))


#Make a bar plot of the data

plot_wage_inc <- wage_income %>%
  ggplot(aes(x=educ, y = median_inc, fill = sex)) +
  geom_col(position = "dodge") +
  labs(x = "Education",
        y = "Median Income",
        fill = "Gender") 

plot_wage_inc


#Adding some ggplot flair with scale_x_discrete for x axis

plot_wage_inc_x <- plot_wage_inc + 
  scale_x_discrete(labels = c("College Degree" = "College \nDegree",
                              "College Degree or More" = "College Degree \nor More",
                              "HS Diploma" = "HS \nDiploma",
                              "No HS Diploma" = "No HS \nDiploma",
                              "Some College" = "Some \nCollege"),
                   limits = c("No HS Diploma", "HS Diploma", "Some College", "College Degree", "College Degree or More")) #\n creates line breaks
  
plot_wage_inc_x


#scale_x_continuous for y axis, create a break from 0 to $200,000 by increments of $50,000

plot_wage_inc_y <- plot_wage_inc_x +
  scale_y_continuous(breaks = seq(0,150000, 50000)) +
  labs(title = "Median Income Distribution by Sex and Education")

plot_wage_inc_y

# What about breaking median income down by three groups?
# Use facet_wrap() and add in all previous steps to chart

# Let's break the chart down by age and sex, then create 
# mutliple outputs by education 
# Round_any will round age to the nearest 5


plot_facet <- sca_filtered %>% filter(!is.na(educ)) %>% 
  mutate(rounded_age = plyr::round_any(age, 5)) %>%
  group_by(rounded_age, sex, region) %>%
  summarise(median_inc = median(inc, na.rm = T)) %>%
  ggplot(aes(x = rounded_age, 
             y = median_inc, 
             fill = sex)) +
  geom_col(position = "dodge") +
  facet_wrap(~region) %>%
  labs(x = "Age", 
       y = "Median Income", 
       fill = "Gender", 
       title = "Median Income by Age, Sex, and Education", 
       caption = "Source: 2023 SCA") +
  scale_y_continuous(breaks = seq(0,500000, 50000)) +
  labs(title = "Median Income Distribution by Sex and Education")

plot_facet


# Data Explorations: Visualization 2 ----------------------------------------#

# Timeseries 

#Import the sca file "sca-timeseries.csv". This file already has many of the
# transformations included outline in previous examples

sca_timeseries <- read.csv("sca_timeseries.csv")

head(sca_timeseries)

#Change in avg. incomes using lag() variable and plot timeseries
 # - use lag() to create a change variable 
 # - create a new column that flags if the value shrunk or not
 # - make a column chart and use variable shrunk as color axis
 # - add geom_hline to create a y=0 to highlight where things shrunk
 # - add a nice theme and labels 

plot_inc_timesries <- sca_timeseries %>%
  group_by(year) %>%
  summarise(mean_inc = mean(inc, na.rm = T), 
            median_inc = median(inc, na.rm = T)) %>%
    mutate(change = 100 * (mean_inc/lag(mean_inc) - 1), 
           shrunk = ifelse(change < 0, "Yes", "No")) %>%
  ggplot(aes(x = year, y = change, fill = shrunk)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_bar(position = "stack", stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
  theme_minimal() +
  xlab('Year') +
  ylab('Percent Change') +
  labs(title = 'Change in Average Household Income over Time',
       caption = 'University of Michigan Survey of Consumers') +
  scale_y_continuous(breaks = seq(-5, 20, 2.5))
  

plot_inc_timesries  



#Import the sca file "sca-timeseries.csv" and make the following manupulations
# Create a new year column from date 
# Create a median mean income variable 
# Keep the variables year, mean income
# Create a lag variable that calculates the percent change in mean income over time
# Flag if income shrunk or not with a new column
# Filter data from 2000 to present

sca_timeseries <- read.csv("survey_timeseries.csv") %>%
  mutate(year = as.numeric(substr(YYYYMM, 1,4))) %>%
  group_by(year) %>%
  summarise(mean_inc = mean(INCOME, na.rm = T), 
            median_inc = median(INCOME, na.rm = T)) %>%
  mutate(change = 100 * (mean_inc/lag(mean_inc) - 1), 
         shrunk = ifelse(change < 0, "Yes", "No")) %>%
  filter(year >= "2000")

head(sca_timeseries)


#Change in avg. incomes using lag() variable and plot timeseries
# - use lag() to create a change variable 
# - create a new column that flags if the value shrunk or not
# - make a column chart and use variable shrunk as color axis
# - add geom_hline to create a y=0 to highlight where things shrunk
# - add a nice theme and labels 

plot_inc_timeseries <- sca_timeseries %>%
  ggplot(aes(x = year, y = change, fill = shrunk)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_bar(position = "stack", stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
  theme_minimal() +
  xlab('Year') +
  ylab('Percent Change') +
  labs(title = 'Change in Average Household Income over Time',
       caption = 'University of Michigan Survey of Consumers') +
  scale_y_continuous(breaks = seq(-5, 20, 2.5)) +
  scale_x_continuous(breaks = seq(2000, 2023, 2))


plot_inc_timeseries  





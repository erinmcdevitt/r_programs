#### Practice R Program for Data Analysis


# This example program walks through how to analyze the SCA data and 
# use regression analysis to examine the relationship a respondent's 
# sentiment score and demographic data

#################################################################################################################################
# SET UP                                                                                           
#################################################################################################################################

# Clean Session -----------------------------------------------------------------------------------------------------------------#

rm(list = ls())

# Set Working Directory ---------------------------------------------------------------------------------------------------------#

# to see current directory

getwd()

# Assign Working Directory - example below
setwd()

setwd("C:/Users/username/data/")


# Install packages and load libraries -------------------------------------------------------------------------------------------#

#Default packages can be found under the packages tab to the right side of the R window. 

# Install - comment out if already installed
install.packages("stargazer")
install.packages('tidyverse')
install.packages('broom')

# Load
library(tidyverse)
library(stargazer)
library(broom)


#################################################################################################################################
# Data Import and Basic Functions for Data Exploration                                                                                  
#################################################################################################################################


# Import Data: read.csv() -------------------------------------------------------------------------------------------------------#


survey <- read.csv("sca_data.csv") #reading in excel files requires the xlsx package used later in this program


# Data Exploration: head(), glimpse(), filter() ---------------------------------------------------------------------------------#

#head() shows the first n rows of data in the data.frame

head(survey)

#glimpse() shows all variables in the data and the data type

glimpse(survey)

#View() opens the data frame in a new tab as a spreadsheet style view

View(survey)


# Data Exploration: filter() and select() --------------------------------------------------------------------------------------#

# Filter() uses operators to keep observations that follow within the parameters. 
# Example: survey_date is a subset of the survey data.frame where YYYYMM (our date variable) is the most recent date. 

most_recent <- survey %>%
  filter(YYYYMM == "202306")

# Alternatively, you can do filter(date == max(date)) if you don't know the last date in the data.frame
# NOTE: character variables must use "" or '' around the value to be read in R


# Select() uses c() to keep (or remove) certain variables from the data.frame
# Example: survey_subset includes only columns YYYYMM, INCOME, ICS, and CASEID. 

survey_subset <- survey %>%
  select(c(YYYYMM, INCOME, ICS, CASEID))

#NOTE: In R, c() means 'combine' and is used to give parameters inside a function. In this case, the function is select()

# Combining functions uses the %>% (pipe) which passes the output of one function into the next as an argument

most_recent_subset <- survey %>%
  filter(YYYYMM == "202306") %>%
  select(c(YYYYMM, INCOME, ICS, CASEID))

#NOTE: At any point the command ?[function] (e.g. ?filter()) in the console will open the Help tab on the right hand side. 


#################################################################################################################################
# Creating a Dataset for Analysis                                                                                    
#################################################################################################################################


# There are many ways of looking at SCA (or any) data, but for simplicity we'll focus on exploring the relationship  
# between ICS (Index of Consumer Sentiment) and the following variables of interest:
  # 1) Age
  # 2) Education
  # 3) Income
  # 4) Sex/Gender

# Data Manipulation: filter(), mutate(), rename()  ----------------------------------------------------------------------------#

# New data.frame that includes our chosen variables and additional parameters, including
# 1) Time: January 2010 to June 2023
# 2) Age: 25 to 65 years old

# Uses mutate() to create new date variable and rename() to recode variables 
# Uses rename and select to get final dataset 

survey_clean <- survey %>%
  filter(YYYYMM >= "201001", 
         AGE <= 65, 
         AGE >= 25) %>%
  mutate(year = as.numeric(substr(YYYYMM, 1,4)), 
         month = as.numeric(substr(YYYYMM, 5, 6)), 
         date = as.Date(as.yearmon(paste(year, month), "%Y %m"), frac = 1)) %>%  # frac is fraction of month. Default 0 (first day).
  rename(ics = ICS, #if you want to keep the variables the same but change case, use rename_with(tolower). 
         age = AGE, 
         educ = EDUC, 
         inc = INCOME, 
         sex = SEX, 
         wt = WT) %>% 
  select(c(date, year, ics, age, educ, inc, sex, wt))


write.csv(survey_clean, "sca_data_clean.csv")



# Data Manipulation: Summary Statistics  ---------------------------------------------------------------------------------------#

# Stargazer(): creates summary statistics of the data. Using a - in front of a c() parameter excludes the selected variables. 

stargazer(survey_clean %>% select(-c(date, year)), title = "Table 1. Summary Statistics", type = "text")

# Summary(): less refined sibling to Stargazer() but includes quantiles 

summary(survey_clean)

# What seems off about the data?

#Unique(), min(), max(): To see various cuts of a variable's values 
# NOTE: the $ operator is used to extract or subset a specific part of a data object in R (e.g. a data frame or list)
# Example: the unique values of inc, largest and smallest values of inc. The min and max values are both missing.

unique(survey_clean$inc)

max(survey_clean$inc)

min(survey_clean$inc)

# na.omit(): Remove any missing values row-wise for each observation. 

survey_clean <- survey_clean %>%
  na.omit()


# Summarise() - create statistical outputs for groupings 

survey_summ <- survey_clean %>%
  group_by(date, sex, educ) %>%
  summarise(median_ics = median(ics, na.rm = T), 
            mean_ics = mean(ics, na.rm = T), 
            obs = n())



# Data Visualization: ggplot ---------------------------------------------------------------------------------------------------#

# ifelse() and casewhen() are conditional parameters within mutate to recode values within a variable
survey_ggplot <- survey_clean %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female"))
  


# lead() and lag() are used to find changes. 
# Example: Change in Average ACS over Time using geom_bar

survey_change <- survey_clean %>%
  group_by(year) %>%
  summarise(median_ics = median(ics, na.rm = T)) %>%
  mutate(median_ics_chng = 100 * (median_ics/lag(median_ics) - 1), 
         shrunk = ifelse(median_ics_chng > 0, "Yes", "No")) %>%
  ggplot(aes(x = year, y = median_ics_chng, fill = shrunk)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_bar(position = "stack", stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
  theme_classic() +
  xlab('Year') +
  ylab('Percent Change') +
  labs(title = 'Change in Average ICS over Time',
       caption = 'University of Michigan Survey of Consumers') +
  scale_x_continuous(breaks = seq(2011, 2023, 1)) +
  scale_y_continuous(limits = c(-50, 30, 5))

survey_change

#geom_histogram: shows distribution/frequency of one variable
# Example: Distribution of Income 

ggplot(survey_ggplot, aes(x = inc)) +
  geom_histogram(bins = 25) + #default bins is 30
  labs(x = "Income", 
       y = "Frequency", 
       title = "Income Distribution",
       caption = "Source: University of Michigan Survey of Consumers.")

#geom_density: a smoothed version of a histogram 

ggplot(survey_ggplot, aes(x = inc)) +
  geom_density(alpha = 0.5) +
  labs(x = 'Income', 
       y = 'Density',
       fill = 'Sex',
       color = 'Sex',
       title = 'Income Distrubtion by Sex',
       caption = 'Source: University of Michigan Survey of Consumers.') 

#geom_point: scatter plot of relationship between two variables

ggplot(survey_clean, aes(x = age, y = inc)) +
  geom_point() + #default bins is 30
  labs(x = "Age", 
       y = "Income", 
       title = "Income Distribution by Age",
       caption = "Source: University of Michigan Survey of Consumers.")

#Question: why isn't a scatterplot good for this data? How can we fixed it?

#####

#How else can we make geom_ plots look pretty?
# Add colors, scales, themes, additional dimensions
# theme(), scale_color_manual(), scale_fill_manual(), scale_x_continuous, scale_y_continuous

#Example: Update geom_density to show income by sex 

ggplot(survey_ggplot, 
       aes(x = inc, fill = sex, color = sex)) +
  geom_density(alpha = 0.5) +
  scale_color_manual(values = c("Female" = "dodgerblue", "Male" = "orange")) +
  scale_fill_manual(values = c("Female" = "dodgerblue", "Male" = "orange")) +
  theme_classic() +
  labs(x = 'Income', 
       y = 'Density',
       fill = 'Sex',
       color = 'Sex',
       title = 'Income Distrubtion by Sex',
       caption = 'Source: University of Michigan Survey of Consumers.') +
  scale_x_continuous(breaks = seq(50000, 500000, by = 50000),
                     expand = c(0.03,0)) +
  scale_y_continuous(expand = c(0,0))


# Another example of a ggplot that uses facet_wrap() to show multiple groupings 
# and how you can save the plot as an object

survey_summ_plot <- survey_clean %>%
    mutate(educ = case_when(educ <= 3 ~ "High School or less", 
                          educ == 4 ~ "Some College", 
                          educ >4 ~ "College Degree"), 
         sex = ifelse(sex ==1, "Male", "Female")) %>%
  group_by(year, sex, educ) %>%
  summarise(median_ics = median(ics, na.rm = T)) %>%

  ggplot(aes(x = year, y = median_ics, color = sex, linetype = sex)) +
  geom_line() +
scale_color_manual(values = c("Female" = "dodgerblue", "Male" = "orange")) +
  facet_wrap("educ", nrow = 3) +
  theme_classic() +
  labs(x = 'Year', 
       y = 'ICS',
       color = 'Sex',
       linetype = 'Sex',
       title = 'Change in ICS over Time, by Sex and Education',
       caption = 'Source: University of Michigan Survey of Consumers.') +
  scale_x_continuous(breaks = seq(2010, 2023, 1), expand = c(0,0)) +
  scale_y_continuous(limits = c(40, 130), 
                     breaks = seq(40, 130, 10))

survey_summ_plot


# Data Visualization: advanced ggplot ------------------------------------------------------------------------------------------#

# This is an example of how to produce charts in the RockCreek Style 

# Set colors: 

color <- "grey30"
  
GREENBLUE <-  "#80BBCB"
PEACH <- "#FF9E80"


# Chart Themes: Since the same style is applied to all plots (and the changes are rather long), theme() is stored in an object to be called

family <- "roboto" # the font family 

plot_theme <- 
  plot_theme <- theme(
    #grid size
    plot.margin = margin(10, 10, 10, 10),
    #text
    plot.title = element_text(hjust = 0.5, color= color, size = 14, family = family),
    plot.subtitle = element_text(hjust = 1, color= color, size = 12, family = family),
    plot.caption = element_text(hjust = 0, color= color, size = 12, family = family),
    #legend
    legend.position = "bottom",
    legend.justification = "center",
    legend.title = element_blank(),
    legend.key.size = unit(0.5, 'cm'),
    legend.text = element_text(size = 14, color= color),
    legend.key = element_blank(),
    #plot background
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    #panel.grid.major.y = element_blank()),
    #both axes
    axis.title = element_blank(),
    #x axis
    axis.text.x = element_text(hjust = 0.5, size = 12),
    axis.ticks.length.x = unit(-2, "mm"),
    axis.line.x.bottom = element_line(color= color),
    #y axis - all
    axis.ticks.y = element_line(color = color),
    axis.ticks.x = element_line(color = color),
    #y axis - left
    axis.text.y.left = element_blank(),
    axis.line.y.left = element_line(color= color),
    axis.ticks.length.y.left = unit(-2, "mm"), 
    #y axis right
    axis.text.y.right = element_text(size = 12, color= color, family = family),
    axis.line.y.right = element_line(color= color),
    axis.ticks.length.y.right = unit(-2, "mm"),   
  )



# The actual plot

plot_rcg_style <- survey_clean %>% 
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  group_by(year, sex) %>%
  summarise(mean_ics = mean(ics, na.rm = T)) %>% 
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x=year, y=mean_ics, fill = sex, group = sex), position = "dodge", stat = "identity", alpha = 0.7, width = 0.8) +
  
  scale_fill_manual(breaks = c("Male", "Female"), 
                    values = c("Male" = GREENBLUE, 
                               "Female" = PEACH))  +
  plot_theme +
  #sometimes if it doesn't make sense to have a certain theme() element, adjustments can be made adding in theme() after plot_theme
  theme(axis.ticks.length.x = unit(0, "mm")) +
  labs(
    title = 'Change in ICS Score over Time, by Sex\n',
    subtitle = 'Percent (%)',
    caption = '\nSource: University of Michigan Survey of Consumers.'
  ) +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023), 
                     labels = c(2010, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23), 
                     expand = c(0.02, 0)) +
  scale_y_continuous(sec.axis = dup_axis(),
                     limits = c(0, 200),
                     breaks = seq(20, 200, by =20),
                     expand = c(0, 0))
  
  
plot_rcg_style  


#################################################################################################################################
# Regression Analysis                                                                                    
#################################################################################################################################

# Taking survey and creating a data set that looks at most recent time period (June 2023) and ics, age, educ, inc, sex, and wt


survey_2023 <- survey %>%
  filter(YYYYMM == "202306") %>%
  # frac is fraction of month. Default 0 (first day).
  rename(ics = ICS, #if you want to keep the variables the same but change case, use rename_with(tolower). 
         age = AGE, 
         educ = EDUC, 
         inc = INCOME, 
         sex = SEX, 
         wt = WT) %>% 
  select(c(ics, age, educ, inc, sex, wt)) %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female"), 
         educ = case_when(educ <=3 ~ "High School or less",
                          educ == 4 ~ "Some College",
                          educ >4 ~ "College Degree")) %>%
  na.omit()


stargazer(survey_2023, header = FALSE, rownames = FALSE,
          title = "Table 2. Summary Statistics, June 2023", type = "text")

# Basic OLS Regression ---------------------------------------------------------------------------------------------------------#

# Baseline model where ICS score is a function of a respondent's income
# ICS = β0 + β1Income_i
# β0 : error term
# β1 : coefficient

# lm() : R's function for the basic regression model

baseline_model <- lm(ics ~ inc, data = survey_2023)

stargazer(baseline_model, title = "Table 3. Baseline Model Regression Results",
          header = F, dep.var.caption = "",
          omit.stat = c("ser", "f"),
          no.space = T, 
          type = "text")


# Adding weights to make data more representative of the population

baseline_model_wt <- lm(ics ~ inc, weights = wt, data = survey_2023)

stargazer(baseline_model_wt, title = "Table 4. Baseline Model Regression Results With Weights",
          header = F, dep.var.caption = "",
          omit.stat = c("ser", "f"),
          no.space = T, 
          type = "text")


# The Broom Package for Interpreting Results -----------------------------------------------------------------------------------#

tidy(baseline_model)

glance(baseline_model)

baseline_model_augmented <- augment(baseline_model, survey_2023)

# Improving the model: Boxplot visuals 

ggplot(baseline_model_augmented) +
  geom_boxplot(aes(x = sex, y = .resid), show.legend = FALSE) +
  ylim(c(-10, 10)) +
  labs(x = "Residual", 
       y = "Density", 
       title = "Distribution Residuals by Sex", 
       color = "") +
  theme_minimal()


# Improving the model: Dummy variables (0,1)

baseline_model_augmented %>%
  ggplot(aes(x = inc, y = .fitted, color = sex)) +
  geom_point(alpha = 0.5, show.legend = FALSE)

lm(ics ~ inc + sex,
   survey_2023) %>%
  augment() %>%
  ggplot(aes(x = inc, y = .fitted, color = sex)) +
  geom_point(alpha = 0.5)


#Improving the model: New regression with dummy variable
# ICS = β0 + β1Income_i + β2sex_i 

survey_2023 <- survey_2023 %>%
  mutate(sex = ifelse(sex == "Male", 1, 0))

improved_model <- lm(ics ~ inc + sex,
                     survey_2023)

  
stargazer(baseline_model, improved_model, 
          title = "Table 5. Model Comparison",
          header = F, dep.var.caption = "",
          omit.stat = c("ser","f"),
          no.space = T, 
          type = "text")



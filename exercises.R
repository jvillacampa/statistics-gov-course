# Code from day one's exercises of the course on extracting and visualising data
# from statistiscs.gov.scot
# Extracting data manually from the platform, tidying and plotting it in R

###############################################.
## Packages ----
###############################################.
library(readr) # for reading csvs
library(ggplot2) # for plotting
library(dplyr)
library(tidyr)
library(janitor) # to tidy variable names

###############################################.
## Exercise 1 ----
###############################################.
# pop estimates data extracted manually from website
pop_data <- read_csv("data/population-estimates-current-geographic-boundaries.csv") %>% 
  gather("year", "pop", 3:9) %>%  # moving years from column to rows
  clean_names() %>% #tidying variable names
  filter(reference_area == "Scotland") %>% #selecting only Scotland
  mutate(year = as.numeric(year)) # year as numeric

## Plotting data
ggplot(pop_data, aes(x=year, y=pop))+
  geom_line()+
  geom_point()+
  theme_bw()+
  labs(title= "Scotland's population", x= "Year", y = "Population")

###############################################.
## Exercise 2 ----
###############################################.
# pop and dwelling data for 2017 for council areas extracted manually
# using data cart feature from web
housing_pop_data <- read_csv("data/exercise2_data.csv") %>% 
  clean_names() %>% #tidying variable names
  rename(pop17 = 3, dwelling17 = 4)

## Plotting data
ggplot(housing_pop_data, aes(x=pop17, y=dwelling17, group=feature_name))+
  geom_point()+
  theme_bw()+
  labs(title= "Scotland's population against number of dwellings", 
       x= "Population", y = "Dwellings")

###############################################.
## Exercise 3 ----
###############################################.
# extracting alcohol stays data with read_csv copying link direction from entire
# dataset download from the website
alcohol_data <- read_csv("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Falcohol-related-discharge") %>% 
  clean_names() %>% #tidying variable names
  # selecting scotland and number of discharges
  filter(feature_code == "S92000003" & units == "Hospital Discharges") 

## Plotting data
ggplot(alcohol_data, aes(x=date_code, y=value, group = feature_code))+
  geom_line()+
  geom_point()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90)) + # horizontal axis labels
  labs(title= "Alcohol-related hospital discharges, Scotland", 
       x= "Year", y = "Number of hospital stays")


##END
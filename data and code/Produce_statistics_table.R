library(data.table)
library(ggplot2)
library(lfe)
library(stargazer)

load('Jiang2020_Bunching_synthetic_data.rda')

pkgs <- c("dplyr", "tidyr", "broom")
sapply(pkgs, require, character.only = T) #load 

data

sumstat <- data %>%
  
  # Select and rename five variables 
  select(
    `Monthly payment` = payment,
    `Interest rate` = int,
    `Borrower credit ccore` = vantage,
    `Total loan amount ($1000)` = highcredit,
    `Length of loan` = terms,
    `Black population in borrower residential area (%)` = black,
    `Hispanic population in borrower residential area (%)` = hispanic,
    `Dealer reservation price` = min_p,
    `Borrower reservation price` = max_p,
  ) %>%
  
  # Find the mean, st. dev., min, and max for each variable 
  summarise_each(funs(mean, sd, min, max)) %>%
  
  # Move summary stats to columns
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  
  # Set order of summary statistics 
  select(variable, mean, sd, min, max) %>%
  
  # Round all numeric variables to one decimal point
  mutate_each(funs(round(., 2)), -variable)

sumstat

# Write to .txt
write.table(sumstat, file = "sumstats.txt", sep = ",", quote = FALSE, row.names = F)


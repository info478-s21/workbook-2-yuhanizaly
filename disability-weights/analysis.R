# Exploring Disability Weights

# Set up -- make sure to set your working directory
library(dplyr)
library(tidyr)
library(ggplot2)

# Load global data (disease burden in 2015, both sexes, all ages)
global_data <- read.csv(
  "./data/prepped/global_burden.csv",
  stringsAsFactors = FALSE
)

# Replace NA as 0 for deaths, ylls, ylds
global_data[is.na(global_data)] <- 0

# What disease was responsible for the most burden (for each metric)?
# Store the name of each *cause* in a variable
most_death <- global_data %>%
  filter(deaths == max(deaths)) %>%
  pull(cause)

most_ylls <- global_data %>%
  filter(ylls == max(ylls)) %>%
  pull(cause)

most_ylds <- global_data %>%
  filter(ylds == max(ylds)) %>%
  pull(cause)

most_dalys <- global_data %>%
  filter(dalys == max(dalys)) %>%
  pull(cause)

# Using prevalence and YLDs, calculate inferred disability weights
# Note: these are not actual weights used in the study

global_data <- global_data %>%
  mutate(dw_computed = ylds / prevalence)

# Identify any "unreasonable" values and replace them as NA

global_data$dw_computed[global_data$dw_computed > 1] <- NA

# Create a histogram of the disability weights
ggplot(global_data) +geom_histogram(mapping = aes(x = dw_computed))


# What are the ten highest disability weights? Store these in a variable
top_10_dw <- global_data %>%
  top_n(10, dw_computed) %>%
  select(cause, dw_computed)

# Which diseases have more YLDs than YLLs (and ylls > 0)?
more_disabling <- global_data %>%
  filter(ylds > ylls, ylls > 0)

# How many times higher is the prevalence than the number of deaths?
# Show the ratio of prevalence to deaths (for these diseases) in a histogram

higher_prevalence <- more_disabling %>%
  mutate(prev_death_ratio = prevalence / deaths)
  
  
higher_prevalence <- ggplot(higher_prevalence) +
  geom_histogram(mapping = aes(prev_death_ratio))


# Which disease has the most similar burden of YLLs and YLDs (where ylls > 0)?
most_similar <- global_data %>%
  mutate(yll_yld_diff = abs(ylls - ylds)) %>%
  filter(ylls > 0) %>%
  filter(yll_yld_diff == min(yll_yld_diff)) %>%
  pull(cause)

# For each cause, compute how many cases would have to have to be avoided
# to equal 65 YLLs (the equivalent of one death of a ~25 year old)

global_data <- global_data %>%
  mutate(num_cases_avoided = 65 / dw_computed)


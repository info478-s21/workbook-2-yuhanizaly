#Yuhaniz Aly
#Analysis.R file about Columbia Health Loss 

library(dplyr)
library(tidyr)
library(ggplot2)

#load data
colombia_data <- read.csv('data/prepped/overview.csv')

### Burden by Deaths ###
# Compute top 10 highest cause of deaths
top_10_deaths <- colombia_data %>%
  filter(metric =="deaths") %>%
  mutate(percent = Value * 100) %>%
  arrange(-percent) %>%
  top_n(10, percent)

# Barchart for top 10 highest causes of deaths
top_10_deaths_plot <- ggplot(top_10_deaths) +
  geom_col(mapping = aes(x = percent, y = reorder(cause, percent)
  ), fill = "red") +
  labs(
    title = "Top 10 Causes of Deaths",
    x = "Percentage of Deaths",
    y = "Causes"
  )

### Burden by YLLs ###
# Compute top 10 highest causes of YLLs
top_10_cause_ylls <- colombia_data %>%
  filter(metric =="ylls") %>%
  mutate(percent = Value * 100) %>%
  arrange(-percent) %>%
  top_n(10, percent)

# Barchart for top 10 highest causes of ylls
top_10_ylls_plot <- ggplot(top_10_cause_ylls) +
  geom_col(mapping = aes(x = percent, y = reorder(cause, percent)
  ), fill = "blue") +
  labs(
    title = "Top 10 Causes of Years of Life Lost (YLLs)",
    x = "Percentage of YLLs",
    y = "Causes"
  )

### Burden by YLDs ###
# Compute top 10 highest causes of YLDs
top_10_cause_ylds <- colombia_data %>%
  filter(metric =="ylds") %>%
  mutate(percent = Value * 100) %>%
  arrange(-percent) %>%
  top_n(10, percent)

# Barchart for top 10 highest causes of ylds
top_10_ylds_plot <- ggplot(top_10_cause_ylds) +
  geom_col(mapping = aes(x = percent, y = reorder(cause, percent)
  ), fill = "green") +
  labs(
    title = "Top 10 Causes of Years Lived with Disability (YLDs)",
    x = "Percentage of YLDs",
    y = "Causes"
  )

### Burden by DALYs ###
# Compute top 10 highest causes of DALYs
top_10_cause_dalys <- colombia_data %>%
  filter(metric =="dalys") %>%
  mutate(percent = Value * 100) %>%
  arrange(-percent) %>%
  top_n(10, percent)

# Barchart for top 10 highest causes of dalys
top_10_dalys_plot <- ggplot(top_10_cause_dalys) +
  geom_col(mapping = aes(x = percent, y = reorder(cause, percent)
  ), fill = "orange") +
  labs(
    title = "Top 10 Causes of Disability Adjusted Life Years (DALYs)",
    x = "Percentage of DALYs",
    y = "Causes"
  )

# Comparison of burden across YLLs to YLDs
comparison <- colombia_data %>%
  filter(cause == "Asthma") %>%
  mutate(percent = Value * 100) %>% 
         
# Barchart for comparison
asthma_comparison <- ggplot(comparison) +
  geom_col(mapping = aes(x = percent, y = metric)) +
  labs(
    title = "Asthma Comparison across different health metrics ",
    x = "Value (%)",
    y = "Health Metric"
  )





         

      
  


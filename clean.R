
# About this script -------------------------------------------------------

# Purpose: Clean/Filter datasets and generate plots for Shiny app
# Author: Yesle Soh
# Last updated: 21 May, 2024
# Contact: yesle.soh@stud.th-deg.de

# Clean and filter data ---------------------------------------------------

library(here)
library(dplyr)
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)

data_age <- read.csv(here("VDH-COVID-19-PublicUseDataset-Cases_By-Age-Group_20240119.csv"))
data_district <- read.csv(here("VDH-COVID-19-PublicUseDataset-Cases_By-District-Death-Hospitalization_20240119.csv"))

# Filter by district_age group Jan.16, 2024 -------------------------------

filter_data_by_district_age <- function(data, district_name, date) {
  data %>%
    clean_names() %>%
    mutate(report_date = mdy(report_date)) %>%
    filter(health_district == district_name &
             report_date == date &
             age_group %in% c("0-9 Years", "10-19 Years", "20-29 Years", "30-39 Years", "40-49 Years", "50-59 Years", "60-69 Years", "70-79 Years", "80+ Years")) %>%
    mutate(age_group = str_replace(age_group, " Years", "")) %>%
    select(report_date, health_district, age_group, number_of_cases, number_of_hospitalizations, number_of_deaths)
}

# Define the report date for January 16, 2024
report_date_jan2024 <- mdy("01/16/2024")

# Filter data for each district and age group
alexandria_Jan2024 <- filter_data_by_district_age(data_age, "Alexandria", report_date_jan2024)
arlington_Jan2024 <- filter_data_by_district_age(data_age, "Arlington", report_date_jan2024)
fairfax_Jan2024 <- filter_data_by_district_age(data_age, "Fairfax", report_date_jan2024)
loudoun_Jan2024 <- filter_data_by_district_age(data_age, "Loudoun", report_date_jan2024)
prince_william_Jan2024 <- filter_data_by_district_age(data_age, "Prince William", report_date_jan2024)


# Data manipulation and plots ---------------------------------------------

library(ggplot2)
library(plotly)
library(ggrepel)

# Function to simplify and capitalize variable names
simplify_variable_names <- function(variable) {
  variable <- str_replace(variable, "number_of_", "")  # Remove "number_of_"
  variable <- str_replace_all(variable, "_", " ")     # Replace underscores with spaces
  str_to_title(variable)  # Convert to title case
}

# Alexandria by age group_1.16.24 -----------------------------------------
# Reshape the data for plotting
alexandria_Jan2024_long <- alexandria_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count") %>%
  mutate(variable = simplify_variable_names(variable))

# Reorder the levels of the variable factor
alexandria_Jan2024_long$variable <- factor(alexandria_Jan2024_long$variable,
                                           levels = c("Cases", "Hospitalizations", "Deaths"))

# Arlington by age group_1.16.24 ------------------------------------------
# Reshape the data for plotting
arlington_Jan2024_long <- arlington_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count") %>%
  mutate(variable = simplify_variable_names(variable))

# Reorder the levels of the variable factor
arlington_Jan2024_long$variable <- factor(arlington_Jan2024_long$variable,
                                          levels = c("Cases", "Hospitalizations", "Deaths"))

# Fairfax by age group_1.16.24 --------------------------------------------
# Reshape the data for plotting
fairfax_Jan2024_long <- fairfax_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count") %>%
  mutate(variable = simplify_variable_names(variable))

# Reorder the levels of the variable factor
fairfax_Jan2024_long$variable <- factor(fairfax_Jan2024_long$variable,
                                        levels = c("Cases", "Hospitalizations", "Deaths"))

# Loudoun by age group_1.16.24 --------------------------------------------
# Reshape the data for plotting
loudoun_Jan2024_long <- loudoun_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count") %>%
  mutate(variable = simplify_variable_names(variable))

# Reorder the levels of the variable factor
loudoun_Jan2024_long$variable <- factor(loudoun_Jan2024_long$variable,
                                        levels = c("Cases", "Hospitalizations", "Deaths"))

# PW by age group_1.16.24 -------------------------------------------------
# Reshape the data for plotting
pw_Jan2024_long <- prince_william_Jan2024 %>%
  pivot_longer(cols = c(number_of_cases, number_of_hospitalizations, number_of_deaths),
               names_to = "variable",
               values_to = "count") %>%
  mutate(variable = simplify_variable_names(variable))

# Reorder the levels of the variable factor
pw_Jan2024_long$variable <- factor(pw_Jan2024_long$variable,
                                   levels = c("Cases", "Hospitalizations", "Deaths"))

# Combine plots into one page with dropdown menu --------------------------
combined_data <- bind_rows(
  mutate(alexandria_Jan2024_long, district = "Alexandria"),
  mutate(arlington_Jan2024_long, district = "Arlington"),
  mutate(fairfax_Jan2024_long, district = "Fairfax"),
  mutate(loudoun_Jan2024_long, district = "Loudoun"),
  mutate(pw_Jan2024_long, district = "Prince William")
)

# Function to generate plotly plots ---------------------------------------
generate_plot <- function(data, title) {
  p <- ggplot(data, aes(x = age_group, y = count, fill = variable, text = paste("Count:", count))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_log10() +  # Log scale for y-axis
    labs(title = title,
         x = "Age Group",
         y = "Count (log scale)",
         fill = "Variable") +
    scale_fill_manual(values = c("Cases" = "blue", 
                                 "Hospitalizations" = "orange", 
                                 "Deaths" = "red")) +  # Custom fill colors
    theme_minimal()
  return(ggplotly(p, tooltip = "text"))
}

# Plotly plots for each district ------------------------------------------
alexandria_plot <- generate_plot(alexandria_Jan2024_long, "COVID-19 Counts by Age Group in Alexandria (January 2024)")
arlington_plot <- generate_plot(arlington_Jan2024_long, "COVID-19 Counts by Age Group in Arlington (January 2024)")
fairfax_plot <- generate_plot(fairfax_Jan2024_long, "COVID-19 Counts by Age Group in Fairfax (January 2024)")
loudoun_plot <- generate_plot(loudoun_Jan2024_long, "COVID-19 Counts by Age Group in Loudoun (January 2024)")
pw_plot <- generate_plot(pw_Jan2024_long, "COVID-19 Counts by Age Group in Prince William (January 2024)")
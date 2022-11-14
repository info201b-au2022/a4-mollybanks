library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

# The functions might be useful for A4
source("../source/a4-helpers.R")


## Section 2  ----
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population


# This function returns relevant data frame

get_year_jail_pop <- function() {
  total_pop_df <- incarceration.data %>%
    select(year, total_jail_pop)
  return(total_pop_df)
}

# This function renders Jail Population Increase bar chart to match figure 1
plot_jail_pop_for_us <- function() {
  total_pop_df <- get_year_jail_pop()
  jail_pop_plot <- ggplot(data = total_pop_df) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)") +
    xlab(label = "Year") +
    scale_y_continuous(
      name = "Total Jail Population",
      labels = c("0", "200,000", "400,000", "600,000", "800,000")
    )
  return(jail_pop_plot)
}

#----------------------------------------------------------------------------#


## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State

# This function returns data frame with relevant values
get_jail_pop_by_states <- function(states) {
  state_df_render <- incarceration.data %>%
    select(state, year, total_jail_pop)
  return(state_df_render)
}
# This function plots jail pop by state
plot_jail_pop_by_states <- function(states) {
  state_df_render <- get_jail_pop_by_states() %>%
    filter(state == states, na.rm = TRUE)
  state_jail_pop_plot <- ggplot(data = state_df_render) +
    geom_smooth(mapping = aes(x = year, y = total_jail_pop, color = state), se = FALSE) +
    labs(title = "Increase of Jail Population by State (1970-2018)",
         x = "Year",
         y = "Total Jail Population",
         color = "State") + 
    scale_color_brewer(palette = "BrBG")
  return(state_jail_pop_plot)
  
}


#----------------------------------------------------------------------------#

## Section 5  ----
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
#  female_15to64_prop = female_prison_pop/female_pop_15to64,
# male_15to64_prop = male_prison_pop/male_pop_15to64,
inequality_df <- function(states) {
  inequality_df <- incarceration.data %>%
    mutate(new_pop = total_jail_pop - lag(total_jail_pop),
           new_pretrial_pop = total_jail_pretrial - lag(total_jail_pretrial)) %>%
    select(urbanicity, year, total_jail_pop, new_pop, total_jail_pretrial, total_jail_pretrial_rate, new_pretrial_pop)
  return(inequality_df)
}


inequality_plot <- function() {
  inequality_df <- inequality_df()
  ineq_plot <- ggplot(data = inequality_df) +
    geom_smooth(mapping = aes(x = year, y = total_jail_pretrial_rate, color = urbanicity)) + 
    labs(title = "Pretrial Jailing Rates by Urbanicity (1970-2018)",
         x = "Year",
         y = "Jail Pretrial Rate",
         color = "Urbanicity") + 
    scale_color_brewer(palette = "BrBG")
  return(ineq_plot)
}

# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ----
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
state_shape <- map_data("state")
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ----
incarceration.data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

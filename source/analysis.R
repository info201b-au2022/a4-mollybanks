library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

## Load data frame ----
incarceration.data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

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
    scale_color_brewer(palette = "PuOr")
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
    select(urbanicity, year, total_jail_pretrial, total_jail_pretrial_rate)
  return(inequality_df)
}


inequality_plot <- function() {
  inequality_df <- inequality_df()
  ineq_plot <- ggplot(data = inequality_df) +
    geom_smooth(mapping = aes(x = year, y = total_jail_pretrial_rate, color = urbanicity), se = FALSE) + 
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

state_pretrial_ineq_df <- function() {
  
  state_abv <- read.csv(
  file = "/Users/mollybanks/Documents/info201/assignments/a4-mollybanks/source/state_names_and_codes.csv",
  stringsAsFactors = FALSE)
  
  jail_state_ineq_df <- incarceration.data %>%
    filter(year == 2015) %>% 
    select(state, total_jail_pretrial_rate)
  
  ineq_by_state <- state_abv %>%
    select(State, Code) %>%
    rename(region = State,
           state = Code) %>%
    left_join(jail_state_ineq_df, by="state") %>%
    mutate(region = tolower(region)) %>%
    select(region, total_jail_pretrial_rate)
  
  state_ineq_df <- map_data("state") %>% 
    left_join(ineq_by_state, by ="region") 
  
  return(state_ineq_df)
  
}

plot_state_ineq <- function() {
  blank_theme <- theme_bw() +
    theme(
      axis.line = element_blank(),        
      axis.text = element_blank(),       
      axis.ticks = element_blank(),       
      axis.title = element_blank(),      
      plot.background = element_blank(),  
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank()     
    )
  
  state_ineq_df <- state_pretrial_ineq_df()
  
  ggplot(state_ineq_df) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = total_jail_pretrial_rate),
      color = "white", 
      size = .1        
    ) +
    coord_map() + 
    scale_fill_continuous(low = "#132B43", high = "Red") +
    labs(fill = "Pretrial Jail Rate") +
    blank_theme
}


# See Canvas
#----------------------------------------------------------------------------#


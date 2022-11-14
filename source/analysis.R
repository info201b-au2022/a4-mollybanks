library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)

## Load data frame ----
incarceration.data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  stringsAsFactors = FALSE
)

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
  total_pop_df <- get_year_jail_pop() # calls relevant data frame
  jail_pop_plot <- ggplot(data = total_pop_df) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)") +
    xlab(label = "Year") + # aesthetic changes
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
  state_df_render <- get_jail_pop_by_states() %>% # calls relevant data frame
    filter(state == states, na.rm = TRUE) # filters data frame for state matches
  state_jail_pop_plot <- ggplot(data = state_df_render) +
    geom_smooth(
      mapping = aes(
        x = year,
        y = total_jail_pop,
        color = state
      ),
      se = FALSE
    ) +
    labs(
      title = "Increase of Jail Population by State (1970-2018)",
      x = "Year",
      y = "Total Jail Population",
      color = "State"
    ) + # aesthetic changes
    scale_color_brewer(palette = "PuOr")
  return(state_jail_pop_plot)
}


#----------------------------------------------------------------------------#

## Section 5  ----
#----------------------------------------------------------------------------#
inequality_df <- function(states) {
  inequality_df <- incarceration.data %>%
    select(urbanicity, year, total_jail_pretrial, total_jail_pretrial_rate)
  return(inequality_df)
}

inequality_plot <- function() {
  inequality_df <- inequality_df() # calls relevant data frame
  ineq_plot <- ggplot(data = inequality_df) +
    geom_smooth(mapping = aes(x = year, y = total_jail_pretrial_rate, color = urbanicity), se = FALSE) +
    labs(
      title = "Pretrial Jailing Rates by Urbanicity (1970-2018)",
      x = "Year",
      y = "Jail Pretrial Rate",
      color = "Urbanicity"
    ) + # aesthetic changes
    scale_color_brewer(palette = "BrBG")
  return(ineq_plot)
}

#----------------------------------------------------------------------------#

## Section 6  ----
#----------------------------------------------------------------------------#
# this function returns relevant data frame for map
state_pretrial_ineq_df <- function(year) {

  # loads dataframe from CSV with state names
  state_abv <- read.csv(
    file = "/Users/mollybanks/Documents/info201/assignments/a4-mollybanks/source/state_names_and_codes.csv",
    stringsAsFactors = FALSE
  )

  # returns relevant columns and features for dataframe
  jail_state_ineq_df <- incarceration.data %>%
    filter(year == year) %>% # filter for year imput
    mutate(prison_prop = total_prison_pop/total_pop) %>%
    select(state, prison_prop) 

  # joins state code data frame with incarceration df to add full state name
  ineq_by_state <- state_abv %>%
    select(State, Code) %>%
    rename(
      region = State,
      state = Code
    ) %>% # renaming state row to join
    left_join(jail_state_ineq_df, by = "state") %>% # join data frame by state
    mutate(region = tolower(region)) %>% # remove caps in state name
    select(region, prison_prop) # selects relevant features

  # joins map data with relevant incarceration data frame from above
  state_ineq_df <- map_data("state") %>%
    left_join(ineq_by_state, by = "region") # joins by region (state)

  return(state_ineq_df)
}

# returns heatmap of states based on their pretrial jailing rates

plot_state_ineq <- function(year) {
  blank_theme <- theme_bw() + # creates minimalist map theme
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

  state_ineq_df <- state_pretrial_ineq_df(year) # calls relevant data frame

  ggplot(state_ineq_df) +
    geom_polygon(
      mapping = aes(
        x = long,
        y = lat,
        group = group,
        fill = prison_prop
      ),
      color = "white",
      size = .1
    ) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Red") + # aesthetic changes
    labs(fill = "Prison Admit Rate") +
    blank_theme
}
#----------------------------------------------------------------------------#

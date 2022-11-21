library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(usmap)


## Load data frame ----
incarceration.data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  stringsAsFactors = FALSE
)

## Section 2  ----
#----------------------------------------------------------------------------#
# finds county with highest pretrial jailing rate of all time
county_max_pretrial_rate <- incarceration.data %>%
  filter(total_jail_pretrial_rate == max(total_jail_pretrial_rate, na.rm = TRUE)) %>%
  pull(county_name)

# finds year that highest jailing rate occoured at the county level
year_max_pretrial_rate <- incarceration.data %>%
  filter(total_jail_pretrial_rate == max(total_jail_pretrial_rate, na.rm = TRUE)) %>%
  pull(year)

# finds year where ICE jailed the most
year_max_ice_jail <- incarceration.data %>%
  filter(total_jail_from_ice == max(total_jail_from_ice, na.rm = TRUE)) %>%
  pull(year)

# finds county where highest amount of ICE admits occoured
county_max_ice_jail <- incarceration.data %>%
  filter(total_jail_from_ice == max(total_jail_from_ice, na.rm = TRUE)) %>%
  pull(county_name)

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
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      caption = "Visual representation of mass incarceration over time."
    ) +
    xlab(label = "Year") + # aesthetic changes
    scale_y_continuous(
      name = "Total Jail Population", # changes labels for readbility
      labels = c("0", "200,000", "400,000", "600,000", "800,000")
    )
  return(jail_pop_plot)
}

#----------------------------------------------------------------------------#


## Section 4  ----
#----------------------------------------------------------------------------#
#  Prison Population Growth by State

# function takes state codes and returns full state name
get_full_state_name <- function(states) {
  state_abv <- read.csv(
    file = "/Users/mollybanks/Documents/info201/assignments/a4-mollybanks/source/state_names_and_codes.csv",
    stringsAsFactors = FALSE
  )

  state_names <- state_abv %>%
    filter(Code %in% states, na.rm = TRUE) %>% # filters abv df for state imput
    pull(State) # returns vector of full state names

  return(state_names)
}

# This function returns data frame with relevant values
df_state_jail_pop <- function(states) {
  state_jail_pop_df <- incarceration.data %>%
    filter(state %in% states, na.rm = TRUE) %>% # filters data for state imput
    select(state, year, total_jail_pop)

  return(state_jail_pop_df)
}
# This function plots jail pop by state
plot_jail_pop_by_states <- function(states) {
  state_labels <- get_full_state_name(states) # prepares label names for states

  state_jail_pop_df <- df_state_jail_pop(states) # calls relevant data frame

  state_jail_pop_plot <- ggplot(data = state_jail_pop_df) +
    geom_smooth(
      mapping = aes(
        x = year,
        y = total_jail_pop,
        color = state # representing each state by a diffrent colored line
      ),
      se = FALSE
    ) +
    labs( # renaming for redability
      title = "Jail Population by State (1970-2018)",
      x = "Year",
      y = "Total Jail Population",
      caption = "Chart represents trends in jailing by state from 1970 to 2018."
    ) +
    scale_color_discrete(name = "States", labels = state_labels) # adds state labels
  return(state_jail_pop_plot)
}

#----------------------------------------------------------------------------#

## Section 5  ----
#----------------------------------------------------------------------------#
# this function returns relevant data frame
inequality_df <- function() {
  inequality_df <- incarceration.data %>%
    select(urbanicity, year, total_jail_pretrial_rate)

  return(inequality_df)
}

# renders line graph of pretrial jailing by state
inequality_plot <- function() {
  df_inequality <- inequality_df() # calls relevant data frame
  ineq_plot <- ggplot(data = df_inequality) +
    geom_smooth(
      mapping = aes(
        x = year,
        y = total_jail_pretrial_rate,
        color = urbanicity
      ), # represents each urbanicity
      se = FALSE
    ) + # removes fill around geom_smooth lines
    labs( # renaming for readability
      title = "Pretrial Jailing Rates by Urbanicity (1970-2018)",
      x = "Year",
      y = "Jail Pretrial Rate",
      color = "Urbanicity",
      caption = "Counties marked 'urban' are core metropolitian areas with a
      population greater than one million. Suburban counties surround urban areas.
      Small and midsized counties have populations under one million.
      Rural counties have populations under 50,000."
    ) + # aesthetic changes
    scale_color_brewer(palette = "BrBG") # aesthetic changes
  return(ineq_plot)
}

#----------------------------------------------------------------------------#

## Section 6  ----
#----------------------------------------------------------------------------#
# this function returns relevant data frame for map
county_ineq_df <- function(states, years) {

  # returns relevant columns and features for dataframe
  jail_county_ineq_df <- incarceration.data %>%
    filter(
      year == years, # filter for year imput
      state == states
    ) %>% # filter for state imput
    mutate(subregion = tolower(str_replace(county_name, " County", ""))) %>%
    select(total_jail_pretrial_rate, subregion)

  # loads dataframe from CSV with state names
  state_abv <- read.csv(
    file = "/Users/mollybanks/Documents/info201/assignments/a4-mollybanks/source/state_names_and_codes.csv",
    stringsAsFactors = FALSE
  )

  state_name <- state_abv %>%
    filter(Code == states) %>% # filter df for state imput
    summarise(state = tolower(State)) %>% # lower case state for accurate join
    pull(state)


  # joins map data with relevant incarceration data frame from above
  county_ineq_df <- map_data("county") %>%
    filter(region == state_name) %>% # filters map data for state imput
    left_join(jail_county_ineq_df, by = "subregion") # joins by county

  return(county_ineq_df)
}

# returns heatmap of states based on their pretrial jailing rates
plot_county_ineq <- function(states, years) {
  county_ineq_df <- county_ineq_df(states, years) # calls relevant data frame
  title_state <- get_full_state_name(states) # gets full state name from abv imput

  blank_theme <- theme_bw() + # creates minimalist map theme
    theme( # creates minimalist theme for map
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )

  ggplot(county_ineq_df) +
    geom_polygon( # mapping states by coordinates, fill with rate
      mapping = aes(
        x = long,
        y = lat,
        group = group,
        fill = total_jail_pretrial_rate, # fill by rate
      ),
      color = "white",
      size = .1
    ) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Red") + # aesthetic changes
    labs( # renaming for readability
      title = paste(
        "Legally Innocent Jailing Rate in",
        title_state, # pastes releavnt full state name
        "(by County)"
      ),
      subtitle = paste("Rate of Pretrial Jail Bookings in", years),
      fill = "Pretrial Jaling Rate",
      caption = paste(title_state, "incarceration during", 
                      years, "displayed by county.")
    ) +
    blank_theme # renders blank theme
}

#----------------------------------------------------------------------------#

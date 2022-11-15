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
county_max_pretrial_rate <- incarceration.data %>%
  filter(total_jail_pretrial_rate == max(total_jail_pretrial_rate, na.rm = TRUE)) %>%
  pull(county_name)

year_max_pretrial_rate <- incarceration.data %>%
  filter(total_jail_pretrial_rate == max(total_jail_pretrial_rate, na.rm = TRUE)) %>%
  pull(year)

year_max_ice_jail <- incarceration.data %>%
  filter(total_jail_from_ice == max(total_jail_from_ice, na.rm = TRUE)) %>%
  pull(year)

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
# this function returns relevant data frame
inequality_df <- function(states) {
  inequality_df <- incarceration.data %>%
    select(urbanicity, year, total_jail_pretrial, total_jail_pretrial_rate)
  return(inequality_df)
}

# renders line graph of pretrial jailing by state
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
county_ineq_df <- function(states, years) {

    # returns relevant columns and features for dataframe
  jail_county_ineq_df <- incarceration.data %>%
    filter(year == years, # filter for year imput
           state == states) %>% # filter for state imput
    mutate(subregion = tolower(str_replace(county_name," County", ""))) %>%
    select(total_jail_pretrial_rate, subregion) 
  
  # loads dataframe from CSV with state names
  state_abv <- read.csv(
    file = "/Users/mollybanks/Documents/info201/assignments/a4-mollybanks/source/state_names_and_codes.csv",
    stringsAsFactors = FALSE)
  
  state_name <- state_abv %>%
    filter(Code == states) %>%
    summarise(state = tolower(State)) %>%
    pull(state)
  

  # joins map data with relevant incarceration data frame from above
  county_ineq_df <- map_data("county") %>%
    filter(region == state_name) %>%
    left_join(jail_county_ineq_df, by = "subregion") # joins by county
    
  return(county_ineq_df)
}

# returns heatmap of states based on their pretrial jailing rates
plot_county_ineq <- function(states, years) {

  county_ineq_df <- county_ineq_df(states, years) # calls relevant data frame
  
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
  
  ggplot(county_ineq_df) +
    geom_polygon(
      mapping = aes(
        x = long,
        y = lat,
        group = group,
        fill = total_jail_pretrial_rate,
      ),
      color = "white",
      size = .1
    ) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Red") +  # aesthetic changes
    labs(title = paste0("Legally Innocent Jailing Rate by County ", "(", states, ")"),
         subtitle = paste0("Rate of Pretrial Jailing", " in ", years), 
         fill = "Jail Pretrial Rate") +
    blank_theme
    
  }

#----------------------------------------------------------------------------#


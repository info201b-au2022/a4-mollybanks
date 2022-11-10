library(tidyverse)
library(dplyr)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

#----------------------------------------------------------------------------#
# Function returns relevant data frame

get_year_jail_pop <- function() {
  total_pop_df <- incarceration.data %>%
    select(year, total_jail_pop)
return(total_pop_df)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  total_pop_df <- get_year_jail_pop()
  jail_pop_plot <- ggplot(data = total_pop_df) +
   geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)") +
    xlab(label = "Year") +
    scale_y_continuous(name = "Total Jail Population",
                       labels = c("0", "200,000", "400,000", "600,000", "800,000")
                       )

  return(jail_pop_plot)   
} 

# labels = c("0", "200,000", "400,000", "600,000", "800,000") scale_y_continuous( name = "Total Jail Population",
# labels = c("200,000","400,000", "600,000", "800,000")) +


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 
incarceration.data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)



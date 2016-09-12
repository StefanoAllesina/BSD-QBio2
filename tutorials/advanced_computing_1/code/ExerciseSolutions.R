# Stefano Allesina Sept 11 2016

# The file `data/Chicago_Crimes_May2016.csv` contains a list of all the 
#crimes reported in Chicago in May 2016. 
# Form small groups and work on the following exercises:
#   
# - **Crime map** write a function that takes as input a crime's `Primary Type` 
#   (e.g., `ASSAULT`), and draws a map of all the occurrences. 
#   Mark a point for each occurrence using `Latitude` and `Longitude`. 
#   Set the `alpha` to something like 0.1 to show brighter colors in areas 
#   with many occurrences.
# 

# Load the packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the data
crimes <- read.csv("../data/Chicago_Crimes_May2016.csv", stringsAsFactors = FALSE)
areas <- read.csv("../data/Chicago_Crimes_CommunityAreas.csv", stringsAsFactors = FALSE)

plot_crime <- function(type = "ASSAULT"){ # Use ASSAULT by default
  pl <- ggplot(crimes %>% filter(Primary.Type == type), # filter the data 
         aes(x = X.Coordinate, y = Y.Coordinate)) +
  geom_point(alpha = 0.1, size = 2, colour = "red") + 
    theme_bw() + # choose simple theme 
    ggtitle(type) # set a title for the plot
  return(pl)
}

# Examples:
show(plot_crime())
show(plot_crime("HOMICIDE"))
show(plot_crime("PROSTITUTION"))

# - **Crimes by community** write a function that takes as input 
#    a crime's `Primary Type`, and produces a barplot showing the
#    number of crimes per `Community area`. 
#    The names of the community areas are found in the file 
#    `data/Chicago_Crimes_CommunityAreas.csv`. 
#     You will need to `join` the tables before plotting.
# 

plot_crime_community <- function(type = "ASSAULT"){
  # Join the two tables
  toplot <- inner_join(crimes %>% filter(Primary.Type == type), 
                       areas)
  # produce the barplot
  pl <- ggplot(toplot,
               aes(x = Name)) +
    geom_bar() + 
    theme_bw() + 
    ggtitle(type) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate labels
  return(pl)
}
# Examples:
show(plot_crime_community())
show(plot_crime_community("HOMICIDE"))
show(plot_crime_community("PROSTITUTION"))

# - **Violent crimes** add a new column to the dataset 
#   specifying whether the crime is considered violent 
#   (e.g., `HOMICIDE`, `ASSAULT`, `KIDNAPPING`, `BATTERY`, 
#   `CRIM SEXUAL ASSAULT`, etc.)
# 

crimes <- crimes %>% 
  mutate(violent = Primary.Type %in% c("HOMICIDE", 
                                       "ASSAULT", 
                                       "KIDNAPPING", 
                                       "BATTERY", 
                                       "CRIM SEXUAL ASSAULT"))

# - **Crimes in time** plot the number of violent crimes against 
#   time, faceting by community areas.

# First, we join crimes and areas
crimes <- inner_join(crimes, areas)
# Second, we convert to time and extract for example the day
library(lubridate)
crimes <- crimes %>% mutate(Date = mdy_hms(Date))

# summarize the number of cases of violent crimes per day
violent_per_day_area <- crimes %>% select(violent, 
                                          Date,
                                          Name)
violent_per_day_area <- violent_per_day_area %>% 
                        filter(violent == TRUE) %>% 
                        mutate(Date = date(Date)) # remove time

violent_per_day_area <- violent_per_day_area %>% 
  group_by(Date, Name) %>% 
  summarise(tot_violent = n())
# Now plot:
show(
  ggplot(data = violent_per_day_area) +
    aes(x = Date, # Yr, Mo, Day
        y = tot_violent) + 
    geom_line() +
    facet_wrap(~Name) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate labels
)

# - **Dangerous day** which day of the week is the most dangerous?

# That's easy to do using lubridate
# Add weekday
crimes <- crimes %>% mutate(wday = wday(Date, label = TRUE))
show(
  ggplot(data = crimes %>% filter(violent == TRUE)) +
    aes(x = wday) + geom_bar() + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate labels
)

# - **Dangerous time** which time of the day is the most dangerous (divide the data by hour of the day).
crimes <- crimes %>% mutate(hour = hour(Date))
show(
  ggplot(data = crimes %>% filter(violent == TRUE)) +
    aes(x = hour) + geom_bar() + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate labels
)

# - **Correlation between crimes** which crimes tend to have the same pattern? Divide the crimes by day and type, and plot the correlation between crimes using `geom_tile` and colouring the cells according to the correlation (see `cor` for a function that computes the correlation between different columns).
# Let's build a table of crimes by day
crimes_day <- crimes %>% group_by(Primary.Type, )

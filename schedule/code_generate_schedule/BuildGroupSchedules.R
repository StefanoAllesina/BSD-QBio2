library(dplyr)
library(tidyr)
library(lubridate)
# Load activities
activities <- read.csv("tables/common.csv", stringsAsFactors = FALSE)
activities <- rbind(activities, read.csv("tables/tutorials.csv", stringsAsFactors = FALSE))
activities <- rbind(activities, read.csv("tables/workshops.csv", stringsAsFactors = FALSE))
# Load activities names
activities_names <- read.csv("tables/activities_name_room.csv", stringsAsFactors = FALSE)
# Load group names
group_names <- read.csv("tables/group_names.csv")
# Join
activities <- inner_join(activities, activities_names)
# Transform into rows
activities <- activities %>% gather(Group, Tmp, 5:16) %>% filter(Tmp == 1) %>% select(-Tmp)
# Join with group names
activities <- inner_join(activities, group_names)

# Create a column for starting time; arrange all events by group and start time
activities <- activities %>% 
  rowwise() %>% 
  mutate(StartTime = paste0("9/", Day, "/2016 ", strsplit(Time, "-")[[1]][1], " ", AMPM)) %>% 
  mutate(StartTime = parse_date_time(StartTime, "%m/%d/%Y %I:%M %p", tz = "America/New_York")) %>% 
  arrange(Group, StartTime)

for (my_group in sort(unique(activities$Group))){
  group_activities <- activities %>% filter(Group == my_group)
  for (my_day in sort(unique())){
    
  }
}
#### Basic Computing 2: Exercises in groups
### Google Flu Trends

## Read in the data.
# Remember to use "stringsAsFactors=FALSE" to prevent the dates from
#   becoming factors
google <- read.csv("../data/GoogleFlu/PreisMoat2014.csv", stringsAsFactors=FALSE)

## Plot number of visits vs. GoogleFluTrends
plot(google$WeeklyOutpatientVisitsforILI, google$GoogleFluTrends)

## Calculate the correlation using `cor`
cor(google$WeeklyOutpatientVisitsforILI, google$GoogleFluTrends)

## Compare new algorithm to same timeframe in previous years
# first generate new columns for year and month using `substr`
# note that we also convert these values to integers to facilitate
#   subsetting by months in the for loop
google$year <- as.integer(substr(google$WeekCommencing, start=1, stop=4))
google$month <- as.integer(substr(google$WeekCommencing, start=6, stop=7))
# loop through each unique year in the data
for (yr in unique(google$year)) {
  # and subset the full dataset by both the year (changes in each loop) and
  #   the months of interest (same in each loop)
  google_subset <- google[google$year == yr & google$month >= 8,]
  # run the correlation on the subsetted dataset
  correlation <- cor(google_subset$WeeklyOutpatientVisitsforILI,
                     google_subset$GoogleFluTrends)
  # and print the results (nicely)
  print(paste("August-December of", yr,
              "shows a correlation coefficient of", round(correlation, 4)))
}
# note that the value for 2013 is not higher than the previous years
#### Advanced Computing 2: Exercises in groups
### Extract primers and polymorphic sites

## first load the necessary packages for the analysis
library(stringr)
library(dplyr)
library(ggplot2)

## take a look at the file
system("cat ../data/Ptak_etal_2004.txt")

## Read the text as a single string, intervaled by new lines
my_txt <- paste(readLines("../data/Ptak_etal_2004.txt"), collapse="\n")

## Use regular expressions to produce a data frame containing
##   the primers used in the study
# note here that the regular expression has two goups:
#   the first starts with TAP and ends with either 3' or 5'
#   the second is the DNA sequence of As, Ts, Cs, and Gs
primers <- str_match_all(my_txt, "\\n(TAP[\\d-]+[35].)\\s+?([ATCG]+)") %>%
  # convert to data frame for dplyr
  as.data.frame(stringsAsFactors=FALSE) %>%
  # rename the the columns corresponding to the regular expression groups
  #   and drop the first column (corresponding to the full match)
  transmute(ID=X2, Sequence=X3)
head(primers)

## Write another regular expression to extract the polymorphic
##   region for each chimp
# this regular expression is similar to that above, but we have to be
#   more general when extracting the sequence because there are spaces
#   and newlines mixed in
chimps <- str_match_all(my_txt, ">\\s(\\w+)\\n([ACTG\\s\\n]+)") %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  # in addition to the renaming and dropping, in this case also get rid of
  #   any spaces or newlines in the sequence (replace them with nothing)
  transmute(Chimp=X2, Sequence=str_replace_all(X3, "\\s|\\n", ""))
head(chimps)

### A map of *Science*
## Read the file `pubmed_results.txt`, and extract all the US ZIP codes
my_txt <- paste(readLines("../data/MapOfScience/pubmed_results.txt"), collapse="\n")
# note that the regular expression here gets both XXXXX and XXXXX-XXXX format
#   ZIP codes, but since the coordinate file only has the former, we store
#   the latter digits separately if present
zipcodes <- str_match_all(my_txt, "(\\d{5})(-\\d{4})?,?\\sUSA") %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  # note that this converts the ZIP codes to integers while also renaming them
  #   to match the other file and dropping the other columns
  transmute(ZIP=as.integer(X2))

## Count the number of occurences of each ZIP code using dplyr
# note that this operation is also quite easy without `dplyr`:
#   zipcodes$count <- table(zipcodes$ZIP)
zipcodes <- zipcodes %>%
  group_by(ZIP) %>%
  mutate(count=n())

## Join the table you've created with the data in `zipcodes_coordinates.txt`
# we use an inner join because we only care about ZIP values that are present
#   in both tables
zipcodes <- inner_join(zipcodes,
                       read.csv("../data/MapOfScience/zipcodes_coordinates.txt"),
                       by="ZIP")

## Plot the results using `ggplot2`
## (either use points with different colors/alphas:
ggplot(zipcodes) +
  aes(x=LNG, y=LAT, colour=count) +
  geom_point() +
  coord_fixed()   # this line makes sure the lat/lng gridlines are square
ggplot(zipcodes) +
  aes(x=LNG, y=LAT, alpha=count) +
  geom_point() +
  coord_fixed()
## or render the density in two dimensions):
ggplot(zipcodes) +
  aes(x=LNG, y=LAT) +
  stat_bin2d(bins=60)

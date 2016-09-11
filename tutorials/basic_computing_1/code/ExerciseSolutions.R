# Stefano Allesina, Sept 11 2016

## Nobel nominations
# The file `../data/nobel_nominations.csv` contains the nominations to the 
# Nobel prize from 1901 to 1964. There are three columns (the file has no header): 
# a) the field (e.g. `Phy` for physics), b) the year of the nomination, 
# c) the id and name of the nominee.
#
# - Take your favorite field. Who received most nominations? 
# - Who received nominations in more than one field?
# - Take the field of physics. Which year had the largest number of nominees?
# - What is the average number of nominees for each field? 
#   Calculate the average number of nominee for each field across years.

## SOLUTION
# First, we need to read the data. Assuming your working directory is basic_computing_1/code
# (or Ctrl+Shift+H to set your working directory), you can type:

nobel <- read.csv("../data/nobel_nominations.csv", header = FALSE, stringsAsFactors = FALSE)
# Notes: 
# header = FALSE -> do not use the first row as column names
# stringsAsFactors = FALSE -> do not convert strings to categorical values

# We choose legible names for the columns, as suggested in the Hints
colnames(nobel) <- c("Field", "Year", "Nominee")

# For the first part of the exercise, we're going to examine nominations in Physics.
# We subset the data:
phy <- nobel[nobel$Field == "Phy", ] 
# explanation: nobel$Field == "Phy" creates a logical vector, specifying whether
# the corresponding row contains "Phy" as Field. We use the vector to choose only
# the rows belonging to Physics.

# Take a look at the result
head(phy)
# Now we want to see who got most nominations. Let's use the table() function:
phy_table <- table(phy$Nominee)
# The table now contains the number of nomination for each nominee
# We can sort it in descending order
sort(phy_table, decreasing = TRUE)
# And take the first element
sort(phy_table, decreasing = TRUE)[1]
# Showing that Arnold Sommerfeld (who never won the Nobel Prize, but advised many
# Nobel laureates such as Heisenberg, Pauli, etc.) was nominated 25 times!

# Now we want to ask "Who received nominations in more than one field?"
# Again, the function table() is very useful:
nominations_by_field <- table(nobel$Nominee, nobel$Field)
# look at the results
nominations_by_field[1:3,]
# Now we just need to list all the scientists that have more than one nonzero
# entry in the corresponding row of the table

# We can check whether each element is nonzero
nominations_by_field > 0
# which gives you a TRUE/FALSE value. Interestingly, for a computer TRUE is also
# equal to 1, and FALSE to zero. We can therefore sum
num_field_nomination <- rowSums(nominations_by_field > 0)
# sort the data
num_field_nomination <- sort(num_field_nomination, decreasing = TRUE)
# and show all those that had 2 or more nominations
num_field_nomination[num_field_nomination > 1]

# The question "Take the field of physics. Which year had the largest number of nominees?"
# works in the same way as that asking the number of nominations in physics. The answer
# is therefore
sort(table(phy$Year), decreasing = TRUE)[1]
# Which gives the year 1957 (57 nominees, as in 1963)

# Finally, we want to calculate the average number of nominees per field.
# One way to solve this problem is to count the number of nominees per field, 
# and divide by the number of years represented in the database
# This command gives the number of nominees across all years
table(nobel$Field)
# And this counts the number of years in which we have at least one nomination
table(unique(nobel[, 1:2])$Field)
# This command is a little complicated: Let's dissect it:
nobel[, 1:2] # takes only the columns Field and Year from the data
unique(nobel[, 1:2]) # removes all duplicate rows from the data. 
                     # Now each year is represented only once
table(unique(nobel[, 1:2])$Field) # Count how many active years per field.
# Thus, the solution is
table(nobel$Field) / table(unique(nobel[, 1:2])$Field)
# Showing that Medicine has (by far) more nominees per year
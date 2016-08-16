# load the data
load("../Data/MTneuron.RData")

# Make a raster plot of the spike data

# Number of directions
directions <- as.vector(directions)
nDirs <- length(directions)

# For each direction, count number of replicates
nReps <- rep(0, nDirs)
for (n in 1:nDirs){
  nReps[n] <- sum(theta == n)
}

# plot each replicate separately, with a small space between replicates
# the color indicates the direction

# Colors
mycols <- rainbow(nDirs)

# initialize an empty data-frame (table)
toplot <- data.frame(x = numeric(0), y = numeric(0), color = character(0))

for (dind in 1:nDirs){
  # for each direction
  trials <- which(theta == dind)
  counter <- 0
  for (iRep in 1:(nReps[dind])){
    # find the time of the spike
    spks <- dirtune[trials[iRep], ]
    spks <- spks[spks > 0]
    # add it to the data to be plotted
    toplot <- rbind(toplot, 
                    data.frame(x = spks, y = directions[dind] + 0.3 * counter, color = mycols[dind]))
    # the counter gives the spacing between replicates for the same direction
    counter <- counter + 1
  }
}
# make sure colors are interpreted literally
toplot$color <- as.character(toplot$color)

print(
plot(toplot$x, toplot$y, col = toplot$color, pch = 16, cex = 0.5,
     xlab = 'Time (s)', ylab = 'Stimulus Direction (degrees)', main = 'Raster Plot of MT neuron',
     xlim = c(0, 400))
)

# get rid of the big matrix
rm(toplot)

# load the data
load("../Data/MTneurons8.RData")
cellnum<-1

spikes_cell<-spikes[,,1:nReps[cellnum],,cellnum]
# Make a raster plot of the spike data

# Number of directions
nDirs <- length(directions)

# plot each repetition separately, with a small space between repetitions
# the color indicates the direction

# Colors
mycols <- rainbow(nDirs)

# initialize an empty data-frame (table)
toplot <- data.frame(x = numeric(0), y = numeric(0), color = character(0))
speed<-4

for (dind in 1:nDirs){
  # for each direction
  counter <- 0
  for (iRep in 1:(nReps[cellnum])){
    # find the time of the spikes
    if (sum(spikes_cell[dind,speed,iRep,])>0){
    spks <-which(spikes_cell[dind,speed,iRep,]==1)
    # add it to the data to be plotted
    toplot <- rbind(toplot, 
                    data.frame(x = spks, y = directions[dind] + 0.3 * counter, color = mycols[dind]))
    # the counter gives the spacing between replicates for the same direction
    }
    counter <- counter + 1
  }
}
# make sure colors are interpreted literally
toplot$color <- as.character(toplot$color)

print(
plot(toplot$x, toplot$y, col = toplot$color, pch = 16, cex = 0.5,
     xlab = 'Time (s)', ylab = 'Stimulus Direction (degrees)', main = 'Raster Plot of MT neuron',
     xlim = c(0, 200))
)

# get rid of the big matrix
rm(toplot)

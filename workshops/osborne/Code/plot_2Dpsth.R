# =============================================
# Copied from plot_tuncurve.R
# =============================================
# load the data
load("../Data/MTneuron.RData")

# Number of directions
directions <- as.vector(directions)
nDirs <- length(directions)

# For each direction, count number of replicates
# the command rep repeats a certain value a number of times, and creates a vectors
nReps <- rep(0, nDirs)
for (n in 1:nDirs){
  nReps[n] <- sum(theta == n)
}

# create array data that uses a vector of 1s and 0s instead of spike times
# What is the maximum time?
maxTime <- round(max(dirtune))

mydata <- array(0, c(maxTime, nDirs, max(nReps))) 
# Let's fill the array with ones and zeros
# For each direction
for (n in 1:nDirs){
  # which are the corresponding thetas?
  index <- which(theta == n)
  # For each trial
  for (i in 1:length(index)){
    spks <- round(dirtune[index[i], ]) # make spike times integers
    spks <- spks[spks > 0] # take only those > 0
    mydata[spks, n, i] <- 1 # set to 1 in the array mydata
  }
}
# =============================================
# END Copied from plot_tuncurve.R
# =============================================

# Matrix of spike times after motion onset for each direction
PSTH <- matrix(0, maxTime, nDirs)
for (n in 1:nDirs){
  PSTH[ ,n] <- rowMeans(mydata[, n, 1:(nReps[n])]) # it is important to average just over the actual trials
}         

# Note that PSTH has units of spikes/per 1ms time bins.  To get spikes/s
# multiply by 1000 ms/s
print(
filled.contour(
               1:maxTime, # x-axis
               directions, # its nice to have real units like degrees!
                                        # these numbers are in the vector called directions 
                                        # in your workspace
               (PSTH) * 1000, 
               nlevels = 30, # number of different colors to use
               plot.title = title(main = 'PSTH vs direction for an MT neuron',
                                  xlab =  'time since motion onset (ms)'),
                                  ylab = 'direction (degrees)',
               # choose colors
               col = rev(rainbow(28,start=0,end=2/3))
)
)

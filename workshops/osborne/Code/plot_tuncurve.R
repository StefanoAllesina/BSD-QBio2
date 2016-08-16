# 2) plot tuncurve

# load the data
load("../Data/MTneuron.RData")

# Number of directions
directions <- as.vector(directions)
nDirs <- length(directions)

# For each direction, count number of repeats
# the command rep repeats a certain value a number of times, and creates a vector
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

# You can just count all the spikes in the array and plot the direction
# tuning of the total count, but then you will be conflating contributions
# from stimulus driven and non-stimulus driven activity.  One way around
# this is to estimate the duration of the neural response and to reject 
# spikes coming earlier or later.

# multiply by 1000 to transform into spikes per second,
# take the mean across repeats
# use direction 12 as an example
toplot <- 1000 * rowMeans(mydata[, 12, 1:nReps[12]])
print(
plot(toplot, 
     type = "l", # plot lines
     xlab = 'time from motion onset (ms)', # x label
     ylab = 'Average firing rate (spikes/s)',
     main = paste('PSTH of MT neuron response to theta = ', directions[12], 'degrees')
     )
)
# Spikes between 50ms and 350ms seem most likely to be motion driven
# so let's look at those between 45 and 355 to leave some room around these values

counts <- matrix(0, nDirs, max(nReps))
mcounts <- rep(0, nDirs)

# for each direction
for (n in 1:nDirs){
  # find which trials are associated with that direction
  index <- which(theta == n)
  # for each trial
  for (i in 1:length(index)){
    # find the spikes
    spks <- which(dirtune[index[i], ] > 0) 
    # take only those between 45 and 355
    spks <- spks[spks > 45 & spks < 355]
    # count them
    counts[n, i] <- length(spks)
  }
  # mean count per direction across repeats
  mcounts[n] <- mean(counts[n, 1:length(index)])
}


# Plot the tuning curve
# how many milliseconds have elapsed?
totTime <- 355 - 45

# use it as denominator
mrates <- 1000 * mcounts / totTime

print(
plot(directions, # x-axis
     mrates, #y-axis
     xlab = 'degrees',
     ylab = 'Avg. rate (spikes/s)',
     ylim = c(0, 170)
     )
)

# now we want to fit a normal distribution
# First, we transform rates into probabilities
p_spike_dir <- mrates / sum(mrates)

# Then we want to calculate the mean and variance
# E[X] = Sum_i X[i] * P(X[i])
mu <- sum(directions * p_spike_dir)
# Now the unadjusted sample variance
# E[(X[i] - E[X])^2] = E[X[i]^2] - E^2[X[i]]
sigma2 <- sum(directions^2 * p_spike_dir) - mu^2

# now add a normal curve to the plot
# the density of a normal distribution is given by dnorm(x, mu, sigma) 
# see ?dnorm for the manual
# because we're trying to fit a normal to a count data, we need to multiply
# the density by the "bin size" (here we're considering 15 degrees apart, so use 15) and transform 
# densities into counts, by multiplying for the sum of mrates
curve(dnorm(x, mu, sqrt(sigma2)) * sum(mrates) * 15, # 15 is the "bin size"
      directions, 
      add = TRUE)


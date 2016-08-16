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

# get an array of the spike counts by direction
Time <- 1:350  # let's just count spikes up to 350ms after motion onset
counts <- matrix(0, nDirs, max(nReps))
for (n in 1:nDirs){
  counts[n, ] <- colSums(mydata[Time, n, ])
}

# Find the joint distribution between counts and directions and the
# conditional distribution
maxcount <- max(counts)
countbins <- seq(0, maxcount + 6, by = 6) # 18 bins for R we need the actual breaks
ncountbins <- length(countbins) - 1 

Pjoint <- matrix(0, ncountbins, nDirs)
Pcounts_given_dir <- Pjoint

for (n in 1:nDirs){
  # extract values
  tmp <- as.vector(counts[n, 1:(nReps[n])])
  # construct histogram
  # see ?hist to see how this function works
  hh <- hist(tmp, breaks = countbins, plot = FALSE)
  # these are the midpoints for each bin, which we will use for plotting
  mids <- hh$mids  
  
  
  # Notice that in each case, we are normalizing by the number of trials that
  # go into the probability estimate.  For the joint distribution that is all
  # the data.  For the conditional distribution, which is formed direction by
  # direction, that is only the repeats for the given direction. 
  
  # normalize by number of repeats
  Pcounts_given_dir[,n] <- hh$counts / nReps[n]
  # or by total number of repeats
  Pjoint[, n] <- hh$counts / sum(nReps)
}

# Plot the conditional count distribution for the first 350ms of the
# response, for several directions

# empty plot
print(
plot(1, type = "n", 
     xlab = 'count', 
     ylab = 'probability', 
     main = 'Conditional count distributions for different directions',
     xlim = c(0, 120),
     ylim = c(0, 1)))

# now plot several directions
for (mydir in seq(1, 24, by = 4)){
  points(mids, Pcounts_given_dir[, mydir], 
         type = "l",
         col = 1 + (mydir - 1) / 4)
  # add a text for sort-of-legend
  text(110, 1.0 - mydir * 0.01, paste("angle = ", directions[mydir]), col =  1  + (mydir - 1) / 4)
}


# Cumulative sums
cumcounts <- array(0, dim(mydata))
for (i in 1:dim(cumcounts)[3]){
  cumcounts[,,i] <- apply(mydata[,,i], 2, cumsum)
}

# get the trial-averaged cumulative count for each direction
# 
mean_cumcounts <- matrix(0, dim(cumcounts)[1], nDirs)
for(n in 1:nDirs){
  mean_cumcounts[, n] <- rowMeans(cumcounts[, n, 1:nReps[n]])
  
}
# plot a few cumulative distributions
# first an empty plot
print(
  plot(1, type = "n", 
       xlab = 'time since motion onset (ms)', 
       ylab = 'spike count', 
       main = 'Mean cumulative spike count by direction',
       xlim = c(0, 350),
       ylim = c(0, 100)))

# now plot several directions
toplot <- c(7, 9, 11, 13, 15, 17, 19)
for (i in 1:length(toplot)){
  points(Time, mean_cumcounts[Time, toplot[i]],  
         type = "l",
         col = i)
  # add a text for sort-of-legend
  text(20, 100 - i * 6, paste("angle = ", directions[toplot[i]]), col =  i)
}

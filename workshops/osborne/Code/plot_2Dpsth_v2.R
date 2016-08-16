##Exercise (3)  Plot the PSTH (peri-stimulus time histogram): the firing rate as a function of time

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

# Now, instead of counting spikes, we want to express the responses as a function of time, but averaged
# across repeats of the same stimulus.  One way to do this is to make the response on each
# trial a time vector from 0 to the maximum spike time.  The vector will have 0s where there
# aren't any spikes and 1s where there are.  This is going to make it easier to average over
# repetitions

# What is the maximum time?
maxTime <- round(max(dirtune))  #550ms

#make an array of 0s of length 550 by nDirs by max(nReps)
mydata <- array(0, c(maxTime, nDirs, max(nReps))) 

# Let's round spike times to the nearest ms and then put 1s in the array mydata
# at those times

# Step through each direction and rep
for (n in 1:nDirs){
  # find the data for the direction of interest
  index <- which(theta == n)
  # For each trial
  for (i in 1:length(index)){
    spks <- round(dirtune[index[i], ]) # make spike times integers
    spks <- spks[spks > 0] # take only those > 0
    mydata[spks, n, i] <- 1 # set to 1 in the array mydata
  }
}


# Let's look at the PSTH for one direction, number 12 (-15 degrees)
# multiply by 1000 to transform into spikes per second
# take the mean across repeats

toplot <- 1000 * rowMeans(mydata[, 12, 1:nReps[12]])
print(
  plot(toplot, 
       type = "l", # plot lines
       xlab = 'time from motion onset (ms)', # x label
       ylab = 'Average firing rate (spikes/s)',
       main = paste('PSTH of MT neuron response to theta = ', directions[12], 'degrees')
  )
)

# Now make the PSTH for all the directions
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

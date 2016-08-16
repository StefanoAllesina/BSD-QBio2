# Exercise (2) Plot the direction tuning curve for an MT neuron

# A tuning curve is the average firing rate of the neuron as a function of the 
# stimulus direction

# load the data
load("../Data/MTneuron.RData")

#you should have the variables dirtune, theta, directions

# Find the number of directions
directions <- as.vector(directions)  # 24 directions
nDirs <- length(directions)          # spaced 15 degrees apart

# For each direction, count number of stimulus repeats
# the command rep repeats a certain value a number of times, and creates a vector
  nReps <- rep(0, nDirs)  #make a vector of zeros of length nDirs
  for (n in 1:nDirs){   #step though the directions
    nReps[n] <- sum(theta == n)   #count how many times the stimulus was repeated
  }

#  Now we want to count the spikes fired for each direction in the array dirtune
#  How is dirtune organized?
dim(dirtune)  

#824 trials (this combines all the repeats of all the directions) by 109 spikes.  
# So for some direction stimulus on one of its repeats there were 109 spikes. 

# You can use the same approach that you used in Exercise 1 (plotting the RF map) 
# to count spikes

# Create a vector of zeros of length nDirs
nspks <- rep(0,nDirs)

for (dind in 1:nDirs){
  getreps <- which(theta==dind)  # get the collections of reps for the direction dind
  nspks[dind] = sum(dirtune[getreps,]>0)  #count the non-zero elements
  
  # You can just count all the spikes in the array and plot the direction
  # tuning of the total count, but then you will be conflating contributions
  # from stimulus driven and non-stimulus driven activity.  One way around
  # this is to estimate the duration of the neural response and to reject 
  # spikes coming earlier or later.
  # Let's count the spikes that occur after 50 (latency of the neuron)
  # and before the response to the 250ms stimulus is over, plus a bit more time on either side
  
  tmp <- dirtune[getreps,]
  nspks[dind] <- length(tmp[tmp > 45 & tmp < 305]) 
  nspks[dind] = nspks[dind]/nReps[dind]  #normalize the count values by the number of reps
}

#  In Neuroscience, it is typical to plot tuning curves in terms of a firing rate
# i.e. the number of spikes per unit time, rather than a count.  To convert your spike count
# nspks into a rate, you will want to divide by the size of the time window you used to 
# count up the spikes.

# how many milliseconds have elapsed?
totTime <- 305 - 45
mean_rates = 1000* nspks/totTime  # divide the average spike count by the size of the time window
                            # to get spikes per ms.  Multiply by 1000 ms /1 s to get spikes per sec

# Plot the tuning curve
print(
  plot(directions,
       mean_rates,
       xlab = 'direction (degrees)', # x label
       ylab = 'Average firing rate (spikes/s)',
       main = paste('Direction tuning curve'),
       ylim = c(0, 305)
  )
)

# Now add a Gaussian fit to the plot

# First, we transform rates into probabilities
p_spike_dir <- mean_rates / sum(mean_rates)

# Then we want to calculate the mean and variance
# E[X] = Sum_i X[i] * P(X[i])
mu <- sum(directions * p_spike_dir)
# Now the unadjusted sample variance
# E[(X[i] - E[X])^2] = E[X[i]^2] - E^2[X[i]]
sigma2 <- sum(directions^2 * p_spike_dir) - mu^2

# Notice that the tuning curve looks Gaussian
# so let's add a normal curve to the plot
# the density of a normal distribution is given by dnorm(x, mu, sigma) 
# see ?dnorm for the manual
# because we're trying to fit a normal to a count data, we need to multiply
# the density by the "bin size" (here we're considering 15 degrees apart, 
# so use 15) and transform densities into counts, by multiplying for the sum of mrates

curve(dnorm(x, mu, sqrt(sigma2)) * sum(mean_rates) * 15, # 15 degrees is the direction "bin size"
      directions, 
      add = TRUE)



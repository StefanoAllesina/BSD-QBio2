##Exercise (3)  Plot the PSTH (peri-stimulus time histogram): the firing rate as a function of time

# load the data
load("../Data/MTneuron_long.RData")
maxTime<-550
nDirs<-length(directions)
PSTH <- array(0, c(maxTime, nDirs))
for (n in 1:nDirs){
  #average over the actual trials for each direction, then multiply by 1000 to get units of spikes per second
  PSTH[ ,n] <- rowMeans(spikes_long[, n, 1:(nReps_long[n])])*1000 
}  

#find preferred direction, plot PSTH
dirtune<-colSums(PSTH)
prefdir<-which(dirtune==max(dirtune))

print(plot(PSTH[,prefdir], 
     type = "l", # plot lines
     xlab = 'Time from motion onset (ms)', # x label
     ylab = 'Firing rate (spikes/s)',
     main = paste('PSTH of MT neuron response to ', directions[prefdir], 'degrees')
))


# Note that PSTH has units of spikes/per 1ms time bins.  To get spikes/s
# multiply by 1000 ms/s
print(
  filled.contour(
    1:maxTime, # x-axis
    directions, # its nice to have real units like degrees!
    # these numbers are in the vector called directions 
    # in your workspace
    (PSTH), 
    nlevels = 30, # number of different colors to use
    plot.title = title(main = 'PSTH vs direction for an MT neuron',
                       xlab =  'Time since motion onset (ms)'),
    ylab = 'Direction (degrees)',
    # choose colors
    col = rev(rainbow(28,start=0,end=2/3))
  )
)

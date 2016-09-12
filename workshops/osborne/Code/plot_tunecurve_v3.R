# Exercise (2) Plot the direction tuning curve for an MT neuron

# A tuning curve is the average firing rate of the neuron as a function of the 
# stimulus direction
rm(list=ls())
# load the data
load("../Data/MTneurons8.RData")

#you should have the variables dirtune, theta, directions

# Find the number of directions
nDirs <- length(directions)          # spaced 15 degrees apart
nSpds<-length(speeds)

#  Now we want to count the spikes fired for each direction in the array spikes
#  How is spikes organized?
dim(spikes)  

# 24(directions) by 8 (speeds) by 32 (maximum number of repetitions) by 200 (1-millisecond time bins) by 8 (cells)
#let's take spikes for only the first cell
cellnum<-1
spikes_cell<-spikes[,,1:nReps[cellnum],,cellnum]

#how many spikes did the cell fire on a 200ms trial to a specific stimulus? 
#take the response on the first trial to direction #13 and speed #4: 15 degrees, 8 dps
sum(spikes_cell[13,4,1,])

#to calculate the mean spike count for all directions, we sum over time (keep first three dimensions), then take the mean across trials (keep first 2 dimensions)
spikecount_sum<-rowSums(spikes_cell,dims=3)
spikecount_mean<-rowMeans(spikecount_sum,dims=2)

#to get direction tuning, collapse across speeds (2nd dimension) and scale to get spikes/second
mean_rates_dir<-rowMeans(spikecount_mean)*(1000/200) 


# Plot the tuning curve
print(
  plot(directions,
       mean_rates_dir,
       xlab = 'Direction (degrees)', # x label
       ylab = 'Firing rate (spikes/s)',
       main = paste('Direction tuning curve'),
       ylim=c(0,70)
  )
)

#to add errorbars, calculate standard deviation first

#make empty vector to concatenate sd values
sddir<-vector()
for (dir in 1:length(directions)){
  sddir<-c(sddir,sd(spikecount_sum[dir,,]*(1000/200) ))
}

#plot vertical segments of errorbars
segments(directions, mean_rates_dir-sddir,directions, mean_rates_dir+sddir)
#width of top/bottom of errorbars
segwidth = 2
segments(directions-segwidth,mean_rates_dir-sddir,directions+segwidth,mean_rates_dir-sddir)
segments(directions-segwidth,mean_rates_dir+sddir,directions+segwidth,mean_rates_dir+sddir)

#plot vertical segments of errorbars
segments(directions, mean_rates_dir-sddir,directions, mean_rates_dir+sddir)
#width of top/bottom of errorbars
segwidth = 2
segments(directions-segwidth,mean_rates_dir-sddir,directions+segwidth,mean_rates_dir-sddir)
segments(directions-segwidth,mean_rates_dir+sddir,directions+segwidth,mean_rates_dir+sddir)


#What is the preferred motion direction for this cell?
#It is the direction with the highest spike count on average (across repeats of the stimulus)

maxdir_index <-which(mean_rates_dir==max(mean_rates_dir))
directions[maxdir_index]

#the preferred direction is +15degrees (0 degrees is rightward)
# the 24 directions were in 15 deg incrememnts
#the opposite direction is 180 deg from the preferred, ie 15-180=165 deg
# So we want index = 1 for the ooposite direction


opp_dir <- 1
background_rate <- mean_rates_dir[opp_dir]

#difference in firing rate between preferred and background (opposite direction)?

max_minus_min <- max(mean_rates_dir)-background_rate
#same for speed
mean_rates_spd<-colMeans(spikecount_mean)*(1000/200)

print(
  plot(log2(speeds),
       mean_rates_spd,
       xlab = expression('log'[2]*'speed (degrees per second)'), # x label
       ylab = 'Average firing rate (spikes/s)',
       main = paste('Speed tuning curve'),
       ylim=c(0,80)
  )
)

sdspeed<-vector()
for (speed in 1:length(speeds)){
  sdspeed<-c(sdspeed,sd(spikecount_sum[,speed,]*(1000/200) ))
}

#plot vertical segments of errorbars
segments(log2(speeds), mean_rates_spd-sdspeed,log2(speeds), mean_rates_spd+sdspeed)
#width of top/bottom of errorbars
segwidth = 0.02
segments(log2(speeds)-segwidth,mean_rates_spd-sdspeed,log2(speeds)+segwidth,mean_rates_spd-sdspeed)
segments(log2(speeds)-segwidth,mean_rates_spd+sdspeed,log2(speeds)+segwidth,mean_rates_spd+sdspeed)


#errorbars are wide: what if we take just one direction (preferred direction, index 13)?
mean_rates_1spd<-as.vector(spikecount_mean[13,])*(1000/200)

print(
  points(log2(speeds),
       mean_rates_1spd,
       main = paste('Speed tuning curve'),
       ylim=c(0,80),
       col='red'
  )
)

#get sd again
sd1speed<-vector()
for (speed in 1:length(speeds)){
  sd1speed<-c(sd1speed,sd(spikecount_sum[20,speed,]*(1000/200) ))
}

#plot vertical segments of errorbars
segments(log2(speeds), mean_rates_1spd-sd1speed,log2(speeds), mean_rates_1spd+sd1speed,col='red')
#width of top/bottom of errorbars
segwidth = 0.02
segments(log2(speeds)-segwidth,mean_rates_1spd-sd1speed,log2(speeds)+segwidth,mean_rates_1spd-sd1speed,col='red')
segments(log2(speeds)-segwidth,mean_rates_1spd+sd1speed,log2(speeds)+segwidth,mean_rates_1spd+sd1speed,col='red')
l1<-'all directions'
l2<-'preferred directions'
legend('topleft', c(l1,l2),lty=1, col=c('black', 'red'), bty='n', cex=.75)
       
#look at spike count for all directions and speeds
#use filled.contour to plot heatmap
#colors pre-calibrated to range from blue (low spike count) to orange/red(high spike count)

filled.contour(directions,log2(speeds),spikecount_mean,nlevels=30,
                    col=rev(rainbow(30,start=0,end=0.7)),
                    plot.title = title(main = 'Joint direction-speed tuning'),
                    xlab =  'direction (degrees)',
                    ylab = expression('log'[2]*'speed (dps)')
)




#how does this change over time?
#to keep axis constant over time, first find the max spike count in any bin
binsize<-10
spikes_rot<-aperm(spikes_cell,c(4,1,2,3))
spikect_binmean<-array(0,c(dim(spikes_rot)[1]-binsize,length(directions),length(speeds)))
for (timebin in 1:(dim(spikes_rot)[1]-binsize)){
  spikect_binsum<-colSums(spikes_rot[timebin:(timebin+binsize),,,],dims=1)
  spikect_binmean[timebin,,]<-rowMeans(spikect_binsum,dims=2)
}
maxsps<-max(spikect_binmean)

#toString to convert numbers to strings
#sys.sleep(seconds)
#you can speed up the animation by changing the increment value in seq
numlevels<-30
for (timebin in seq(1,(dim(spikes_rot)[1]-binsize),2)){
  filled.contour(directions,log2(speeds),spikect_binmean[timebin,,],levels=seq(0,maxsps,(maxsps/numlevels)),
                 plot.title = title(main = paste('spike count from',toString(timebin),'to',toString(timebin+binsize),'ms'),
                                    xlab =  'direction (degrees)',
                                    ylab = 'log(speed) (degrees per second)',),
                 col=rev(rainbow(numlevels,start=0,end=0.7)))
  Sys.sleep(0.1)
}

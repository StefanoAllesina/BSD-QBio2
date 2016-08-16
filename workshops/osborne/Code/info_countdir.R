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

# create a Poisson shuffled version of the data for the optional part of the
# exercise
data_shuffle <- array(0, dim(mydata))

for (dind in 1:nDirs){
  for (tind in 1:dim(mydata)[1]){
   index <-  sample(1:nReps[dind]) # shuffle the indices from 1 to nReps[dind]
   data_shuffle[tind, dind, 1:nReps[dind]] <- mydata[tind, dind, index]
  }
}

#For Exercise 5
# use t= 1 to 350 for neuron 1 so we don't consider the spiking after the stimulus is
# over
# neuron 2 has 256 time points
Time <- 1:350
cumcounts<-array(0,c(350, nDirs, max(nReps)))
cumcounts_shuffle<-array(0,c(350, nDirs, max(nReps)))

for (dind in 1:nDirs){
  cumcounts[,dind,] <- apply(mydata[Time,dind, ],2,cumsum)
  cumcounts_shuffle[,dind,] <- apply(data_shuffle[Time,dind, ],2,cumsum)
  }
maxcount <-max(cumcounts,cumcounts_shuffle)



countbins <-seq(0,maxcount+4,4) #26 bins for neuron 1
# how many bins can we use?  could we use 0:maxcount (as many bins as the
# count)?  it depends on how many repetitions we have and how variable the
# response is.  If you have time, you can try different binning and see how
# it changes your results.
ncountbins <-length(countbins)-1;

probcumcounts <- array(0,c(length(Time),nDirs,ncountbins))
probcumcounts_shuffle <-probcumcounts;
for (n in 1:length(Time)){
  for (m in 1:nDirs){
    probcumcounts[n,m,] <-hist(cumcounts[n,m,1:nReps[m]],breaks=countbins,plot=FALSE)$counts/nReps[m]
    probcumcounts_shuffle[n,m,] <-hist(cumcounts_shuffle[n,m,1:nReps[m]],breaks=countbins,plot=FALSE)$counts/nReps[m]
  }
}

pdir <-array(0,c(nDirs,1)) #the probability of each direction is not uniform
for (n in 1:nDirs){
  pdir[n] <-nReps[n]/sum(nReps)
}


# a basic algorithm without finite sample size correction to compute the 
# mutual information between the cumulative spike count and
# motion direction over time
#initialize arrays
Icount_dir <-array(0,c(length(Time),1))
Icount_dir_shuffle <-array(0,c(length(Time),1))
I <-Icount_dir
I_shuffle <-I
Pcount_given_dir <-array(0,c(ncountbins,nDirs))
Scount<-array(0,length(Time))
Scount_given_dir<-array(0,c(length(Time),nDirs))
Pcount_given_dir_shuffle <-array(0,c(ncountbins,nDirs))
Scount_shuffle<-array(0,length(Time))
Scount_given_dir_shuffle<-array(0,c(length(Time),nDirs))


for (tind in 1:length(Time)){
  #find probability distribution (PDF) of cumulative count for each time step across all directions
  P<-probcumcounts[tind,,]
  #normalize
  P<- P/sum(P)
  #joint probability distribution, computed from P(theta) and P(count)
  jt<-(t(rowSums(P)) %o% colSums(P))[1,,]
  #constant, epsilon=2.2E-16, used to prevent dividing by 0
  eps<-.Machine$double.eps
  
  #Equation 3.5 in tutorial
  #mutual info
  Icount_dir[tind] <- sum(P*log2(P/(jt+eps) +eps))
  
  #or less compactly
  #normalized PDF of cumulative counts
  Pcount <-t(colSums(P))
  Pcount <-Pcount/sum(Pcount)
  
  #Equation 3.1 
  #entropy of cumulative counts: PlogP
  Scount[tind] <- -sum(Pcount*log2(Pcount+eps))
  
  #normalized PDF of directions
  Pdir <- rowSums(P)
  Pdir <- Pdir/sum(Pdir)
  
  for (dind in 1:nDirs){
    
    #normalized PDF of conditional joint distribution of count and direction
    Pcount_given_dir[,dind] <-P[dind,]/(sum(P[dind,])+eps)
    
    #Equation 3.2
    #entropy of conditional joint distribution
    Scount_given_dir[tind,dind]<- -sum(Pcount_given_dir[,dind]*log2(Pcount_given_dir[,dind]+eps))
    
    #Equation 3.5
    #mutual info
    I[tind] <- I[tind] + Pdir[dind]*sum(Pcount_given_dir[,dind]*log2(Pcount_given_dir[,dind]/(Pcount +eps) +eps))
  } 
  
  #compute Poisson shuffle as above
  P_shuffle<-probcumcounts_shuffle[tind,,]
  P_shuffle<- P_shuffle/sum(P_shuffle)
  jt_shuffle<-(t(rowSums(P_shuffle))%o% colSums(P_shuffle))[1,,]
  Icount_dir_shuffle[tind] <- sum(P_shuffle*log2(P_shuffle/(jt_shuffle+eps) +eps))
  #or less compactly
  #normalized PDF of cumulative counts
  Pcount_shuffle <-t(colSums(P_shuffle))
  Pcount_shuffle <-Pcount/sum(Pcount_shuffle)
  
  #Equation 3.1 
  #entropy of cumulative counts: PlogP
  Scount_shuffle[tind] <- -sum(Pcount_shuffle*log2(Pcount_shuffle+eps))
  
  #normalized PDF of directions
  Pdir_shuffle <- rowSums(P_shuffle)
  Pdir_shuffle <- Pdir_shuffle/sum(Pdir_shuffle)
  
  for (dind in 1:nDirs){
    
    #normalized PDF of conditional joint distribution of count and direction
    Pcount_given_dir_shuffle[,dind] <-P_shuffle[dind,]/(sum(P_shuffle[dind,])+eps)
    
    #Equation 3.2
    #entropy of conditional joint distribution
    Scount_given_dir_shuffle[tind,dind]<- -sum(Pcount_given_dir_shuffle[,dind]*log2(Pcount_given_dir_shuffle[,dind]+eps))
    
    #Equation 3.5
    #mutual info
    I_shuffle[tind] <- I_shuffle[tind] + Pdir_shuffle[dind]*sum(Pcount_given_dir_shuffle[,dind]*log2(Pcount_given_dir_shuffle[,dind]/(Pcount_shuffle +eps) +eps))
  } 
}

#set up plot axes
print(
  plot(Time,I,
       xlab = 'time from motion onset (ms)',
       ylab = 'information (bits)',
       main = 'Mutual information between count and direction',
       type="n"))

#plot information from neuron
#this should look like Fig 3C from Osborne et al. 2004, in the Readings folder
print(
  lines(Time, # x-axis
       I, #y-axis
       col="black",
       lwd=2
       )
  )
#plot information from Poisson-shuffled neuron
print(
  lines(
       Time, # x-axis
       I_shuffle, #y-axis
       col="red",lwd=2
  )
)
#add a legend
legend(x="bottomright",y=NULL,c("neuron","Poisson model"),lty=c(1,1),lwd=c(2,2),col = c("black","red"))

                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
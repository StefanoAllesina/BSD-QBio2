
# load the data
# you can use either example neuron, but _2 will give a pronounced difference between
# the real data and the Poisson shuffle model that you will be creating

load("../Data/MTneuron_2.RData")

# create a Poisson shuffled version of the data for the optional part of the
# exercise
data_shuffle <- array(0, dim(mydata))  #start with an array of 0s

nDirs <- length(directions)
nReps <- rep(0, nDirs)
for (n in 1:nDirs){
  nReps[n] <- 184
}

for (dind in 1:nDirs){
  for (tind in 1:dim(mydata)[1]){  
   index <-  sample(1:nReps[dind]) # shuffle the indices from 1 to nReps[dind]
   data_shuffle[tind, dind, 1:nReps[dind]] <- mydata[tind, dind, index]
  }
}

#For Exercise 5
# use t = 1 to 350 for neuron 1 so we don't consider post-stimulus spiking 
# neuron 2 has 256 time points

Time <- 1:dim(mydata)[1]
cumcounts<-array(0,c(length(Time), nDirs, max(nReps)))
cumcounts_shuffle<-array(0,c(length(Time), nDirs, max(nReps)))

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

# Use the hist function to translate the data into a probability distribution
# hist will count the number the values that fall into each bin you have defined
# diving by the total number of values gives you the fraction
probcumcounts <- array(0,c(length(Time),nDirs,ncountbins))
probcumcounts_shuffle <-probcumcounts;
for (n in 1:length(Time)){
  for (m in 1:nDirs){
    probcumcounts[n,m,] <-hist(cumcounts[n,m,1:nReps[m]],breaks=countbins,plot=FALSE)$counts/nReps[m]
    # see the text for a guide to the hist command
    # breaks are giving bin edges
    probcumcounts_shuffle[n,m,] <-hist(cumcounts_shuffle[n,m,1:nReps[m]],breaks=countbins,plot=FALSE)$counts/nReps[m]
  }
}



# Here is a basic algorithm without finite sample size correction to compute the 
# mutual information between the cumulative spike count and
# motion direction over time

#initialize arrays
Icount_dir <-array(0,c(length(Time),1))
Icount_dir_shuffle <-array(0,c(length(Time),1))
I <-Icount_dir
I_shuffle <-I
I_diff <- I
Pcount_given_dir <-array(0,c(ncountbins,nDirs))
Scount<-array(0,length(Time))
Scount_given_dir<-array(0,c(length(Time),nDirs))
Snoise<-Scount
Pcount_given_dir_shuffle <-array(0,c(ncountbins,nDirs))
Scount_shuffle<-array(0,length(Time))
Scount_given_dir_shuffle<-array(0,c(length(Time),nDirs))


for (tind in 1:length(Time)){
  #find probability distribution of cumulative count for each time step across all directions
  # dimensions are time by dirs by count bins
  
  P<-probcumcounts[tind,,]   #dirs by count bins
  
  # a probability distribution sums to 1
  # HOW you normalize probcumcounts will determine whether you 
  # create P(count | theta) the prob. of the count value given the direction
  # or P(count,theta) the joint probability of count and direction
  # remember that P(count,theta) = P(count | theta) P(theta) 
  # by Bayes Theorem
  
  #joint probability distribution, P(theta,count)
  jt<-(t(rowSums(P)) %o% colSums(P))[1,,]
  # all probability distributions need to sum to 1
  jt <- jt/sum(jt)
  
  #constant, epsilon=2.2E-16, used to prevent dividing by 0
  eps<-.Machine$double.eps
  
  #Equation 3.5 in tutorial
  #The simplest way to compute the mutual information is to
  #use the joint distribution of counts and directions

  Icount_dir[tind] <- sum(P*log2(P/(jt+eps) +eps))
  
  #or less compactly, we can create each of the probability distributions we need
  # then use them to form the total entropy of the count, S(P(n))
  # and the conditional (noise) entropy of the count given each direction, S(P(n|theta),
  # averaged over directions, sum_over_theta P(theta) S(P(n | theta))
  
  #normalized PDF of cumulative counts
  Pcount <-t(colSums(P))
  Pcount <-Pcount/sum(Pcount)
  
  #Equation 3.1 
  #entropy of cumulative counts: -PlogP
  Scount[tind] <- -sum(Pcount*log2(Pcount+eps))
  
  #normalized PDF of directions, which you need in order to
  #compute the average noise entropy
  Pdir <- rowSums(P)
  Pdir <- Pdir/sum(Pdir)
  
  #a loop to compute the conditional entropy S(P(count | theta))
  for (dind in 1:nDirs){
    
    # you can form conditional distributions, i.e. P( count | theta)
    # from the joint distribution P(count, theta) by selecting a value of theta
    # and then normalizing the column to 1
    Pcount_given_dir[,dind] <-P[dind,]/(sum(P[dind,])+eps)
    
    #Equation 3.2
    #entropy of conditional joint distribution
    Scount_given_dir[tind,dind]<- -sum(Pcount_given_dir[,dind]*log2(Pcount_given_dir[,dind]+eps))
    
    #second term of Equation 3.3: noise entropy, or the entropy of the conditional joint distribution,
    #weighted by the probability of the stimulus presentation
    #this will get used to make I_diff after this loop is done
   
    Snoise[tind]<-Snoise[tind] + Scount_given_dir[tind,dind]*Pdir[dind]
    
    
    #Equation 3.5
    #mutual info can be calculated this way
    I[tind] <- I[tind] + Pdir[dind]*sum(Pcount_given_dir[,dind]*log2(Pcount_given_dir[,dind]/(Pcount +eps) +eps))
    
  } 
  
  
  #Equation 3.3
  #The mutual information about stimulus direction and a neuron's spike count can also be expressed as
  # the difference between the 
  #total entropy of the observed spike count (Eq3.1) and the noise entropy (Eq 3.2): equivalent to Eq3.5
  I_diff[tind]<-Scount[tind]-Snoise[tind]
  
  #check to see that I_diff and I are identical
  
  
  #compute Poisson shuffle as above
  P_shuffle<-probcumcounts_shuffle[tind,,]
  P_shuffle<- P_shuffle/sum(P_shuffle)
  jt_shuffle<-(t(rowSums(P_shuffle))%o% colSums(P_shuffle))[1,,]
  Icount_dir_shuffle[tind] <- sum(P_shuffle*log2(P_shuffle/(jt_shuffle+eps) +eps))
  #or less compactly
  #normalized PDF of cumulative counts
  Pcount_shuffle <-t(colSums(P_shuffle))
  Pcount_shuffle <-Pcount_shuffle/sum(Pcount_shuffle)
  
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
  plot(2*Time,I,
       xlab = 'time from motion onset (ms)',
       ylab = 'information (bits)',
       ylim = c(0, 0.8),
       main = 'Mutual information between count and direction',
       type="n"))

#plot information from neuron
#this should look like Fig 3C from Osborne et al. 2004, in the Readings folder
print(
  lines(2*Time, # x-axis
       I, #y-axis
       col="black",
       lwd=2
       )
  )
#plot information from Poisson-shuffled neuron
print(
  lines(
       2*Time, # x-axis
       I_shuffle, #y-axis
       col="red",lwd=2
  )
)
#add a legend
legend(x="bottomright",y=NULL,c("neuron","Poisson model"),lty=c(1,1),lwd=c(2,2),col = c("black","red"))

                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
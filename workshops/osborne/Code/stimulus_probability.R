#histograms of cumulative spike count

load("../Data/MTneurons8.RData")

cellnum<-1
spikes_cell<-spikes[,,1:nReps[cellnum],,cellnum]
spikecount_sum<-rowSums(spikes_cell,dims=3)

spikehist<-array(0,c(length(directions),length(speeds),(max(spikecount_sum)+1)))
#bins to separate spike count (including 0 spikes)
bin_edges<-seq(0,max(spikecount_sum)+1,1)-0.5

for (d in 1:length(directions)){
  for (s in 1:length(speeds)){
    #hist: defining bin edges to bin spike counts
    #hist gives a list of components and plots histogram
    #we extract "counts" component by following hist command with $counts
    #suppress plot
    spikehist[d,s,]<-hist(spikecount_sum[d,s,],bin_edges,plot=FALSE)$counts
  }
}

PnGstim<-spikehist/nReps[cellnum]

theta<-13
v<-6
print(plot(1:dim(PnGstim)[3],PnGstim[theta,v,],
      type = "l", # plot lines
      xlab = 'Spike count',
      ylab = expression(paste('P(n| ',theta,')')),ylim=c(0,0.5))
)

theta<-6
lines(1:dim(PnGstim)[3],PnGstim[theta,v,],col='red')
l1<-expression(paste(' 15',degree,', 32 dps            '))
l2<-expression(paste('-90',degree,', 32 dps            '))
legend('topright',c(l1,l2),lty=1, col=c('black', 'red'), bty='n', cex=.75)
      
#so if this cell fires n spikes in 200ms, what can we infer about the stimulus that was presented?
#safeguard against dividing by zero by adding epsilon (minimum value) to denominator
PstimGn<-array(0,c(length(directions),length(speeds),(max(spikecount_sum)+1)))

for (n in 1:(max(spikecount_sum)+1)){
PstimGn[,,n]<-spikehist[,,n]/(sum(spikehist[,,n])+.Machine$double.eps)
}

#visual: cycle through number of spikes fired (or take mean spike count from specific stimulus)

for (n in 1:(max(spikecount_sum)+1)){
  
  
  filled.contour(directions,log2(speeds),PstimGn[,,n],levels=seq(0,0.2,0.2/30),
                 plot.title = title(main = paste('stimulus probability from',toString(n-1),'spikes')),
                                    xlab =  'direction (degrees)',
                                    ylab = 'log(speed) (degrees per second)',
                 col=rev(rainbow(30,start=0,end=0.7))
                 )

Sys.sleep(0.1)
}
#how is this affected by different spike count binning? (change levels)
#consider that a stimulus that elicits 10 spikes on average would also have an SD of +/-3 spikes

numcells<-8
#how does our uncertainty of estimating stimulus decrease when we use multiple cells?
#repeat process above for all cells

spikecount_sum_allcells<-array(0,c(length(directions),length(speeds),max(nReps),numcells))
spikecount_mean_allcells<-array(0,c(length(directions),length(speeds),numcells))

for (cellnum in 1:numcells){
  spikes_cell<-spikes[,,1:nReps[cellnum],,cellnum]
  spikecount_sum_allcells[,,1:nReps[cellnum],cellnum]<-rowSums(spikes_cell,dims=3)
  spikecount_mean_allcells[,,cellnum]<-rowMeans(spikecount_sum_allcells[,,1:nReps[cellnum],cellnum],dims=2)
}

bin_edges<-seq(0,max(spikecount_sum_allcells)+1,1)-0.5
spikehist_allcells<-array(0,c(length(directions),length(speeds),length(bin_edges)-1,numcells))
PstimGn_allcells<-array(0,c(length(directions),length(speeds),length(bin_edges)-1,numcells))

for (cellnum in 1:numcells){
  #calculate histograms
  for (d in 1:length(directions)){
    for (s in 1:length(speeds)){
      spikehist_allcells[d,s,,cellnum]<-hist(spikecount_sum_allcells[d,s,1:nReps[cellnum],cellnum],bin_edges,plot=FALSE)$counts
    }
  }
  #normalize for each spike count bin
  for (n in 1:dim(spikehist_allcells)[3]){
    PstimGn_allcells[,,n,cellnum]<-spikehist_allcells[,,n,cellnum]/(sum(spikehist_allcells[,,n,cellnum])+.Machine$double.eps)
  }
}

#what are the relative probabilities for the estimate of a stimulus given the response from these cells?
#dirindex 16, spdindex 4
for (dirindex in 1:length(directions)){
  for (spdindex in 1:length(speeds)){
    #for each stimulus value, make empty stimprob_estim to generate distribution
    stimprob_estim<-array(0,c(length(directions),length(speeds)))
    for (cellnum in 1:numcells){
      spikect_estim<-round(spikecount_mean_allcells[dirindex,spdindex,cellnum])
      if (sum(PstimGn_allcells[,,spikect_estim,cellnum])>0){
        stimprob_estim<-stimprob_estim+PstimGn_allcells[,,spikect_estim,cellnum]
      }
    }
    
    stimprob_estim<- stimprob_estim/sum(stimprob_estim) #normalization
    
    filled.contour(directions,log2(speeds),stimprob_estim,levels=seq(0,max(stimprob_estim),max(stimprob_estim)/30),
                   plot.title = title(main = 'stimulus probability'),
                   xlab =  'direction (degrees)',
                   ylab = 'log(speed) (degrees per second)',
                   col=rev(rainbow(30,start=0,end=0.7))
    )
    
    Sys.sleep(1)
  }
}

#how do these estimates change depending on how much of the response you use?

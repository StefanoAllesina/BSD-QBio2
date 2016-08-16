# 1) RFmap

# load the data
load("../Data/MTneuron.RData")

# see what has been loaded
print(ls())

# There are four objects:
# directions is a vector containing directions of motion, from -180 to 165 in steps of 15 degrees
# dirtune is a matrix with 824 rows (trials) and 109 columns (spike times) containing spike time for that trial
# theta is a vector of 824 elements indexing which element of the directions variable is associated with each trial
# Finally, RFMap is a 4-dimensional array 
# 10 (y position) x 15 (x position) x 16 (repetitions) x 24 (max number of spikes) containing spike times

# initialize a matrix with dimensions equal to the first two in RFmap (x and y coordinates)
numspks <- matrix(0, dim(RFmap)[1], dim(RFmap)[2]) 
# find numbnumspkser of trials
nTrials <- dim(RFmap)[3]
# find max number of spikes
max_num_spks <- dim(RFmap)[4]

#  To count the number of spikes at each location on the grid, write a for loop that will step though the
#  x and y positions on the grid.  Then at each position find the number of non-zero entries in the array RFmap.
#  The number of y positions in the grid is 10, i.e. dim(numspks)[1]  or dim(RFmap)[1]
#  The number of x positions in the grid is 15, i.e. dim(numspks)[2] or dim(RFmap)[2]
#  You can certainly just use 10 and 15, but if you were trying to write a script that would give you RFmaps from 
#  differently sized grids, your code wouldn't run!  Sometimes it is helpful to stay general

for (yind  in 1:dim(numspks)[1]){
  for (xind in 1:dim(numspks)[2]){
    # We care about how many spikes were fired at each grid position, not which
    # stimulus repeat they were fired on.  So let's count how many spikes are 
    # in the RFmap at each grid location. 
    # Note that RFmap[yind, xind, , ] is a matrix. We just count how
    # many values in the matrix are not zero, and store the value in numspks[yind,xind]
    
   #  inds <- sum(RFmap[yind,xind,,] != 0)    # != means not equal.  You could also use >0 because there
                                              # are no negative spike times in the array RFmap
    
    # We can be more accurate with the map we build if we only count spikes that are driven by the 
    # stimulus.  The latency of this neuron is about 50ms, so spike times < 45ms are not a result of the 
    # motion. Create a temporary variable "inds" that stores the (non-zero) spike times > 45ms
    
    inds <- sum(RFmap[yind,xind,,] > 45)
    
    # You will want to store the number of spikes you have counted for each grid location
    # into the array numspks that you made (full of zeros for now)
    
    numspks[yind, xind] <- inds
    
    # There are many ways of getting to the correct spike count.  Another way would be to use the 
    # command "which" that returns the indices of the spike times that satisfy the condition >45.
    # The R operation length will then tell you long that list is
    
    # numspks[yind,xind] <- length(which(RFmap[yind,xind,,] > 45))
  }
}

# To map the locations,where we should plot the values that are now stored in the 10 by 15 matrix numspks,
# we create a vector from -14 degrees in the visual field to 14 in steps of 2 degrees (see the description of the 
# experiment on pp.1-2 of the Osborne Workshop notes)

x <- seq(-22, 6, by = 2) 
# We do the same for the y axis, but now we're limited to -9, +9
y <- seq(-2, 16, by = 2) 

# For plotting, we want to modify the data slightly:
# first, now numspks has y coordinates in the rows, 
# and x coordinates in the columns because MATLAB likes it that way, while R likes it 
# to be x -> rows and y -> columns.  
# Transposing the matrix will please R
numspks <- t(numspks)

# Also, the plotting routine treats the grid position [1,1] as the bottom-left corner,
# while we want it to be the upper-left corner (again, a MATLAB thing)
# This fixes the problem
numspks <- numspks[,(dim(numspks)[2]):1]

# A simple plot uses the funcion image, which plots a matrix
# to see how to use it, type ?image
print(
image(x, y, numspks / nTrials, 
            main = "RF map of an MT neuron", 
            xlab = "degrees", 
            ylab = "degrees",
            col = rev(rainbow(27,start=0,end=0.7))
)
)
# notice that we have divided by the number of times in which the stimulus was repeated 
# at each grid location.  That yields the average spike count.  The total number of spikes across
# all reps would give you an identical looking plot!

# a fancier plot can be done using 
# ?filled.contour
# wich automatically smooths the data

print(
filled.contour(x, y, numspks/nTrials, nlevels = 25,
               plot.title = title(main = "RF map of an MT neuron",
                                  xlab = "degrees", ylab = "degrees"),
               # choose colors
               col = rev(rainbow(28,start=0,end=0.7)),
               # add a point
               plot.axes={
                 axis(1); # plot the x-axis
                 axis(2); # plot the y axis
                 # The center of the visual field of the monkey was at (7.5, -7.5)
                 # Let's put a + sign to mark the spot
                 points(0, 0, pch = "+", cex = 2, col = "white", font = 2)}
               )

)
# 1) RFmap

# load the data
load("../Data/MTneuron.RData")

# see what has been loaded
print(ls())

# There are four objects:
# directions is a vector containing directions of motion, from -180 to 165 in steps of 15 degrees
# dirtune is a matrix with 824 rows (trials) and 109 columns (spike times) containing spike time for that trial
# theta is a vector of 824 elements indexing which element of the directions variable is associated with each trial
# Finally, RFMap is a 4-dimensional array 10 (y position) x 15 (x position) x 16 (repetitions) x 24 (max number of spikes) containing spike times

# initialize a matrix with dimensions equal to the first two in RFmap (x and y coordinates)
numspks <- matrix(0, dim(RFmap)[1], dim(RFmap)[2]) 
# find number of trials
nTrials <- dim(RFmap)[3]
# find max number of spikes
max_num_spks <- dim(RFmap)[4]

for (yind  in 1:dim(numspks)[1]){
  for (xind in 1:dim(numspks)[2]){
    # We care about how many spikes were fired at each grid position, not which
    # stimulus repeat they were fired on.  So let's count how many spikes are 
    # in the RFmap at each grid location. 
    # Note that RFmap[yind, xind, , ] is a matrix. We just count how
    # many values in the matrix are not zero, and store the value in numspks[yind,xind]
    inds <- which(RFmap[yind,xind,,] > 45)
    numspks[yind, xind] <- length(inds)
  }
}

# To map the locations, we create a vector from -14 degrees in the visual field to 14 in steps of 2 degrees
x <- seq(-22, 6, by = 2) 
# We do the same for the y axis, but now we're limited to -9, +9
y <- seq(-2, 16, by = 2) 

# For plotting, we want to modify the data slightly:
# first, now numspks has y coordinates in the rows, 
# and x coordinates in the columns, while R likes it 
# to be x -> rows and y -> columns.
# Transposing the matrix will please R
numspks <- t(numspks)

# Also, the plotting routine treats the cell [1,1] as the bottom-left corner,
# while we want it to be the upper-left corner
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

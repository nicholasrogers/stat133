#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
 grid.size <- r*c 
 num.cars.vector <- replicate(grid.size, sample(0:1, 1, prob = c(1-p, p)))
#this function does not allow for zero cars on a grid
   for(i in 1:grid.size){
     if(num.cars.vector[i] == 1){
       num.cars.vector[i] <- sample(1:2, 1, prob = c(.5, .5))
     } 
  }
 m <- matrix(num.cars.vector, nrow = r, ncol = c)
 return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
n <- m 
for(i in 1:nrow(m))
  {
    for(j in 1:(ncol(m) - 1))
      {
        if(m[i, j] == 1)
          {
            if(m[i, j+1] == 0)
              {
                n[i, j] <- 0 
                n[i, j+1] <- 1 
              }
          }
      }
    if(m[i, ncol(m)] == 1)
      {
        if(m[i, 1] == 0)
          {
            n[i, ncol(m)] <- 0
            n[i, 1] <- 1 
          }
      }
  }
o <- n 
for(j in 1:ncol(n))
{
  for(i in 2:nrow(n))
  {
    if(n[i, j] == 2)
    {
      if(n[i-1, j] == 0)
      {
        o[i, j] <- 0
        o[i-1,j] <- 2
      }
    }
  }
  if(n[1, j] == 2)
  {
    if(n[nrow(n), j] == 0)
    {
      o[1, j] <- 0
      o[nrow(n), j] <- 2
    }
  }
}
grid.fresh <- o == m
grid.new <- FALSE %in% grid.fresh
return(list(o, grid.new))
}

#image(t(apply(o[[1]], 2, rev)), col = c("White", "Red", "Blue"))

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){

}

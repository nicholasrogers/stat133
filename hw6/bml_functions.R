#################################################################################
#### Functions for BML Simulation Study

############NOTE: This is a functin I wrote in order to handle the unique cases of matricies where there are only 2 unique 
#numbers or 1 row and n collumns such that the colours are always assinged the same way, 0 to White, 1 to 
#Red, and 2 to Blue. 
####I have built the image functions into the bml.init and bml.sim functions, but comment them out such that
####they are replicated say 100 times, it doesn't plot 100+ images. 
matrix.image <- function(m, lock)
{
  x <- (0 %in% m) 
  y <- (1 %in% m) 
  z <- (2 %in% m)  
  if(x == TRUE)
  {
    if(y == TRUE)
    {
      if(z == TRUE)
      {
        clrs = c("White", "Red", "Blue") 
      }
      if(z == FALSE)
      {
        clrs = c("White", "Red") 
      }
    }
    if(y == FALSE)
    {
      if(z == TRUE)
      {
        clrs = c("White", "Blue")
      }
      if(z == FALSE)
      {
        clrs = c("White")
      }
    }
  }
  if(x == FALSE)
  {
    if(y == TRUE)
    {
      if(z == TRUE)
      {
        clrs = c("Red", "Blue") 
      }
      if(z == FALSE)
      {
        clrs = c("Red") 
      }
    }
    if(y == FALSE)
    {
      if(z == TRUE)
      {
        clrs = c("Blue")
      }
      
    }
  }
  if(nrow(m) == 1)
  {
    image(t(m), col = clrs)
  } 
  else {
    image(t(apply(m, 2, rev)), col = clrs, main = c(lock))
  }
}


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)






bml.init <- function(r, c, p){
  stopifnot(r>=0, c>=0, p>=0, 1>=p)
 grid.size <- r*c 
 num.cars.vector <- replicate(grid.size, sample(0:1, 1, prob = c(1-p, p)))
   for(i in 1:grid.size){
     if(num.cars.vector[i] == 1){
       num.cars.vector[i] <- sample(1:2, 1, prob = c(.5, .5))
     } 
  }
 m <- matrix(num.cars.vector, nrow = r, ncol = c)
matrix.image(m, "Initial State of System") 
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
if(ncol(m) > 1)
{
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
}
if(ncol(m) == 1)
{ 
  n <- m
}
o <- n 
if(nrow(n) > 1) 
{
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
}
if(nrow(n) == 1)
{
  o <- n
}
grid.fresh <- o == m
grid.new <- FALSE %in% grid.fresh
return(list(o, grid.new))
}


#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

#t is the number of steps the functions runs and the output i is the last step number taken. 
#It only outputs a plot if the system results in gridlock. If the system is free flowing, the output will
#return true. 

bml.sim <- function(r, c, p, t = 10000) 
  {
     m <- bml.init(r, c, p)
     n <- m
     o <- bml.step(n)
     i <- 1
     while(o[[2]] == TRUE & (i < t))
     {
       m <- o[[1]]
       n <- m 
       o <- bml.step(n)
       i <- i + 1
     }
     if(o[[2]] == FALSE)
     {
       matrix.image(o[[1]], "Gridlocked System")
     }
     return(list(o[[2]], i))
  }
       
     
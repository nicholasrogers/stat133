#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

#### This function performs a replication of the simulation for a given [n] number of times. 
#### It returns a list with three elements. The first of which is the proportion of gridlock systems
#### of the [n] replications. The seoncond are summary statstics on the number of steps it took the gridlock
#### systems to reach gridlock. The third is the standard deviation fo the number of steps it took the 
#### gridlock systems to reach gridlock. 

bml.repl <- function(n, r, c, p)
{
  repl <- replicate(n, bml.sim(r, c, p))
  num.ff <- sum(unlist(repl[1,]))
  num.gl <- n - num.ff 
  gl.p <- num.gl/n 
  ngltrials <- (unlist(repl[2,]) - 10000)*(-1)
  ngltsumm <- summary(ngltrials)
  ngltsd <- sd(ngltrials)
  return(list(gl.p, ngltsumm, ngltsd))
}

bml.repl(100, 10, 10, .32) 
bml.repl(100, 10, 10, .5)
bml.repl(100, 10, 10, .8)

bml.repl(100, 20, 20, .32)
bml.repl(100, 20, 20, .5)
bml.repl(100, 20, 20, .8)



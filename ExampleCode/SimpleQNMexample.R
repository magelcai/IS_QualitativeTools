#Q Press example 

# Load required packages
library(QPress)
library(tcltk2)
library(XML)
library(here) #folder management

# Load the Dia model
mod <- QPress::model.dia("./DiaModels/ExampleNetwork_AXYZ.dia")
mod <- enforce.limitation(mod) 

## Examine unweighted adjacency matrix
A <- adjacency.matrix(mod)
A

#If model simulation does not exist, simulate
n_sims <- 10000 #number of accepted simulations requested
sims <- QPress::system.simulate(n_sims, mod)

sims$total # total number of runs to produce x accepted runs
sims$accepted # This should be the number specified by user in system.simulate


impact.barplot(sim = sims)

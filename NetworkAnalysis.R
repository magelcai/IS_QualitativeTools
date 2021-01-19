#####
# Network Analysis for Magel & Francis
# Cross-IS ecosystem model

#QPress package is based on the analysis from Melbourne-Thomas et al 2012
#https://wiley.figshare.com/articles/dataset/Supplement_1_Example_R_code_and_models_/3568107/1
#https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/12-0207.1?casa_token=VHTR6o_DvQ8AAAAA%3A_vmeRY9Q0b_fiUK3YpRBnKSq5_q-armBNM4ik05kN15SkrRXWQL-uUfmJyENGHCm_gN0-VY8rh-amw

# Load required packages
library(QPress)
library(tcltk2)
library(XML)
library(here) #folder management


# Load the Stormwater/Wastewater Dia model
mod <- QPress::model.dia("./DiaModels/Network_12Jan2021_forQPress.dia")


## Examine unweighted adjacency matrix
A <- adjacency.matrix(mod)
A

mod <- enforce.limitation(mod) #I think this is redundant with the self-limiting edges in the Dia model


#If model simulations already exist, load them
sims <- readRDS("SSWW_Sims_20201218.rds")

#If model simulation does not exist, simulate and save!
n_sims <- 1000 #number of accepted simulations requested
sims <- QPress::system.simulate(n_sims, mod)


sims$total # total number of runs to produce x accepted runs
sims$accepted # This should be the number specified by user in system.simulate
sims$stable # should be the same number as accepted?

impact.barplot(sim = sims) # exploratory widget
weight.density(sim = sims) # to produce density plots of edge weights for all the simulations


#Extract the weight values in the accepted model runs:
sims$edges
head(sims$w)
mean(abs(sims$w))

#save simulations
saveRDS(sims, file = paste("Sims_", n_sims, "_", Sys.Date(), ".rds", sep = ""))

##################################################################################################
##################################################################################################

# For generating plots that are not part of the QPress package, use code below
# Example code was provided by Ben Raymond, but this came from K. Sobocinski

# Extract the bits we need
edges <- sims$edges
As <- sims$A
nodes <- node.labels(edges)
monitor <- c(rep(NA,length(nodes))) ## Don't enforce any required responses


#Call specific nodes of interest
#To show only a subset of node responses (e.g. 12, 13, 27, 4, 31), run this instead of standard plot:
myplot <- function(nodes,As,perturb,monitor,epsilon=1.0E-5,main="",cex.axis=1) {
  pal <- c("firebrick4", "#808080", "lightblue")
  results <- matrix(0,length(nodes),3)
  for(i in 1:length(As)) {
    impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
    if(all(monitor==impact,na.rm=T)) {
      results <- results + outer(impact,-1:1,'==')
    }
  }
  rownames(results) <- nodes
  nodes <- nodes[c(17,2,13,24,4,27,32,11,10,31)] #this is where you specify the nodes of interest
  results <- results[c(17,2,13,24,4,27,32,11,10,31),] #this is where you specify the nodes of interest
  lwidth <- max(strwidth(nodes,units="inches",cex=cex.axis))
  opar <- par(mai=c(0.5,lwidth+0.15,0.15,0.15)+0.2)
  barplot(t(results),horiz=T,las=1,border=F,col=pal,
          xlab="Simulations",main=main,cex.axis=cex.axis)
  par(opar)
}

# Standard slider plot of all response nodes, perturbing each in turn given the vector of perturbations (press):
# windows()
# To output to PDF
currentDate <- Sys.Date()
Indiv_Perturb <- paste(currentDate,"_PerturbationPlots_v2",".pdf",sep="")
pdf(file = Indiv_Perturb)
# For function
opar <- par
par(mfrow=c(2,2)) #This can be invoked for a 2x2 layout (better for simple (reduced vars) plot)
for (i in 1:40) { #number of nodes in model
  #i=2
  #Set up desired directions of perturbations--based upon direction of press (-1,1)
  #For all presses (should have 1 per node)
  press=c(1,1,-1,1,1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,-1,1,-1,1,1,1,-1,-1,1,-1,1,-1,1,1,-1,1,-1,-1,1,-1,-1,1)
  
  #length(press)
  presses=diag(press, nrow=40, ncol=40)
  perturb=presses[i,]
  
  perturb2=ifelse(perturb==1,"(Increase)","(Decrease)")
  
  #If all press perturbs are positive, use this code:
  #perturb <- c(rep(0,length(nodes)))
  #perturb[i] <- 1 ## perturb the ith node, this is a positive perturbation
  
  #Choose: all nodes (impact.barplot.action) or a subset of nodes (myplot)--myplot code below
  #impact.barplot.action(nodes,As,perturb,monitor,main=c(nodes[i],perturb[i]))
  myplot(nodes,As,perturb,monitor,main=c(nodes[i],perturb2[i]))
}
par(opar)
dev.off()


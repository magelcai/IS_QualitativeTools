source("./Documents/UW-PSI Postdoc/Qualitative Modeling Projects/IS_QualitativeTools/IS_QualitativeTools/dia.r")
source("./Documents/UW-PSI Postdoc/Qualitative Modeling Projects/IS_QualitativeTools/IS_QualitativeTools/community.r")
source("./Documents/UW-PSI Postdoc/Qualitative Modeling Projects/IS_QualitativeTools/IS_QualitativeTools/tk.r")

#Analysis from Melbourne-Thomas et al 2012
#https://wiley.figshare.com/articles/dataset/Supplement_1_Example_R_code_and_models_/3568107/1
#https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/12-0207.1?casa_token=VHTR6o_DvQ8AAAAA%3A_vmeRY9Q0b_fiUK3YpRBnKSq5_q-armBNM4ik05kN15SkrRXWQL-uUfmJyENGHCm_gN0-VY8rh-amw

library(QPress)
library(tcltk2)
library(XML)
library(here)

#Read model specification
model <- QPress::model.dia("./Documents/UW-PSI Postdoc/Qualitative Modeling Projects/DiaExamples/macquarie.dia")


## Examine unweighted adjacency matrix
A <- adjacency.matrix(model)
A

## Function to generate the community matrix
s <- community.sampler(model)

## Function to check the validation condition
press <- press.validate(model,
                        perturb=c("1"=1),
                        monitor=c("1"=1,"2"=-1))

## Function to define the perturbation scenario
impact <- press.impact(model,perturb=c("1"=-1,"2"=-1,"3"=-1))

## Use 10000 simulations
n.sims <- 10
results <- 0
i <- 0
while(i < n.sims) {
  
  ## Randomly choose edges to retain
  z <- s$select(runif(1))
  ## Sample community matrix
  W <- s$community()
  
  ## Check press condition and stability
  if(!(press(W) && stable.community(W))) next
  
  ## Monitor impact post press
  imp <- impact(W)
  results <- results + outer(sign(imp),-1:1,'==')
  i <- i+1
}

## Print results
rownames(results) <- levels(model$From)
colnames(results) <- c('-','0','+')
results

## Plot outcomes
library(RColorBrewer)
pal <- brewer.pal(n=5,"RdBu")[4:2]
opar <- par(mar=c(5,10,1,1)+0.1)
prop <- results/rowSums(results)
r <- colSums(t(prop)*(-1:1))
barplot(t(prop[order(r),]),
        horiz=T,cex.names=0.8,cex.axis=0.8,las=2,border=F,col=pal,xlab="Proportion")
par(opar)



model <- enforce.limitation(model)
sims <- system.simulate(10,model,
                        validators=list(
                          press.validate(model,
                                         perturb=c("Rats"=1),
                                         monitor=c("Rats"=1)),
                          press.validate(model,
                                         perturb=c("Rats"=1),
                                         monitor=c("Rabbits"=-1, "Tall tussock vegetation"=1))))

impact.barplot(sim = sims)
weight.density(sim = sims)


######
model <- QPress::model.dia("./Documents/UW-PSI Postdoc/Qualitative Modeling Projects/IS_QualitativeTools/IS_QualitativeTools/DiaModels/CombinedNetwork_9Dec2020_forQPress_simple.dia")
model_full <- QPress::model.dia("./DiaModels/CombinedNetwork_16Dec2020_forQPress.dia")
SSWW <- QPress::model.dia("./DiaModels/SWWWNetwork_17Dec2020_forQPress_simpler.dia")


## Examine unweighted adjacency matrix
A <- adjacency.matrix(SSWW)
A

SSWW <- enforce.limitation(SSWW)

sims <- QPress::system.simulate(10000, SSWW)


sims$total # total number of runs to produce x accepted runs
sims$accepted # This should be the number specified by user in system.simulate
sims$stable # should be the same number as accepted?

impact.barplot(sim = sims) # exploratory widget
weight.density(sim = sims) # to produce density plots of edge weights for all the simulations


#Extract the weight values in the accepted model runs:
sims$edges
head(sims$w)
mean(abs(sims$w))



##################################################################################################
##################################################################################################

# For generating plots that are not part of the QPress package, use code below
# Some example code was provided by Ben Raymond

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
Indiv_Perturb <- paste(currentDate,"_PerturbationPlots",".pdf",sep="")
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


#####
# Network Analysis for Magel & Francis - this script works through and compares alternative model structures based on new and redevelopment hypoths
# Cross-IS ecosystem model

#QPress package is based on the analysis from Melbourne-Thomas et al 2012
#https://wiley.figshare.com/articles/dataset/Supplement_1_Example_R_code_and_models_/3568107/1
#https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/12-0207.1?casa_token=VHTR6o_DvQ8AAAAA%3A_vmeRY9Q0b_fiUK3YpRBnKSq5_q-armBNM4ik05kN15SkrRXWQL-uUfmJyENGHCm_gN0-VY8rh-amw
#Some modifications were made to the original QPress code - they are contained in QPressFunctions.R

# Load required packages
library(QPress)
library(tcltk2)
library(XML)
library(here) #folder management


##### Load the Dia models #####
statquo <- QPress::model.dia("./DiaModels/MultiMods/StatusQuo_3Sept2021_forR.dia")
mod_redevB_newdevC <- QPress::model.dia("./DiaModels/MultiMods/Moderate_RedevB+NewDevC_3Sept2021_forR.dia") 

redev_A <- QPress::model.dia("./DiaModels/MultiMods/RedevA_3Sept2021_forR.dia")
redev_B <- QPress::model.dia("./DiaModels/MultiMods/RedevB_3Sept2021_forR.dia") 
redev_C <- QPress::model.dia("./DiaModels/MultiMods/RedevC_3Sept2021_forR.dia") 

newdev_A <- QPress::model.dia("./DiaModels/MultiMods/NewDevA_3Sept2021_forR.dia") 
newdev_B <- QPress::model.dia("./DiaModels/MultiMods/NewDevB_3Sept2021_forR.dia") 
newdev_C <- QPress::model.dia("./DiaModels/MultiMods/NewDevC_3Sept2021_forR.dia") 
newdev_D <- QPress::model.dia("./DiaModels/MultiMods/NewDevD_3Sept2021_forR.dia") 

## Examine unweighted adjacency matrices
A_Statquo <- adjacency.matrix(statquo)
A_Statquo
write.csv(A, file = "Model_AdjMatrix.csv", row.names = FALSE) #save the matrix, ifn needed

## Enforce limitation (This is redundant with the self-limiting edges in the Dia model, if they are already included)
statquo <- QPress::enforce.limitation(statquo)
moderate <- QPress::enforce.limitation(mod_redevB_newdevC)

redev_A <- QPress::enforce.limitation(redev_A)
redev_B <- QPress::enforce.limitation(redev_B)
redev_C <- QPress::enforce.limitation(redev_C)

newdev_A <- QPress::enforce.limitation(newdev_A)
newdev_B <- QPress::enforce.limitation(newdev_B)
newdev_C <- QPress::enforce.limitation(newdev_C)
newdev_D <- QPress::enforce.limitation(newdev_D)

##### Simulations #####
#If model simulations already exist, load them
sims_redev_A <- readRDS("Sims_redev_A10000_2021-08-11.rds")



#If model simulation does not exist, simulate and save!
n_sims <- 10000 #number of accepted simulations requested

sims_statquo <- QPress::system.simulate(n_sims, statquo)
sims_statquo$total #66797

sims_moderate <- QPress::system.simulate(n_sims, moderate)
sims_moderate$total #65625

sims_redev_A <- QPress::system.simulate(n_sims, redev_A)
sims_redev_A$total #66061
sims_redev_B <- QPress::system.simulate(n_sims, redev_B)
sims_redev_B$total #66876
sims_redev_C <- QPress::system.simulate(n_sims, redev_C)
sims_redev_C$total #66834

sims_newdev_A <- QPress::system.simulate(n_sims, newdev_A)
sims_newdev_A$total #65825
sims_newdev_B <- QPress::system.simulate(n_sims, newdev_B)
sims_newdev_B$total #66596
sims_newdev_C <- QPress::system.simulate(n_sims, newdev_C)
sims_newdev_C$total #66905
sims_newdev_D <- QPress::system.simulate(n_sims, newdev_D)
sims_newdev_D$total #33130

##### Save Simulations #####
saveRDS(sims_statquo, file = paste("Sims_", "StatusQuo_", n_sims, "_", Sys.Date(), ".rds", sep = ""))
saveRDS(sims_moderate, file = paste("Sims_", "Moderate_", n_sims, "_", Sys.Date(), ".rds", sep = ""))

saveRDS(sims_redev_A, file = paste("Sims_", "redev_A_", n_sims, "_", Sys.Date(), ".rds", sep = ""))
saveRDS(sims_redev_B, file = paste("Sims_", "redev_B_", n_sims, "_", Sys.Date(), ".rds", sep = ""))
saveRDS(sims_redev_C, file = paste("Sims_", "redev_C_", n_sims, "_", Sys.Date(), ".rds", sep = ""))

saveRDS(sims_newdev_A, file = paste("Sims_", "newdev_A_", n_sims, "_", Sys.Date(), ".rds", sep = ""))
saveRDS(sims_newdev_B, file = paste("Sims_", "newdev_B_", n_sims, "_", Sys.Date(), ".rds", sep = ""))
saveRDS(sims_newdev_C, file = paste("Sims_", "newdev_C_", n_sims, "_", Sys.Date(), ".rds", sep = ""))
saveRDS(sims_newdev_D, file = paste("Sims_", "newdev_D_", n_sims, "_", Sys.Date(), ".rds", sep = ""))


##### Perturbations #####
#using custom exploratory widget & print results from QPressFunctions.R
#Save the pos/neg sims by copying/pasting last output into Excel
impact.barplot(sim = sims_statquo) 
impact.barplot(sim = sims_moderate)

impact.barplot(sim = sims_redev_A) 
impact.barplot(sim = sims_redev_B) 
impact.barplot(sim = sims_redev_C) 
impact.barplot(sim = sims_redev_D) 

impact.barplot(sim = sims_newdev_A)
impact.barplot(sim = sims_newdev_B) 
impact.barplot(sim = sims_newdev_C) 
impact.barplot(sim = sims_newdev_D) 

impact.barplot(sim = sims_newdev_E1) 
impact.barplot(sim = sims_newdev_E2) 
impact.barplot(sim = sims_newdev_E3)
impact.barplot(sim = sims_newdev_E4) 

#Examine weight values in the accepted model runs:
sims_statquo$edges
mean(abs(sims_statquo$w))

sims_moderate$edges
mean(abs(sims_moderate$w))

##################################################################################################
##################################################################################################

# For generating plots that are not part of the QPress package, use code below
# Example code was provided by Ben Raymond, but this came from K. Sobocinski

# Extract the bits we need
edges <- sims_redev_A$edges
write.csv(edges, file = "Model_EdgesList.csv")
As <- sims_redev_A$A
nodes <- node.labels(edges)
write.csv(nodes, file = "Model_NodesList.csv")

monitor <- c(rep(NA,length(nodes))) ## Don't enforce any required responses


#Call specific nodes of interest
#To show only a subset of node responses (e.g. 12, 13, 27, 4, 31), run this instead of standard plot:
myplot <- function(nodes,As,perturb,monitor,epsilon=1.0E-5,main="",cex.axis=1) {
  pal <- c("lightblue", "#808080", "tomato3")  #Kathryn's colors: c("firebrick4", "#808080", "lightblue")
  results <- matrix(0,length(nodes),3)
  for(i in 1:length(As)) {
    impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
    if(all(monitor==impact,na.rm=T)) {
      results <- results + outer(impact,-1:1,'==')
    }
  }
  rownames(results) <- nodes
  nodes <- nodes[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33)] #this is where you specify the nodes of interest
  results <- results[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33),] #this is where you specify the nodes of interest
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
Indiv_Perturb <- paste(currentDate,"_PerturbationPlots_pos",".pdf",sep="")
pdf(file = Indiv_Perturb)
# For function
#opar <- par
#par(mfrow=c(2,2)) #This can be invoked for a 2x2 layout (better for simple (reduced vars) plot)
for (i in 1:33) { #number of nodes in model
  #i=2
  #Set up desired directions of perturbations--based upon direction of press (-1,1)
  #For all presses (should have 1 per node)
  press = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

  #length(press)
  presses=diag(press, nrow=33, ncol=33)
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

#NEGATIVES
Indiv_Perturb <- paste(currentDate,"_PerturbationPlots_neg",".pdf",sep="")
pdf(file = Indiv_Perturb)
# For function
#opar <- par
#par(mfrow=c(2,2)) #This can be invoked for a 2x2 layout (better for simple (reduced vars) plot)
for (i in 1:33) { #number of nodes in model
  #i=2
  #Set up desired directions of perturbations--based upon direction of press (-1,1)
  #For all presses (should have 1 per node)
  press = c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)

  #length(press)
  presses=diag(press, nrow=33, ncol=33)
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



##### Sensitivity Analysis Code adapted from K. Sobocinski ##### 

library(reshape2)
library(ggplot2)

#To extract the weight values in the accepted model runs:
sims$edges
head(sims$w)
tail(sims$w)
mean(abs(sims$w)) #0.5007

is.matrix(sims$w) #True
weight <- as.data.frame(sims$w)
head(weight)
#Check distributions of different nodes to see how variable they are
hist(weight[,63])

#To assess the sensitivity of the weights
#Extract edges and weights from simulations
wts <- reshape2::melt(weight)
colnames(wts)=c("Edge", "Value")
head(wts)

dim(sims$w) #10000 x 145 (145 linkages in the model)

#Get means for each edge
edgemean <- as.data.frame(apply(sims$w, 2, mean))
summary(edgemean)
edgemin <- as.data.frame(apply(sims$w, 2, min))
edgemax <- as.data.frame(apply(sims$w, 2, max))
edgestdev <- as.data.frame(apply(sims$w, 2, sd))
hist(abs(edgemean[,1]))
hist(abs(edgemin[,1]))
hist(abs(edgemax[,1]))
hist(abs(edgestdev[,1]))
lowerSD <- abs(edgemean[,1])-abs(edgestdev[,1])
upperSD <- abs(edgemean[,1])+abs(edgestdev[,1])

edgenames <- as.data.frame(levels(wts$Edge)) 
edge.vals <- cbind(edgenames, abs(edgemean[,1]), abs(edgemin[,1]), 
                 abs(edgemax[,1]))

#USe max and min values with mean
head(edge.vals)
dim(edge.vals) #145 linkaes in the model x 4 columns
colnames(edge.vals)=c("Edge", "Mean", "Min", "Max")
str(edge.vals)

#Use SD
edgesSD <- cbind(edgenames, abs(edgemean[,1]), lowerSD, upperSD)
head(edgesSD)
colnames(edgesSD)=c("Edge", "Mean", "LowerSD", "UpperSD")

#Plot all edges and means, maxes, and mins
ggplot(edge.vals, aes(x=Mean, y=Edge)) +
  geom_errorbarh(data=edge.vals, aes(xmax=Max, xmin=Min), colour = "grey50") + 
  geom_point()

#Reorder so easier to see
ggplot(edgesSD, aes(x=Mean, y=reorder(Edge, Mean))) +
  geom_errorbarh(data=edgesSD, aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
  geom_point() + theme_bw() + xlab("Weight abs(mean +/- sd)") + ylab("Edge") +
  geom_vline(xintercept=0, linetype="dotted") +
  geom_vline(xintercept=1.0, linetype="dotted") +
  geom_vline(xintercept=0.5, linetype="dotted") + theme(axis.text=element_text(size=6),
                                                         axis.title=element_text(size=10,face="bold"))

#Pull out outliers (top/bottom 15 values)
#Reorder EdgesSD by mean
ro <- edgesSD[order(-edgesSD$Mean),]
top <- ro[1:15,]
bottom <- ro[131:145,]

outliers <- rbind(top, bottom)

#Plot all outliers
ggplot(outliers, aes(x=Mean, y=reorder(Edge, Mean))) +
  geom_errorbarh(data=outliers, aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
  ylab("Edge") +
  xlab("Weight (mean +/- sd)") +
  theme_bw() +
  geom_point() +
  geom_vline(xintercept=0, linetype="dotted") +
  geom_vline(xintercept=1.0, linetype="dotted") +
  geom_vline(xintercept=0.5, linetype="dotted") + theme(axis.text=element_text(size=12),
                                                        axis.title=element_text(size=12,face="bold"))
#Plot top (minus self-reg. loops) and bottom:
ggplot(outliers[8:30,], aes(x=Mean, y=reorder(Edge, Mean))) +
  geom_errorbarh(data=outliers[8:30,], aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
  ylab("Edge") +
  xlab("Weight abs(mean +/- sd)") +
  theme_bw() +
  geom_point() +
  geom_vline(xintercept=0.5, linetype="dotted")

#To plot bottom (most sensitive edges only):
ggplot(bottom, aes(x=Mean, y=reorder(Edge, Mean))) +
  geom_errorbarh(data=bottom, aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
  ylab("Edge") +
  xlab("Mean Weight") +
  theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"))+
  geom_point() +
  geom_vline(xintercept=0.5, linetype="dotted")



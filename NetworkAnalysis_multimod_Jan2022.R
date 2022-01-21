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
statquo <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/StatQuo_Jan2022_forR.dia")

redev_E <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/RedevE_Jan2022_forR.dia")
redev_F <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/RedevF_Jan2022_forR.dia") 
redev_Fb <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/RedevFb_Jan2022_forR.dia") 
redev_G <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/RedevG_Jan2022_forR.dia") 
redev_H <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/RedevH_Jan2022_forR.dia") 


## Examine unweighted adjacency matrices
A_Statquo <- adjacency.matrix(statquo)
A_Statquo
write.csv(A, file = "Model_AdjMatrix.csv", row.names = FALSE) #save the matrix, ifn needed

## Enforce limitation (This is redundant with the self-limiting edges in the Dia model, if they are already included)
statquo <- QPress::enforce.limitation(statquo)

redev_E <- QPress::enforce.limitation(redev_E)
redev_F <- QPress::enforce.limitation(redev_F)
redev_Fb <- QPress::enforce.limitation(redev_Fb)
redev_G <- QPress::enforce.limitation(redev_G)
redev_H <- QPress::enforce.limitation(redev_H)


##### Simulations #####
#If model simulations already exist, load them

sims_statquo <- readRDS("Sims_StatusQuo_10000_2022-01-20.rds")

sims_redev_E <- readRDS("Sims_redev_E_10000_2022-01-20.rds")
sims_redev_F <- readRDS("Sims_redev_F_10000_2022-01-20.rds")
sims_redev_G <- readRDS("Sims_redev_G_10000_2022-01-20.rds")
sims_redev_H <- readRDS("Sims_redev_H_10000_2022-01-20.rds")


#If model simulation does not exist, simulate and save!
n_sims <- 10000 #number of accepted simulations requested

sims_statquo <- QPress::system.simulate(n_sims, statquo)
sims_statquo$total #65571

sims_redev_E <- QPress::system.simulate(n_sims, redev_E)
sims_redev_E$total #66296
sims_redev_F <- QPress::system.simulate(n_sims, redev_F)
sims_redev_F$total #65607
sims_redev_G <- QPress::system.simulate(n_sims, redev_G)
sims_redev_G$total #66028
sims_redev_H <- QPress::system.simulate(n_sims, redev_H)
sims_redev_H$total #66566

sims_redev_Fb <- QPress::system.simulate(n_sims, redev_Fb)
sims_redev_Fb$total #66075


##### Save Simulations #####
saveRDS(sims_statquo, file = paste("Sims_", "StatusQuo_", n_sims, "_", Sys.Date(), ".rds", sep = ""))

saveRDS(sims_redev_E, file = paste("Sims_", "redev_E_", n_sims, "_", Sys.Date(), ".rds", sep = ""))
saveRDS(sims_redev_F, file = paste("Sims_", "redev_F_", n_sims, "_", Sys.Date(), ".rds", sep = ""))
saveRDS(sims_redev_G, file = paste("Sims_", "redev_G_", n_sims, "_", Sys.Date(), ".rds", sep = ""))
saveRDS(sims_redev_H, file = paste("Sims_", "redev_H_", n_sims, "_", Sys.Date(), ".rds", sep = ""))

saveRDS(sims_redev_Fb, file = paste("Sims_", "redev_Fb_", n_sims, "_", Sys.Date(), ".rds", sep = ""))

##### Perturbations #####
#using custom exploratory widget & print results from QPressFunctions.R
#Save the pos/neg sims by copying/pasting last output into Excel

# Status Quo Individual Strategies
impact.barplot(sim = sims_statquo) # (+ Redevelopment)

# Urban Strategies only (+ Redevelopment) 
impact.barplot(sim = sims_redev_E)
impact.barplot(sim = sims_redev_F) 
impact.barplot(sim = sims_redev_G)
impact.barplot(sim = sims_redev_H)

impact.barplot(sim = sims_redev_Fb) 

#Examine weight values in the accepted model runs:
sims_statquo$edges
mean(abs(sims_statquo$w)) # 0.5008522

sims_moderate$edges
mean(abs(sims_moderate$w)) # 0.5004666

mean(abs(sims_redev_A$w)) # 0.5007057
mean(abs(sims_redev_B$w)) # 0.5004665
mean(abs(sims_redev_C$w)) # 0.5006133

mean(abs(sims_newdev_A$w)) # 0.5004843
mean(abs(sims_newdev_B$w)) # 0.500708
mean(abs(sims_newdev_C$w)) # 0.5008376
mean(abs(sims_newdev_D$w)) # 0.500463

##################################################################################################
##################################################################################################

# For generating plots that are not part of the QPress package, use code below
# Example code was provided by Ben Raymond, but this came from K. Sobocinski

# Extract the bits we need from the Status Quo Model
edges <- sims_statquo$edges
write.csv(edges, file = "Model_EdgesList.csv")
As <- sims_statquo$A
nodes <- node.labels(edges)
write.csv(nodes, file = "Model_NodesList.csv")

monitor <- c(rep(NA,length(nodes))) ## Don't enforce any required responses


#Call specific nodes of interest
#To show only a subset of node responses (e.g. 12, 13, 27, 4, 31), run this instead of standard plot:
myplot <- function(nodes,As,perturb,monitor,epsilon=1.0E-5,main="",cex.axis=1) {
  pal <- c("grey30", "gray80", "tomato2")
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



##### Sensitivity Analysis ##### 
#Code adapted from K. Sobocinski 

library(reshape2)
library(ggplot2)

#write a function to plot the outlier edge weights for a given simulation
plot_outwts = function (sims) 
{
  print("plotting outlier weights")
  weight <- as.data.frame(sims$w) #extract the weight values in the accepted model runs
  wts <- reshape2::melt(weight)
  colnames(wts)=c("Edge", "Value")
  
  edgemean <- as.data.frame(apply(sims$w, 2, mean)) #calculate weight mean for each edge
  edgestdev <- as.data.frame(apply(sims$w, 2, sd)) #calculate weight stdev for each edge
  lowerSD <- abs(edgemean[,1])-abs(edgestdev[,1])
  upperSD <- abs(edgemean[,1])+abs(edgestdev[,1])
  
  edgenames <- as.data.frame(levels(wts$Edge)) 
  edgesSD <- cbind(edgenames, abs(edgemean[,1]), lowerSD, upperSD)
  colnames(edgesSD)=c("Edge", "Mean", "LowerSD", "UpperSD")
  
  #Pull out outliers (top/bottom 15 values) & reorder EdgesSD by mean
  bottom <- edgesSD %>% dplyr::slice_min(Mean, n = 15)
  top <- edgesSD %>% dplyr::slice_max(Mean, n = 15)
  outliers <- rbind(top, bottom)
  
  #Plot all outliers (top/bottom 15 values)
  ggplot(outliers, aes(x=Mean, y=reorder(Edge, Mean))) +
    geom_errorbarh(data=outliers, aes(xmax=UpperSD, xmin=LowerSD), colour = "grey50") + 
    ylab("Edge") +
    xlab("Weight (mean +/- SD)") +
    theme_bw() +
    geom_point() +
    geom_vline(xintercept=0, linetype="dotted") +
    geom_vline(xintercept=1.0, linetype="dotted") +
    geom_vline(xintercept=0.5, linetype="dotted") + theme(axis.text=element_text(size=12),
                                                          axis.title=element_text(size=12,face="bold"))
}

plot_outwts(sims = sims_statquo)
plot_outwts(sims = sims_moderate)
plot_outwts(sims = sims_newdev_A)
plot_outwts(sims = sims_newdev_B)
plot_outwts(sims = sims_newdev_C)
plot_outwts(sims = sims_newdev_D)
plot_outwts(sims = sims_redev_A)
plot_outwts(sims = sims_redev_B)
plot_outwts(sims = sims_redev_C)


##### Old sensitivity analysis code #####
#To extract the weight values in the accepted model runs:
sims_statquo$edges
head(sims_statquo$w)
tail(sims_statquo$w)

is.matrix(sims_statquo$w) #True
weight_statquo <- as.data.frame(sims_statquo$w)
head(weight_statquo)
#Check distributions of different nodes to see how variable they are
hist(weight_statquo[,10])

#To assess the sensitivity of the weights
#Extract edges and weights from simulations
wts_statquo <- reshape2::melt(weight_statquo)
colnames(wts_statquo)=c("Edge", "Value")
head(wts_statquo)

dim(sims_statquo$w) #10000 x 143 (143 linkages in the model)

#Get means for each edge
edgemean <- as.data.frame(apply(sims_statquo$w, 2, mean))
summary(edgemean)
edgemin <- as.data.frame(apply(sims_statquo$w, 2, min))
edgemax <- as.data.frame(apply(sims_statquo$w, 2, max))
edgestdev <- as.data.frame(apply(sims_statquo$w, 2, sd))
hist(abs(edgemean[,1]))
hist(abs(edgemin[,1]))
hist(abs(edgemax[,1]))
hist(abs(edgestdev[,1]))
lowerSD <- abs(edgemean[,1])-abs(edgestdev[,1])
upperSD <- abs(edgemean[,1])+abs(edgestdev[,1])

edgenames <- as.data.frame(levels(wts_statquo$Edge)) 
edge.vals <- cbind(edgenames, abs(edgemean[,1]), abs(edgemin[,1]), 
                 abs(edgemax[,1]))

#USe max and min values with mean
head(edge.vals)
dim(edge.vals) #143 linkages in the model x 4 columns
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
bottom <- edgesSD %>% dplyr::slice_min(Mean, n = 15)
top <- edgesSD %>% dplyr::slice_max(Mean, n = 15)

outliers <- rbind(top, bottom)

#Plot all outliers (top/bottom 15 values)
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



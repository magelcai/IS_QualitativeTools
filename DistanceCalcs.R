#Calculate Network Metrics (Distance and Density) using iGraph

#install.packages("igraph")
library(igraph)
library(here)
library(QPress)

##### Load the Dia models #####
statquo <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/StatusQuo_Jan2022_forR.dia")
moderate <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/Moderate_Jan2022_forR.dia") 

newdev_A <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/NewDevA_Jan2022_forR.dia") 
newdev_B <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/NewDevB_Jan2022_forR.dia") 
newdev_C <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/NewDevC_Jan2022_forR.dia") 
newdev_D <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/NewDevD_Jan2022_forR.dia") 

redev_E <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/RedevE_Jan2022_forR.dia")
redev_F <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/RedevF_Jan2022_forR.dia") 
redev_G <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/RedevG_Jan2022_forR.dia") 
redev_H <- QPress::model.dia("./DiaModels/MultiMods_Jan2022/RedevH_Jan2022_forR.dia") 
 

##### Adjacency Matrix #####
#Write function to create an igraph-ready adjacency matrix of the dia models
create_adj = function (diamod) 
{
  print("creating adjacency matrix for igraph")
  diamod <- QPress::enforce.limitation(diamod)
  A <- QPress::adjacency.matrix(diamod)
  nodes <- QPress::node.labels(diamod)#grab the node names to assign to columns/rows
  rownames(A) <- nodes[c(1:33)] #assign row names
  colnames(A) <- nodes[c(1:33)] #assign column names
  A_trans <- t(A) #transpose so that igraph reads it correctly
  A_pos <- abs(A_trans) #absolute value because igraph doesn't do negatives (I think)
  igraph <- igraph::graph_from_adjacency_matrix(A_pos, mode = "directed", add.rownames = TRUE)
}

ig_statquo <- create_adj(diamod = statquo)
ig_mod <- create_adj(diamod = moderate)

ig_newdevA <- create_adj(diamod = newdev_A)
ig_newdevB <- create_adj(diamod = newdev_B)
ig_newdevC <- create_adj(diamod = newdev_C)
ig_newdevD <- create_adj(diamod = newdev_D)

ig_redevE <- create_adj(diamod = redev_E)
ig_redevF <- create_adj(diamod = redev_F)
ig_redevG <- create_adj(diamod = redev_G)
ig_redevH <- create_adj(diamod = redev_H)


#Examine the igraph plots of the models
plot.igraph(ig_statquo) #basic plot
tkplot(ig_statquo) #fancy plot that can be manipulated

#Calculate Edge Density: The density of a graph is the ratio of the number of edges and the number of possible edges.
#For Table S3
edge_density(ig_statquo)
edge_density(ig_mod)

edge_density(ig_newdevA)
edge_density(ig_newdevB)
edge_density(ig_newdevC)
edge_density(ig_newdevD)

edge_density(ig_redevE)
edge_density(ig_redevF)
edge_density(ig_redevG)
edge_density(ig_redevH)

#distance_table returns a named list with two entries: res is a numeric vector, the histogram of distances, unconnected is a numeric scalar, 
#the number of pairs for which the first vertex is not reachable from the second. The sum of the two entries is always n(n-1) 
#for directed graphs and n(n-1)/2 for undirected graphs.
dist_tab <- distance_table(ig_statquo)

#mean_distance calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices 
#(both ways for directed graphs). This function does not consider edge weights currently and uses a breadth-first search.
mean_distance(ig_statquo) #2.414201
mean_distance(ig_mod) #2.418146

mean_distance(ig_newdevA) #2.406312
mean_distance(ig_newdevB) #2.402367
mean_distance(ig_newdevC) #2.418146
mean_distance(ig_newdevD) #2.421471

mean_distance(ig_redevE) #2.414201
mean_distance(ig_redevF) #2.414201
mean_distance(ig_redevG) #2.392505
mean_distance(ig_redevH) #2.416016

#distances calculates the lengths of pairwise shortest paths from a set of vertices (from) to another set of vertices (to).
NewDevDist_statquo <- as.data.frame(t(distances(ig_statquo, 16, mode = "out")))
NewDevDist_mod <- as.data.frame(t(distances(ig_mod, 16, mode = "out")))
NewDevDist_newdevA <- as.data.frame(t(distances(ig_newdevA, 16, mode = "out")))
NewDevDist_newdevB <- as.data.frame(t(distances(ig_newdevB, 16, mode = "out")))
NewDevDist_newdevC <- as.data.frame(t(distances(ig_newdevC, 16, mode = "out")))
NewDevDist_newdevD <- as.data.frame(t(distances(ig_newdevD, 16, mode = "out")))

NewDev_list <- list("NewDevDist_statquo", "NewDevDist_mod", "NewDevDist_newdevA", "NewDevDist_newdevB", "NewDevDist_newdevC", "NewDevDist_newdevD")

NewDev_dists <- do.call(rbind.data.frame, Map(data.frame, statquo=NewDevDist_statquo, mod = NewDevDist_mod, NewDevA=NewDevDist_newdevA, 
                                              NewDevB=NewDevDist_newdevB, NewDevC=NewDevDist_newdevC, NewDevD=NewDevDist_newdevD))
nodes <- QPress::node.labels(statquo)
rownames(NewDev_dists) <- nodes[c(1:33)]

ReDevDist_statquo <- as.data.frame(distances(ig_statquo, 21, mode = "out"))
ReDevDist_mod <- as.data.frame(distances(ig_mod, 21, mode = "out"))
ReDevDist_redevE <- as.data.frame(distances(ig_redevE, 21, mode = "out"))
ReDevDist_redevF <- as.data.frame(distances(ig_redevF, 21, mode = "out"))
ReDevDist_redevG <- as.data.frame(distances(ig_redevG, 21, mode = "out"))
ReDevDist_redevH <- as.data.frame(distances(ig_redevH, 21, mode = "out"))


ReDev_list <- list("ReDevDist_statquo", "ReDevDist_mod","ReDevDist_redevE", "ReDevDist_redevF", "ReDevDist_redevG", "ReDevDist_redevH")

ReDev_dists <- do.call(rbind.data.frame, Map(data.frame, ReD_statquo=ReDevDist_statquo, ReD_mod = ReDevDist_mod, ReD_E=ReDevDist_redevE, ReD_F=ReDevDist_redevF, 
                                             ReD_G=ReDevDist_redevG, ReD_H=ReDevDist_redevH))
nodes <- QPress::node.labels(statquo)
rownames(ReDev_dists) <- nodes[c(1:33)]

Dev_dists <- cbind(NewDev_dists, ReDev_dists)

write.csv(Dev_dists, "DistanceFromDevelopment.csv")

#Calculate Network Metrics (Distance and Density) using iGraph

install.packages("igraph")
library(igraph)
library(here)
library(QPress)

# Load the Dia models
statquo <- QPress::model.dia("./DiaModels/MultiMods/InterJurWatershed_11Aug2021_StatusQuo_forR.dia") # Load the Dia model - Status Quo
redev_A <- QPress::model.dia("./DiaModels/MultiMods/InterJurWatershed_11Aug2021_RedevA_forR.dia") # Load the Dia model - Redev A
redev_B <- QPress::model.dia("./DiaModels/MultiMods/InterJurWatershed_11Aug2021_RedevB_forR.dia") # Load the Dia model - Redev B
redev_C <- QPress::model.dia("./DiaModels/MultiMods/InterJurWatershed_11Aug2021_RedevC_forR.dia") # Load the Dia model - Redev C
redev_D <- QPress::model.dia("./DiaModels/MultiMods/InterJurWatershed_11Aug2021_RedevD_forR.dia") # Load the Dia model - Redev D

newdev_A <- QPress::model.dia("./DiaModels/MultiMods/InterJurWatershed_11Aug2021_NewDevE4_forR.dia") #this became A
newdev_B <- QPress::model.dia("./DiaModels/MultiMods/InterJurWatershed_11Aug2021_NewDevA_forR.dia") #this became B
newdev_C <- QPress::model.dia("./DiaModels/MultiMods/InterJurWatershed_11Aug2021_NewDevB_forR.dia") #C
newdev_D <- QPress::model.dia("./DiaModels/MultiMods/InterJurWatershed_11Aug2021_NewDevC_forR.dia") #D
newdev_E <- QPress::model.dia("./DiaModels/MultiMods/InterJurWatershed_11Aug2021_NewDevD_forR.dia") #E
 

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
ig_redevA <- create_adj(diamod = redev_A)
ig_redevB <- create_adj(diamod = redev_B)
ig_redevC <- create_adj(diamod = redev_C)
ig_redevD <- create_adj(diamod = redev_D)

ig_newdevA <- create_adj(diamod = newdev_A)
ig_newdevB <- create_adj(diamod = newdev_B)
ig_newdevC <- create_adj(diamod = newdev_C)
ig_newdevD <- create_adj(diamod = newdev_D)
ig_newdevE <- create_adj(diamod = newdev_E)


#Examine the igraph plots of the models
plot.igraph(ig_statquo) #basic plot
tkplot(ig_statquo) #fancy plot that can be manipulated

#Calculate Edge Density: The density of a graph is the ratio of the number of edges and the number of possible edges.
edge_density(ig_statquo)
edge_density(ig_redevA)
edge_density(ig_redevB)
edge_density(ig_redevC)
edge_density(ig_redevD)

edge_density(ig_newdevA)
edge_density(ig_newdevB)
edge_density(ig_newdevC)
edge_density(ig_newdevD)
edge_density(ig_newdevE)

#distance_table returns a named list with two entries: res is a numeric vector, the histogram of distances, unconnected is a numeric scalar, 
#the number of pairs for which the first vertex is not reachable from the second. The sum of the two entries is always n(n-1) 
#for directed graphs and n(n-1)/2 for undirected graphs.
dist_tab <- distance_table(ig_statquo)

#mean_distance calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices 
#(both ways for directed graphs). This function does not consider edge weights currently and uses a breadth-first search.
mean_distance(ig_statquo) #2.41


#distances calculates the lengths of pairwise shortest paths from a set of vertices (from) to another set of vertices (to).
NewDevDist_statquo <- as.data.frame(t(distances(ig_statquo, 16, mode = "out")))
NewDevDist_newdevA <- as.data.frame(t(distances(ig_newdevA, 16, mode = "out")))
NewDevDist_newdevB <- as.data.frame(t(distances(ig_newdevB, 16, mode = "out")))
NewDevDist_newdevC <- as.data.frame(t(distances(ig_newdevC, 16, mode = "out")))
NewDevDist_newdevD <- as.data.frame(t(distances(ig_newdevD, 16, mode = "out")))
NewDevDist_newdevE <- as.data.frame(t(distances(ig_newdevE, 16, mode = "out")))

NewDev_list <- list("NewDevDist_statquo", "NewDevDist_newdevA", "NewDevDist_newdevB", "NewDevDist_newdevC", "NewDevDist_newdevD", "NewDevDist_newdevE")

NewDev_dists <- do.call(rbind.data.frame, Map(data.frame, statquo=NewDevDist_statquo, NewDevA=NewDevDist_newdevA, 
                                              NewDevB=NewDevDist_newdevB, NewDevC=NewDevDist_newdevC, NewDevD=NewDevDist_newdevD, NewDevE=NewDevDist_newdevE))
nodes <- QPress::node.labels(statquo)
rownames(NewDev_dists) <- nodes[c(1:33)]

ReDevDist_statquo <- as.data.frame(distances(ig_statquo, 21, mode = "out"))
ReDevDist_newdevA <- as.data.frame(distances(ig_redevA, 21, mode = "out"))
ReDevDist_newdevB <- as.data.frame(distances(ig_redevB, 21, mode = "out"))
ReDevDist_newdevC <- as.data.frame(distances(ig_redevC, 21, mode = "out"))
ReDevDist_newdevD <- as.data.frame(distances(ig_redevD, 21, mode = "out"))

ReDev_list <- list("ReDevDist_statquo", "ReDevDist_newdevA", "ReDevDist_newdevB", "ReDevDist_newdevC", "ReDevDist_newdevD")

ReDev_dists <- do.call(rbind.data.frame, Map(data.frame, ReDevstatquo=ReDevDist_statquo, ReDevA=ReDevDist_newdevA, ReDevB=ReDevDist_newdevB, 
                                             ReDevC=ReDevDist_newdevC, ReDevD=ReDevDist_newdevD))
nodes <- QPress::node.labels(statquo)
rownames(ReDev_dists) <- nodes[c(1:33)]

Dev_dists <- cbind(NewDev_dists, ReDev_dists)

write.csv(Dev_dists, "DistanceFromDevelopment.csv")

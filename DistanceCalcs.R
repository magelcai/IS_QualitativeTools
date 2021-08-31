#Calculate Network Metrics (including Distance) from iGraph

install.packages("igraph")
library(igraph)
library(here)
library(QPress)

## row/column names
adjm <- matrix(sample(0:1, 100, replace=TRUE, prob=c(0.9,0.1)), nc=10)
rownames(adjm) <- sample(letters, nrow(adjm))
colnames(adjm) <- seq(ncol(adjm))
g10 <- graph_from_adjacency_matrix(adjm, weighted=TRUE, add.rownames="code")
summary(g10)
plot.igraph(g10)
Dist <- distances(g10, 9, mode = "in")

# Load the Dia models
statquo <- QPress::model.dia("./DiaModels/MultiMods/InterJurWatershed_11Aug2021_StatusQuo_forR.dia")
A_Statquo <- adjacency.matrix(statquo)
Nodes_Statquo <- node.labels(statquo) #grab the node names to assign to columns/rows
rownames(A_Statquo) <- Nodes_Statquo[c(1:33)] #assign row names
colnames(A_Statquo) <- Nodes_Statquo[c(1:33)] #assign column names
A_Statquo <- t(A_Statquo) #transpose so that igraph reads it correctly
A_Statquo_pos <- abs(A_Statquo) #absolute value because igraph doesn't do neg?

igraph_Statquo <- graph_from_adjacency_matrix(A_Statquo_pos, mode = "directed", add.rownames = TRUE)
plot.igraph(igraph_Statquo) #basic plot
tkplot(igraph_Statquo) #fancy plot that can be manipulated


#distance_table returns a named list with two entries: res is a numeric vector, the histogram of distances, unconnected is a numeric scalar, 
#the number of pairs for which the first vertex is not reachable from the second. The sum of the two entries is always n(n-1) 
#for directed graphs and n(n-1)/2 for undirected graphs.
dist_tab <- distance_table(igraph_Statquo)

#mean_distance calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices 
#(both ways for directed graphs). This function does not consider edge weights currently and uses a breadth-first search.
mean_distance(igraph_Statquo) #2.41


#distances calculates the lengths of pairwise shortest paths from a set of vertices (from) to another set of vertices (to).
NewDevDist <- as.data.frame(distances(igraph_Statquo, 16, mode = "out"))
ReDevDist <- as.data.frame(distances(igraph_Statquo, 21))


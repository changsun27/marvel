library(networkD3)
library(igraph)
library(plyr)
library(dplyr)
library(modMax)
library(fcd)


df <- read.csv("link-10.csv")
colnames(df) <- c('source', 'target', 'value')
df$weight = 1/df$cnt

hero_x = as.character(df$source)
hero_y = as.character(df$target)
nodes = unique(c(hero_x, hero_y))

df = df[, names(df) %in% c('hero_x', 'hero_y', 'weight')]
write.csv(df, 'link-10-reverse-weight.csv')
# network
g <- graph_from_data_frame(df, directed=FALSE, vertices = nodes)

d = igraph::degree(g, mode = "all")   # total degree

hist(d, col='light blue', main='Total Degree Distribution', 
     ylab='Frequency', xlab='Degree', breaks=40)

b = betweenness(g, v = V(g), directed = FALSE)
hist(b, col='light green', main='Betweenness Distribution', 
     ylab='Frequency', xlab='Betweenness', breaks=100)
max(b)

################### Louvain ################### 
#df =  read.csv("link-10.csv")
#df =  read.csv("link-50.csv")
df =  read.csv("link-100.csv")
names(df) = c('hero_x', 'hero_y', 'weight')


# create a graph
g = igraph::simplify(igraph::graph.data.frame(df, directed=FALSE))

# create nodelist data frame
node.df = data.frame(id = c(0:(igraph::vcount(g)-1)), name = igraph::V(g)$name)

# map node names from the edge list to node IDs
getNodeID = function(x){
  which(x==igraph::V(g)$name) -1 
}

# get edge dataframe
edge.df = ddply(df, .variables = c('hero_x', 'hero_y', 'weight'), 
                function(x) data.frame(xID = getNodeID(x$hero_x), yID = getNodeID(x$hero_y)))

# calculate degree
node.df = cbind(node.df, node_degree = igraph::degree(g, v = igraph::V(g), mode='all'))

# calculate betweenness
beAll = igraph::betweenness(g, v=igraph::V(g), directed = F, normalized = T)
node.df = cbind(node.df, node_between_graph = 500*beAll)
node.df = cbind(node.df, node_between = beAll)

# calculate Dice similarities
dsAll = igraph::similarity.dice(g, vids = igraph::V(g), mode = 'all')

# create  data frame that contains the Dice similarity between any two vertices
F1 <- function(x) {data.frame(diceSim = dsAll[x$xID +1, x$yID + 1])}
# place a new column in edgeList with the Dice Sim
edge.df <- plyr::ddply(edge.df, .variables=c("hero_x", "hero_y", "weight", "xID", "yID"), 
                       function(x) data.frame(F1(x)))

# create a set of colors for each edge, based on their dice similarity values
F2 <- colorRampPalette(c("gray48", "gray74"), bias = nrow(edge.df), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(edge.df$diceSim)))
edges_col <- sapply(edge.df$diceSim, function(x) colCodes[which(sort(unique(edge.df$diceSim)) == x)])

# change cnt value
edge.df$weight_cnt = edge.df$weight/100

# louvain clustering to identify hero groups
clustering = igraph::cluster_louvain(g)
com = igraph::communities(clustering)

# assign group to hero names
for (row_index in 1: length(node.df$name)){
  for (n in 1:length(com)) {
    if (node.df$name[row_index] %in% com[[n]]) {
      node.df$group[row_index] = n
    }
  }
}

# output hero and group name to a csv
var = c('name', 'group', 'node_between', 'node_degree')
group.df = node.df[var]
rownames(group.df) <- 1:nrow(group.df)
#write.csv(group.df, 'louvain-10.csv')
#write.csv(group.df, 'louvain-50.csv')
write.csv(group.df, 'louvain-100.csv')


# plot
network_d3 = networkD3::forceNetwork(Links = edge.df,
                                     Nodes = node.df,
                                     Source = 'xID',
                                     Target = 'yID',
                                     Value = 'weight_cnt',
                                     NodeID = 'name',
                                     Nodesize = 'node_between_graph',
                                     fontSize = 15, 
                                     Group = 'group',
                                     linkDistance = networkD3::JS("function(d) { return 40*d.value; }"), 
                                     linkWidth = networkD3::JS("function(d) { return d.value/5; }"),
                                     zoom = TRUE,
                                     opacity = 1,
                                     opacityNoHover = 0.3,
                                     linkColour=edges_col) 

# Plot network
#network_d3 

#saveNetwork(network_d3, "louvain-10.html", selfcontained = TRUE)
#saveNetwork(network_d3, "louvain-50.html", selfcontained = TRUE)
saveNetwork(network_d3, "louvain-100.html", selfcontained = TRUE)

################### spectral ################### 
#df =  read.csv("link-10.csv")
#df =  read.csv("link-50.csv")
df =  read.csv("link-100.csv")
names(df) = c('hero_x', 'hero_y', 'weight')

# create a graph
g = igraph::simplify(igraph::graph.data.frame(df, directed=FALSE))

# adjacency matrix
adj = get.adjacency(g, sparse = FALSE, attr='weight')

# create nodelist data frame
node.df = data.frame(id = c(0:(igraph::vcount(g)-1)), name = igraph::V(g)$name)

# map node names from the edge list to node IDs
getNodeID = function(x){
  which(x==igraph::V(g)$name) -1 
}

# get edge dataframe
edge.df = ddply(df, .variables = c('hero_x', 'hero_y', 'weight'), 
                function(x) data.frame(xID = getNodeID(x$hero_x), yID = getNodeID(x$hero_y)))

# calculate degree
node.df = cbind(node.df, node_degree = igraph::degree(g, v = igraph::V(g), mode='all'))

# calculate betweenness
beAll = igraph::betweenness(g, v=igraph::V(g), directed = F, normalized = T)
node.df = cbind(node.df, node_between_graph = 500*beAll)
node.df = cbind(node.df, node_between = beAll)

# calculate Dice similarities
dsAll = igraph::similarity.dice(g, vids = igraph::V(g), mode = 'all')

# create  data frame that contains the Dice similarity between any two vertices
F1 <- function(x) {data.frame(diceSim = dsAll[x$xID +1, x$yID + 1])}
# place a new column in edgeList with the Dice Sim
edge.df <- plyr::ddply(edge.df, .variables=c("hero_x", "hero_y", "weight", "xID", "yID"), function(x) data.frame(F1(x)))

# create a set of colors for each edge, based on their dice similarity values
F2 <- colorRampPalette(c("gray48", "gray74"), bias = nrow(edge.df), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(edge.df$diceSim)))
edges_col <- sapply(edge.df$diceSim, function(x) colCodes[which(sort(unique(edge.df$diceSim)) == x)])

# change cnt value
edge.df$weight_col = edge.df$weight/100

# spectrial clustering to identify hero groups
clustering = spectralOptimization(adj, initial='general')
### 30 ### 
#number of communities 56
#modularity 0.1
### 50 ### 
#number of communities 37
#modularity 0.23
group = clustering$`community structure`

#group = spectral.clustering(adj, normalised = TRUE, score = FALSE, K = 23, adj = FALSE)
## k=10 : 51 groups
# k=50 23 groups
# k=100 14 groups
name = colnames(adj)
#df_group = read.csv('spectral-50.csv')
#df_group = df_group[, names(df_group) %in% c('name', 'group')]
df_group = data.frame(name, group)

node.df = merge(node.df, df_group, by='name')
# output hero and group name to a csv
var = c('name', 'node_between', 'node_degree', 'group')
#df_bd = node.df[var]
#rownames(df_bd) <- 1:nrow(df_bd)
#group.df = merge(df_bd, df_group, by='name')
group.df = node.df[var]

#write.csv(group.df, 'spectral-10.csv')
#write.csv(group.df, 'spectral-50.csv')
#write.csv(group.df, 'spectral-100.csv')
write.csv(group.df, 'spectral-100-optimal.csv')

# plot
network_d3 = networkD3::forceNetwork(Links = edge.df,
                                     Nodes = node.df,
                                     Source = 'xID',
                                     Target = 'yID',
                                     Value = 'weight_col',
                                     NodeID = 'name',
                                     Nodesize = 'node_between_graph',
                                     fontSize = 15, 
                                     Group = 'group',
                                     linkDistance = networkD3::JS("function(d) { return 40*d.value; }"), 
                                     linkWidth = networkD3::JS("function(d) { return d.value/5; }"),
                                     zoom = TRUE,
                                     opacity = 1,
                                     opacityNoHover = 0.3,
                                     linkColour=edges_col) 

# Plot network
#network_d3 

#saveNetwork(network_d3, "spectral-10.html", selfcontained = TRUE)
#saveNetwork(network_d3, "spectral-50.html", selfcontained = TRUE)
#saveNetwork(network_d3, "spectral-100.html", selfcontained = TRUE)
saveNetwork(network_d3, "spectral-100-optimal.html", selfcontained = TRUE)


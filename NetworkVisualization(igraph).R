######################### reference ##################################
# Network Analysis.pdf
rm(list = ls())
graphics.off()
cat("\014")
library(igraph)
######################### read data  #################################

setwd("~/DataSciences/DS6401_VisualizationOf ComplexData/HW6")
# as.is default behaviour is to convert character variables to factors
links <- read.csv('Dataset1-Media-Example-EDGES.csv', as.is = TRUE)
nodes <- read.csv('Dataset1-Media-Example-NODES.csv', as.is = TRUE)
head(links);head(nodes)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)
links2 <- as.matrix(links2)

############################ Create Net ################################
# D:
# describes the edges of the network. 
# Its first two columns are the IDs of the source and the target node for each edge. 
# The following columns are edge attributes (weight, type, label, or anything else)
# VERTICES:
# starts with a column of node IDs. 
# Any following columns are interpreted as node attributes.
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)
net
E(net) # edges of the net
V(net) # vertices of the net
E(net)$type; V(net)$media # attribute
# net2 <- graph_from_incidence_matrix(links2)
# net2.bp <- bipartite.projection(net2)

###################### developing igraph #####################
plot(net)
##### removing loops in the graph #####
net <- simplify(net, remove.multiple = F, remove.loops = T)
# Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, edge.curved=.1)

# Set edge color to gray, and the node color to orange.
### Replace the vertex label with the node names stored in "media" ###
plot(net, edge.arrow.size=.2, edge.curved=.1,
     vertex.color="orange", vertex.frame.color="gray50", 
     vertex.label=V(net)$media, vertex.label.color="black", vertex.label.cex=.7)


##### Generate colors based on media type: #####
colrs <- c("gray50", "tomato", "gold") 
V(net)$color <- colrs[V(net)$media.type]
##### Set node size based on audience size: #####
V(net)$size <- V(net)$audience.size*0.7
##### Set edge width based on weight: #####
E(net)$width <- 1+E(net)$weight/12
{
##### some attribute you can change #####
# The labels are currently node IDs.
# Setting label to NA will render no labels: 
V(net)$label.color <- "black"
V(net)$label <- NA
#change arrow size and edge color:
E(net)$arrow.size <- .2 
E(net)$edge.color <- "gray80"
}

plot(net, edge.arrow.size=.2, edge.curved=.1,
     vertex.frame.color="gray50", vertex.label=V(net)$media, 
     vertex.label.color="black", vertex.label.cex=.7)
##### add legend #####
legend(x=-1.5, y=-1.1, c("Newspaper","TV","Online News"), pch=21,col="#777777",
       pt.bg=colrs,pt.cex=2, cex=.8,bty="n",ncol=1)

{
  # Sometimes, especially with semantic networks, 
  # we may be interested in plotting only the labels of the nodes
  plot(net, vertex.shape="none", vertex.label=V(net)$media, 
       vertex.label.font=2, vertex.label.color="gray40", 
       vertex.label.cex=.7, edge.color="gray85")
}

##### color the edge based on their source node color #####
# get the starting node for each edge with the ends()
edge.start <- ends(net, es=E(net), names=F)[,1] 
edge.col <- V(net)$color[edge.start]

plot(net, edge.arrow.size=.2, edge.curved=.1, edge.color = edge.col,
     vertex.frame.color="gray50", vertex.label=V(net)$media, 
     vertex.label.color="black", vertex.label.cex=.7)
legend(x=-1.5, y=-1.1, c("Newspaper","TV","Online News"), pch=21,col="#777777",
       pt.bg=colrs,pt.cex=2, cex=.8,bty="n",ncol=1)


######################### Heatmap of newtwork Matrix #################################
netmat <- get.adjacency(net, attr="weight", sparse=F)
colnames(netmat) <- V(net)$media
rownames(netmat) <- V(net)$media
palf <- colorRampPalette(c("gold","dark orange"))
heatmap(netmat[,17:1], Rowv = NA, Colv = NA, col = palf(100), scale="none", margins=c(10,10))


# user 
net2 <- graph.incidence(links2)
plot(net2, vertex.shape="none", vertex.label=nodes2$media, vertex.label.color=V(net2)$color, 
     vertex.label.font=2, vertex.label.cex=.6, edge.color="gray70", edge.width=2)















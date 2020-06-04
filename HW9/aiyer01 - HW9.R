#
# Author: Advait Ramesh Iyer
# lab 08: Social Network Analysis and Visualization
library(igraph)
library(rgl)

fname <- file.choose()
class.raw <- read.csv(fname, stringsAsFactors = FALSE, header = TRUE)
colnames(class.raw)
class.raw$X

colnames(class.raw) <- gsub("\\.","",colnames(class.raw))
class.raw$X <- gsub(" ","",class.raw$X)

class.raw$X == colnames(class.raw)[-1]

M <- as.matrix(class.raw[ ,-1])
M[,1]
rownames(M) <- colnames(M) <- class.raw$X
M[is.na(M)]

g <- graph_from_adjacency_matrix(M)
# node = person = matrix
# edge = link = connection = line........ on and on

plot.igraph(g)

g <- simplify(g)
plot.igraph(g)

par(mar = c(1,1,1,1))
plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0)

# Who has the most friends?
tmp <- degree(g)
tmp <- sort(tmp, decreasing = TRUE)
par(mar=c(5,15,4,2), cex.axis=0.5)
barplot(tmp, horiz = TRUE, las = 2, border = 'white', col = "darkblue"
        , main = "Most Friends")

V(g)$deg <- degree(g)
plot(V(g)$deg)
V(g)$deg.in <- degree(g,mode = "in")

barplot(V(g)$deg.in, horiz = TRUE, names.arg = V(g)$name, las=2, border = "white"
        , col = "darkorange", main = "Most Popular")

tmp <- degree(g, mode = "out")
barplot(sort(tmp), horiz = TRUE, names.arg = V(g)$name, las=2, border = "white"
        , col = "darkgreen", main = "Most Friendly")

V(g)$deg.out <- degree(g, mode="out")

V(g)$color <- rgb(100,149,237, alpha = 180, maxColorValue = 255)
V(g)$day <- sample(c("T","W"), size = vcount(g), replace = TRUE)
V(g)$color[V(g)$day == "W"] <- rgb(165,42,42,alpha = 180, maxColorValue = 255)

par(mar=c(1,1,1,1))
plot.igraph(g,edge.arrow.size=0,edge.arrow.width=0)

V(g)$size <- V(g)$deg.in
plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0)

par(mar=c(5,4,4,2))
par(mfrow=c(2,2))
plot(sort(V(g)$deg.in), type="l", lwd=2)
plot(sort(sqrt(V(g)$deg.in)), type="l", lwd=2)
plot(sort(log10(V(g)$deg.in)), type="l", lwd=2)
plot(sort(V(g)$deg.in^2), type = "l", lwd=2)

plot(sort(V(g)$deg.in^(1/2)), type = "l", lwd=2)
plot(sort(V(g)$deg.in^(1/3)), type = "l", lwd=2)
plot(sort(V(g)$deg.in^(1/5)), type = "l", lwd=2)
plot(sort(V(g)$deg.in^(1/10)), type = "l", lwd=2)

par(mfrow=c(1,1))
V(g)$size <- 1 + (2*sqrt(V(g)$deg.in))
plot.igraph(g, edge.arrow.size=0,edge.arrow.width=0)

V(g)$label.cex <- 0.75 + (V(g)$deg.out/max(V(g)$deg.out))
plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0)

V(g)$name
g <- delete_vertices(g, "JohnRattazzi")
plot.igraph(g, edge.arrow.size=0,edge.arrow.width=0)

E(g)$color <- "gold"
E(g)[to("AdvaitRameshIyer")]$color <- "red"
E(g)[from("AdvaitRameshIyer")]$color <- "blue"
plot.igraph(g,edge.arrow.size=0, edge.arrow.width=0)

l <- layout.circle(g)
l <- layout_with_kk(g)
l <- layout_on_grid(g)
l <- layout_as_star(g)
l <- layout_as_tree(g, root="AdvaitRameshIyer")
V(g)$x <- l[,1]
V(g)$y <- l[,2]
V(g)$z <- V(g)$deg
rglplot(g)

plot.igraph(g, edge.arrow.size=0,edge.arrow.width=0)

coords <- layout_with_kk(g, dim=3)
rglplot(g, layout=coords)

fname.2 <- "\\\\hd.ad.syr.edu\\03\\75b07e\\Documents\\Desktop\\IST 719\\Class 8\\3dnetwork.png"
rgl.snapshot(filename = fname.2)

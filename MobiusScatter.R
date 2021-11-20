source("OptimalGenerator.R")

R = 5
u = runif(200, min=0, max= 2*pi)
v = runif(200, min=-1, max=1)

x <- (R + v/2 * cos(u /2)) * cos(u)
y <- (R + v/2 * cos(u /2)) * sin(u)
z <- v * sin(u / 2)  

data = matrix(c(x, y, z), ncol=3)


maxdimension = 1
maxscale = 20
dist = "euclidean"
library = "Dionysus"

FltRips = ripsFiltration(X = data, maxdimension = maxdimension, maxscale = maxscale,
                         dist = dist, library = library)

DiagFltRips = filtrationDiag(filtration = FltRips, maxdimension = maxdimension,
                             library = library, location = TRUE)

plot(DiagFltRips[["diagram"]])

path = DiagFltRips[["cycleLocation"]][[228]]
open3d()

points3d(data, color="black", size=6)
# points3d(data[path[,2],])
# rgl.linestrips(data[path[,1],], data[path[,2],], color="green")

for (i in seq(length(path[,1]))){
  x = c(data[path[i,1],][1], data[path[i,2],][1])
  y = c(data[path[i,1],][2], data[path[i,2],][2])
  z = c(data[path[i,1],][3], data[path[i,2],][3])
  lines3d(x, y, z, color="red", size=10, lwd=5)
}


# Compute optimal generators

# Find complex and filtration for t=2
time = 2
bools = FltRips[["values"]] <= 2
cmplx = FltRips[["cmplx"]][bools]
values = FltRips[["values"]][bools]
inf = max(values)+0.001
# Compute discrete distance matrix for 0-cells at t=2
eucDist = as.matrix(dist(data))
eucDist[eucDist > time] = 0
# distance = floyd(eucDist)
graph = graph_from_adjacency_matrix(eucDist, mode="undirected", weighted=TRUE)
distance = shortest.paths(graph)
# optimal generators
gens = .optimalGenerators(cmplx, values, distance, inf)
gen = gens[[1]]
for (i in seq(length(gen[,1]))){
  from = gen[i,1]
  to = gen[i,2]
  path = shortest_paths(graph, from, to)
  path = unlist(path, use.names = FALSE)
  froms = head(path, length(path) - 1)
  tos = tail(path, length(path) - 1)
  for (j in seq(length(froms))){
    x = c(data[froms[j],1], data[tos[j],1])
    y = c(data[froms[j],2], data[tos[j],2])
    z = c(data[froms[j],3], data[tos[j],3])
    lines3d(x,y,z, color="green", size=10, lwd=5)
  }
}

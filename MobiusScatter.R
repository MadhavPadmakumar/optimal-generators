R = 5
u = runif(400, min=0, max= 2*pi)
v = runif(400, min=-1, max=1)

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

path = DiagFltRips[["cycleLocation"]][[458]]
open3d()

points3d(data, color="black", size=6)
# points3d(data[path[,2],])
# rgl.linestrips(data[path[,1],], data[path[,2],], color="green")

for (i in seq(length(path[,1]))){
  x = c(data[path[i,1],][1], data[path[i,2],][1])
  y = c(data[path[i,1],][2], data[path[i,2],][2])
  z = c(data[path[i,1],][3], data[path[i,2],][3])
  lines3d(x, y, z, color="green", size=10, lwd=5)
}
close3d()
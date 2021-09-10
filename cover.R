library(rlist)
library(TDA)
complex = list(1,2,3,4,5,6,7,8, c(1,2), c(1,3), c(2,3), c(3,4), c(4,5), c(5,6),
               c(5,7),c(6,8), c(7,8))
values = c(0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7)
inf = 10

#Requirements:
#inf > max(values)
cover = function(complex, inf){
  vertices = list.filter(complex, length(.) == 1)
  edgesVec = combn(vertices, 2)
  edges = list()
  i = 1
  while(i <= length(edgesVec)/2){
    edges[[i]] = c(edgesVec[[1,i]], edgesVec[[2,i]])
    i = i + 1
  }
  
  facesVec = combn(vertices, 3)
  faces = list()
  i = 1
  while(i < length(facesVec)/3){
    faces[[i]] = c(facesVec[[1,i]], facesVec[[2,i]], facesVec[[3,i]])
    i = i + 1
  }
  
  simplexCover = c(edges, faces)
  coverValues = rep(inf, times = length(simplexCover))
  return (list(simplexCover, coverValues))
}

cover = cover(complex, inf)
complex = c(complex, cover[[1]])
values = c(values, cover[[2]])
filtration = list(complex, values, TRUE)
names(filtration) = c("cmplx", "values", "increasing")
diagram = filtrationDiag(filtration = filtration, maxdimension = 1, 
                         library = "Dionysus", location = TRUE)
plot(diagram[["diagram"]])
diagram

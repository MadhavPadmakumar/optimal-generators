library(rlist)
library(TDA)
# complex = list(1,2,3,4,5,6,7,8, c(1,2), c(1,3), c(2,3), c(3,4), c(4,5), c(5,6),
#                c(5,7),c(6,8), c(7,8))
# values = c(0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7)
# distance = matrix(c(
#   0, 1, 1, 2, 3, 4, 4, 5,
#   1, 0, 1, 2, 3, 4, 4, 5,
#   1, 1, 0, 1, 2, 3, 3, 4,
#   2, 2, 1, 0, 1, 2, 2, 3,
#   3, 3, 2, 1, 0, 1, 1, 2,
#   4, 4, 3, 2, 1, 0, 2, 1,
#   4, 4, 3, 2, 1, 2, 0, 1,
#   5, 5, 4, 3, 2, 1, 1, 0
# ),
# nrow = 8)
# inf = 10

# Returns a list that, when concatenated, will connect all vertices at the inf
# filtration step.
#Requirements:
#inf > max(values)
.cover = function(complex, inf){
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
  while(i <= length(facesVec)/3){
    faces[[i]] = c(facesVec[[1,i]], facesVec[[2,i]], facesVec[[3,i]])
    i = i + 1
  }
  
  simplexCover = c(edges, faces)
  coverValues = rep(inf, times = length(simplexCover))
  return (list(simplexCover, coverValues))
}

# Returns the radius of the smallest geodesic ball containing a cycle, centered at a vertex v.
.geodesicBall = function(v, complex, distMatrix, cover){
  # For each complex, assigns a filtration value equal to max_{x in complex} d(v,x)
  values = double(length(complex))
  for (i in seq_along(complex)){
    values[i] = max(distMatrix[v, unlist(complex[i])])
  }
  # Seals any found cycles
  complex = c(complex, cover[[1]])
  values = c(values, cover[[2]])
  
  # Adds an extra cover at the last step of filtration
  inf = max(values) + 1
  cover = .cover(complex, inf)
  complex = c(complex, cover[[1]])
  values = c(values, cover[[2]])
  
  # Computes persistent homology using this filtration
  filtration = list(complex, values, TRUE)
  names(filtration) = c("cmplx", "values", "increasing")
  diagram = filtrationDiag(filtration= filtration, maxdimension = 1,
                           library= "Dionysus", location= TRUE)
  
  # Find the youngest cycle that dies at inf
  # i = match(1, diagram[["diagram"]][,"dimension"])
  for(i in seq_along(diagram[["diagram"]][,1])){
    
    # Debugging
    # print("geodesicBall:")
    # print(i)
    
    if (diagram[[1]][i,1] == 1 && diagram[[1]][i,3] == inf){
      index = i
      break
    }
  }
  
  # Debugging
  # print(seq_along(diagram[["diagram"]][,1]))
  # print(diagram[["diagram"]])
  # print(complex)
  # print(values)
  
  radius = diagram[["diagram"]][index,2]
  cycle = diagram[["cycleLocation"]][index]
  
  # Debugging
  # print(diagram)
  #print(c(cycle, radius))
  
  return(c(cycle, radius))
}

# Returns the smallest nontrivial homology class, along with its size
.measureSmallest = function(complex, distMatrix, minSize=1, cover){
  vertices = unlist(list.filter(complex, length(.) == 1))
  currentMin = Inf
  for (v in vertices) {
    
    # Debugging
    # print("measureSmallest:")
    # print(v)
    
    
    ball = .geodesicBall(v, complex, distMatrix, cover)
    size = ball[[2]]
    if(size < currentMin){
      currentMin = size
      currentCycle = ball[1]
    }
    if(currentMin == minSize){
      break
    }
  }
  return(c(currentCycle, currentMin))
}


### Testing .cover
# cover = .cover(complex, inf)
# complex = c(complex, cover[[1]])
# values = c(values, cover[[2]])
# filtration = list(complex, values, TRUE)
# names(filtration) = c("cmplx", "values", "increasing")
# diagram = filtrationDiag(filtration = filtration, maxdimension = 1,
#                          library = "Dionysus", location = TRUE)
# plot(diagram[["diagram"]])
# diagram


### Testing .geodesicBall
# .geodesicBall(3, complex, distance)

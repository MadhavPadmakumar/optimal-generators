library(rlist)
library(TDA)

source('helperFunctions.R')

complex = list(1,2,3,4,5,6,7,8, c(1,2), c(1,3), c(2,3), c(3,4), c(4,5), c(5,6)
               ,c(5,7),c(6,8), c(7,8))
values = c(0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7)
inf = 10
distance = matrix(c(
  0, 1, 1, 2, 3, 4, 4, 5,
  1, 0, 1, 2, 3, 4, 4, 5,
  1, 1, 0, 1, 2, 3, 3, 4,
  2, 2, 1, 0, 1, 2, 2, 3,
  3, 3, 2, 1, 0, 1, 1, 2,
  4, 4, 3, 2, 1, 0, 2, 1,
  4, 4, 3, 2, 1, 2, 0, 1,
  5, 5, 4, 3, 2, 1, 1, 0
),
nrow = 8)

# Perform initial homology calculation to determine beta_1
filtration = list(complex, values, TRUE)
names(filtration) = c("cmplx", "values", "increasing")
diagram = filtrationDiag(filtration = filtration, maxdimension = 1,
                         library = "Dionysus")
betti = sum(diagram[[1]][,1] == 1 & diagram[[1]][,3] == Inf)

# Find optimal generators
generators = list()
minSize = 1
seals = list(list(),c())
for (i in seq(betti)) {
  
  # Debugging
  # print("OptimalGenerator:")
  # print(i)
  
  # Calculate the smallest cycle.
  smallest = .measureSmallest(complex, distance, minSize, seals)
  minSize = smallest[2]
  generators = append(generators, smallest[1])
  # Seal the cycle by attaching all vertices in it, modifying the complex
  cover = .cover(unique(unlist(smallest[1])), inf)
  seals[[1]] = append(seals[[1]], cover[[1]])
  seals[[2]] = append(seals[[2]], cover[[2]])
}

print(generators)
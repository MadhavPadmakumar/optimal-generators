library(TDA)
complex = list(1, 2, 3, 4, c(1, 2), c(2, 3), c(3, 4), c(1,4))
values = c(0, 0, 0, 0, 1, 1, 1,1)



filtration = list(complex, values, TRUE)
names(filtration) = c("cmplx", "values", "increasing")

diagram = filtrationDiag(filtration = filtration, maxdimension = 1,
                         library = "Dionysus", location = TRUE)
plot(diagram[["diagram"]])
diagram


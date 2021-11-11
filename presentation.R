X = cbind(c(1,2.3,3.8,7.5,9.4,0.2), c(1,4,1.8,3.9,2.7,3.4))
maxdimension = 1
maxscale = 20
dist = "euclidean"
library = "Dionysus"

FltRips = ripsFiltration(X = X, maxdimension = maxdimension, maxscale = maxscale,
                         dist = dist, library = library)

DiagFltRips = filtrationDiag(filtration = FltRips, maxdimension = maxdimension, library = library, location = TRUE)

plot(DiagFltRips[["diagram"]])

# plot(X)
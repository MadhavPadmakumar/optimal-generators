library(TDA)
library(data.table)
library(phom)
ASD = read.csv('DM.counts.csv')
maxdimension = 2
maxscale = 20
dist = "euclidean"
library = "Dionysus"

rownames(ASD) = ASD[,1]
ASD[,1] = NULL #NOTE! Do this IF AND ONLY IF running the whole script
# tASD = scale(transpose(ASD))



FltRips = ripsFiltration(X = ASD, maxdimension = maxdimension, maxscale = maxscale,
                         dist = dist, library = library)

DiagFltRips = filtrationDiag(filtration = FltRips, maxdimension = maxdimension,
                             library = library, location = TRUE)

plot(DiagFltRips[["diagram"]])


# intervals = pHom(ASD, 2, 20)
# plotPersistenceDiagram(intervals, 2, 20)
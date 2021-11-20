data = read.table(file="ratios_forclusterayasdi.tsv", sep='\t', header=TRUE)
rownames(data) = data[,1]
data[,1] = NULL
data[,1] = NULL

maxdimension = 2
maxscale = 20
dist = "euclidean"
library = "Dionysus"

FltRips = ripsFiltration(X = data, maxdimension = maxdimension, maxscale = maxscale,
                         dist = dist, library = library)

DiagFltRips = filtrationDiag(filtration = FltRips, maxdimension = maxdimension,
                             library = library, location = TRUE)

plot(DiagFltRips[["diagram"]])

cycles = DiagFltRips[["cycleLocation"]][205]
cycles = unique(unlist(cycles))

genes1 = rownames(data)[cycles]

cycles = DiagFltRips[["cycleLocation"]][220]
cycles = unique(unlist(cycles))
genes2 = rownames(data)[cycles]
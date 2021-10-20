n = 30
X = cbind(cos(2*pi*seq_len(n)/n), sin(2*pi*seq_len(n)/n))
# Y = cbind(1.1*cos(2*pi*seq_len(n)/n), 1.1*sin(2*pi*seq_len(n)/n))
# X = rbind(X, Y)
X = rbind(X, c(0, 0))
maxdimension = 1
maxscale = 20
dist = "euclidean"
library = "Dionysus"

FltRips = ripsFiltration(X = X, maxdimension = maxdimension, maxscale = maxscale,
                         dist = dist, library = library)

DiagFltRips = filtrationDiag(filtration = FltRips, maxdimension = maxdimension, library = library, location = TRUE)

# plot(DiagFltRips[["diagram"]])

plot(X)

# Find optimal step in filtration
t = 1
values = FltRips[["values"]] <= t
# Plot 1-cells
cmplx = FltRips[["cmplx"]][values]
oneCells = cmplx[unlist(lapply(cmplx, function(x) length(x) == 2))]
for (i in seq_len(length(oneCells))){
  lines(X[oneCells[[i]],])
}
  
# Plot 2-cells

# Plot cycle
index = 33
for (i in seq_len(dim(DiagFltRips[["cycleLocation"]][[index]])[1])){
  # print(X[DiagFltRips[["cycleLocation"]][[index]][i,],])
  lines(
    X[DiagFltRips[["cycleLocation"]][[index]][i,],],
    col="red"
  )
}
help("sapply") 
help("apply")
help("lapply")
help("c")

## Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
class(x)
View(x)

# apply(X, MARGIN, FUN, ...)
# MARGIN a vector giving the subscripts which the function will be applied over. 
# E.g., for a matrix 1 indicates rows, 2 indicates columns, c(1, 2) indicates rows and columns. 
# Where X has named dimnames, it can be a character vector selecting dimension names.
# FUN the function to be applied
apply(x, 2, sum)
apply(x, 2, mean)
apply(x, 1, mean)
apply(x, 1, sum)


#Sum each row and column of the matrix.  1 indicates rows 2 indicates columns.
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)
column_M <- cbind(x, Rtot = row.sums)
View(column_M)

rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))





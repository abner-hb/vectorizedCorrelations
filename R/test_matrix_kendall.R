X = matrix (c(1, 2, 2, 1), 2, 2)
Y = t(matrix(1:nrow(X), nrow=nrow(X), ncol=ncol(X)))
all.equal(
    as.vector(cor(X, 1:nrow(X), method = "kendall")),
    matrix_kendall(X, Y)
)


X = matrix(c(2, 2, 1, 1), 2, 2)
Y = t(matrix(1:nrow(X), nrow=nrow(X), ncol=ncol(X)))
all.equal(
    as.vector(cor(X, 1:nrow(X), method = "kendall")),
    matrix_kendall(X, Y)
)
set.seed(854)
test = matrix(rnorm(100000), nrow=1000, ncol=1000)
matrix_kendall(test)
microbenchmark::microbenchmark(matrix_kendall(test), times = 5)

test_matrix_kendall = function(){
    mtrx = matrix(rnorm(100000), nrow=1000, ncol=1000)
    n = 10
    max_tau = rep(NA_integer_, n)
    for (i in 1:n) {
        max_tau[i] = matrix_kendall(mtrx)
    }
    return(max_tau)
}
set.seed(854)
profvis::profvis(test_matrix_kendall())

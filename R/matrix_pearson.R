matrix_pearson = function(X, Y) {
    if (is.vector(X)) {
        X = matrix(X, ncol=1)
    }
    if (is.vector(Y)) {
        Y = matrix(Y, ncol=1)
    }
    n = nrow(X)
    cov_mat = matrix(NA_real_, nrow=ncol(X), ncol=ncol(Y))
    cntrd_X = t(t(X) - colMeans(X))
    cntrd_Y = t(t(Y) - colMeans(Y))
    for (i in 1:ncol(X)) {
        product = cntrd_X[, i] * cntrd_Y
        cov_mat[i,] = colSums(product) / (n - 1)
    }
    if (all(dim(cov_mat) == c(1,1))) {
        cov_mat = cov_mat[1,1]
    }
    return(cov_mat)
}

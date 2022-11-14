matrix_pearson = function(X, Y) {
    # if (is.vector(X)) {
    #     X = matrix(X, ncol=1)
    # }
    # if (is.vector(Y)) {
    #     Y = matrix(Y, ncol=1)
    # }
    n = nrow(X)
    # Center matrices
    cntrd_X = t(t(X) - colMeans(X))
    cntrd_Y = t(t(Y) - colMeans(Y))
    # Compute variances
    Xstd_vec = sqrt(colSums(cntrd_X^2))
    Ystd_vec = sqrt(colSums(cntrd_Y^2))
    corr_mat = matrix(NA_real_, nrow=ncol(X), ncol=ncol(Y))
    for (i in 1:ncol(X)) {
        # Computing covariances
        covariance_vec = colSums(cntrd_X[, i] * cntrd_Y)
        # Compute correlations
        corr_mat[i,] = covariance_vec / (Xstd_vec[i]*Ystd_vec)
    }
    # Convert 1 by 1 matrix to number
    if (all(dim(corr_mat) == c(1,1))) {
        corr_mat = corr_mat[1,1]
    }
    return(corr_mat)
}

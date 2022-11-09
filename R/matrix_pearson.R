matrix_pearson = function(X, Y) {
    n = nrow(X)
    covariance = n*colSums(X*Y) - colSums(X)*colSums(Y)
    var_X = n*colSums(X^2) - colSums(X)^2
    var_Y = n*colSums(Y^2) - colSums(Y)^2
    log_rho = log(covariance) - 0.5*log(var_X) - 0.5*log(var_Y)
    return(exp(log_rho))
}

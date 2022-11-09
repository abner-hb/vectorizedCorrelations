matrix_kendall = function(X, Y){
    conc_pairs = rep(0L, nrow(X))
    for (i in 1:(ncol(X) - 1)) {
        conc_pairs = conc_pairs +
            # Both entries are smaller
            rowSums(X[, i] < X[, i:ncol(X)]) * rowSums(Y[, i] < Y[, i:ncol(X)]) +
            # Both entries are bigger
            rowSums(X[, i] > X[, i:ncol(Y)]) * rowSums(Y[, i] > Y[, i:ncol(Y)])
    }
    tau_vec = 4*conc_pairs / (ncol(X)*(ncol(X) - 1)) - 1
    return(tau_vec)
}

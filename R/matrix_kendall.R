matrix_kendall = function(X, Y){
    # Transpose so I can use row operations
    X = t(X)
    Y = t(Y)
    conc_pairs = rep(0L, nrow(X))
    for (i in 2:ncol(X)) {
        conc_pairs[] = conc_pairs +
            # Both entries are smaller
            rowSums(
                # Counting the equal cases allows to consider ties
                (X[, i - 1] <= X[, i:ncol(X), drop=FALSE])*
                    (Y[, i - 1] <= Y[, i:ncol(X), drop=FALSE])
            ) +
            # Both entries are bigger
            rowSums(
                (X[, i - 1] > X[, i:ncol(X), drop=FALSE])*
                    (Y[, i - 1] > Y[, i:ncol(X), drop=FALSE])
            )
    }
    tau_vec = 4*conc_pairs / (ncol(X)*(ncol(X) - 1)) - 1
    return(tau_vec)
}

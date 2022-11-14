#' matrix_kendall
#'
#' @param X A matrix or data.frame
#' @param Y A matrix or data.frame
#'
#' @return returns a matrix `X` containing the correlation
#'  between all the columns of `X` and all the columns of `Y`.
#'
#' @noRd
#'
#' @examples
#' matrix_kendall(matrix(c(1, 2, 2, 1), 2, 2), c(1, 2))
#' matrix_kendall(matrix(rnorm(100), nrow=10), matrix(rnorm(100), nrow=10))
#'
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

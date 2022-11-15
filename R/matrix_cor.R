#' Correlation matrices
#'
#' @description `matrix_cor()` computes the correlation between the columns of
#'   `X` and the columns of `Y`.
#'
#' @param X A matrix or a vector
#' @param Y A matrix or a vector
#' @param method A string
#'
#' @return For `method = "pearson"`, returns a matrix containing the pearson
#'   correlation coefficient between all the columns of `X` and all the columns
#'   of `Y`.
#'
#'   For `method = "kendall"`, returns a *vector* containing the kendall rank
#'   correlation between columns of `X` and the corresponding columns of `Y`.
#'
#' @export
#'
#' @examples
#' matrix_cor(rnorm(20), rnorm(20), method="pearson")
#' matrix_cor(matrix (c(1, 2, 2, 1), 2, 2), c(1,2), method="kendall")
matrix_cor = function(X, Y=NULL, method = "pearson") {
    # Define Y to ease implementation
    if (is.null(Y)) {
        Y = X
    }
    # Check that matrices are numerical
    if (!(is.numeric(X) & is.numeric(Y))) {
        stop("arguments have non-numeric values")
    }
    # Ensure X is a matrix
    if (is.vector(X)) {
        X = matrix(X, ncol=1)
    }
    if (method == "pearson") {
        if (is.vector(Y)) {
            Y = matrix(Y, ncol=1)
        }
        incompat_dim =
            # Vectors have different sizes
            (nrow(X) != nrow(Y)) |
            # Different number of vectors
            (ncol(X) > 1 & ncol(Y) > 1 & (ncol(X) != ncol(Y)))
        if (incompat_dim) {
            stop("incompatible dimensions")
        }
        corr_mat = matrix_pearson(X, Y)
    } else if (method == "kendall") {
        if (is.vector(Y)) {
            Y = matrix(Y, nrow=length(Y), ncol=ncol(X))
        } else if (ncol(X) == 1) {
            X = matrix(X, nrow=nrow(X), ncol=ncol(Y))
        }
        incompat_dim = (nrow(X) != nrow(Y)) | (ncol(X) != ncol(Y))
        if (incompat_dim) {
            stop("incompatible dimensions")
        }
        corr_mat = matrix_kendall(X, Y)
    } else {
        stop("method must be one of 'pearson' or 'kendall'")
    }
    return(corr_mat)
}

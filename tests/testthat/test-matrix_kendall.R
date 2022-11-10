test_that("matrix_kendall works", {
    # Case 1
    X = t(matrix (c(1, 2, 2, 1), 2, 2))
    Y = matrix(1:nrow(X), nrow=nrow(X), ncol=ncol(X))
    expect_equal(
        matrix_kendall(X, Y),
        diag(cor(X, Y, method = "kendall"))
    )
    # Case 2
    X = t(matrix (c(1, 2, 2, 1), 2, 2))
    Y = 1:nrow(X)
    expect_equal(
        matrix_kendall(X, Y),
        as.vector(cor(X, Y, method = "kendall"))
    )
    # case 3
    X = t(matrix(c(2, 2, 1, 1), 2, 2))
    Y = matrix(1:nrow(X), nrow=nrow(X), ncol=ncol(X))
    expect_equal(
        matrix_kendall(X, Y),
        diag(cor(X, Y, method = "kendall"))
    )
    # Case 4
    set.seed(854)
    X = matrix(rnorm(100*100), nrow=100, ncol=100)
    Y = matrix(rnorm(100*100), nrow=100, ncol=100)
    expect_equal(
        matrix_kendall(X, Y),
        diag(cor(X, Y, method = "kendall"))
    )
    # Case 5
    X = matrix(1:10, nrow=10, ncol=10)
    Y = X
    expect_equal(
        matrix_kendall(X, Y),
        diag(cor(X, Y, method = "kendall"))
    )
    # Case 6
})

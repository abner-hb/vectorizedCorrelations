test_that("matrix_cor works", {
    # Case 1
    set.seed(9008)
    expect_error(
        matrix_cor(rnorm(5), rnorm(5), method="lame"),
        regexp = "method must be one of 'pearson' or 'kendall'"
    )
    # Case 2
    # expect_error(
    #     matrix_cor(rnorm(5), rnorm(5), method=NULL),
    #     regexp = "method must be one of 'pearson' or 'kendall'"
    # )
    # Case 3
    expect_error(
        matrix_cor(rnorm(5), rnorm(5), method="paerson"),
        regexp = "method must be one of 'pearson' or 'kendall'"
    )
    # Case 4
    expect_error(
        matrix_cor(rnorm(7), rnorm(5), method="pearson"),
        regexp = "incompatible dimensions"
    )
    # Case 5
    set.seed(201)
    X = matrix(rnorm(45), nrow=5)
    Y = rnorm(7)
    expect_error(
        matrix_cor(X, Y, method="kendall"),
        regexp = "incompatible dimensions"
    )
    # Case 6
    set.seed(29)
    X = matrix(rnorm(100), nrow=10)
    Y = matrix(rnorm(50), nrow=5)
    expect_error(
        matrix_cor(X, Y, method="pearson"),
        regexp = "incompatible dimensions"
    )
    # Case 7a
    set.seed(29)
    X = as.character(matrix(rnorm(100), nrow=10))
    Y = matrix(rnorm(50), nrow=5)
    expect_error(
        matrix_cor(X, Y, method="kendall"),
        regexp = "arguments have non-numeric values"
    )
    # Case 7b
    set.seed(29)
    X = matrix(rnorm(100), nrow=10)
    Y = as.character(matrix(rnorm(50), nrow=5))
    expect_error(
        matrix_cor(X, Y, method="kendall"),
        regexp = "arguments have non-numeric values"
    )
    # Case 8
    X = t(matrix (c(1, 2, 2, 1), 2, 2))
    Y = 1:nrow(X)
    expect_equal(
        matrix_cor(X, Y, method = "kendall"),
        as.vector(cor(X, Y, method = "kendall"))
    )
    # Case 9
    set.seed(56)
    X = rnorm(200)
    Y = rnorm(200)
    expect_equal(
        matrix_cor(X, Y, method="kendall"),
        cor(X, Y, method = "kendall")
    )
    # Case 10
    set.seed(56)
    X = rnorm(200)
    Y = rnorm(200)
    expect_equal(
        matrix_cor(X, Y, method="pearson"),
        cor(X, Y, method = "pearson")
    )
    # Case 11
    set.seed(201)
    X = matrix(rnorm(45), nrow=5)
    Y = rnorm(5)
    expect_equal(
        matrix_cor(X, Y, method="kendall"),
        as.vector(cor(X, Y, method = "kendall"))
    )
    # Case 12
    set.seed(201)
    X = rnorm(5)
    Y = matrix(rnorm(45), nrow=5)
    expect_equal(
        matrix_cor(X, Y, method="kendall"),
        as.vector(cor(X, Y, method = "kendall"))
    )
    # Case 13
    set.seed(56)
    X = rnorm(200)
    Y = rnorm(200)
    expect_equal(
        matrix_cor(X, Y, method="kendall"),
        cor(X, Y, method = "kendall")
    )
    # Case 14
    set.seed(201)
    X = rnorm(5)
    Y = matrix(rnorm(45), nrow=5)
    expect_equal(
        matrix_cor(X, Y, method="pearson"),
        cor(X, Y, method = "pearson")
    )
    # Case 15
    set.seed(20)
    X = matrix(rnorm(1000*100), nrow=1000, ncol=100)
    Y = rcauchy(1000)
    expect_equal(
        cor(X, Y),
        matrix_cor(X, Y, "pearson")
    )
    # Case 16
    set.seed(20)
    X = matrix(rnorm(10*5), nrow=10, ncol=5)
    expect_equal(
        cor(X),
        matrix_cor(X, method="pearson")
    )
    # Case 16
    set.seed(20)
    X = matrix(rnorm(10*5), nrow=10, ncol=5)
    expect_equal(
        diag(cor(X, method="kendall")),
        matrix_cor(X, method="kendall")
    )
})

test_that("pearson works", {
    # Case 1
    set.seed(1120)
    X = matrix(sample.int(100, size=50), nrow=10, ncol=5)
    Y = matrix(sample.int(100, size=50), nrow=10, ncol=5)
    expect_equal(
        cor(X, Y),
        matrix_pearson(X, Y)
    )
    # Case 2
    set.seed(1120)
    X = matrix(rnorm(100*10, mean = 30, sd = 0.001), nrow=100, ncol=10)
    Y = matrix(rnorm(100*10, mean = 30, sd = 0.001), nrow=100, ncol=10)
    expect_equal(
        cor(X, Y),
        matrix_pearson(X, Y)
    )
    # Case 3
    set.seed(600)
    X = matrix(rnorm(100000), nrow=1000, ncol=1000)
    Y = matrix(rnorm(100000), nrow=1000, ncol=1000)
    expect_equal(
        cor(X, Y),
        matrix_pearson(X, Y)
    )
    # Case 4
    set.seed(121)
    X = matrix(rnorm(1000*100), nrow=1000, ncol=100)
    Y = matrix(rnorm(1000*1000), nrow=1000, ncol=1000)
    expect_equal(
        cor(X, Y),
        matrix_pearson(X, Y)
    )
    # Case 5
    set.seed(20)
    X = matrix(rnorm(1000*100), nrow=1000, ncol=100)
    Y = matrix(rcauchy(1000), nrow=1000, ncol=1)
    expect_equal(
        cor(X, Y),
        matrix_pearson(X, Y)
    )
    # Case 6
    set.seed(20)
    X = matrix(rcauchy(1000), nrow=1000, ncol=1)
    Y = matrix(rnorm(1000*100), nrow=1000, ncol=100)
    expect_equal(
        cor(X, Y),
        matrix_pearson(X, Y)
    )
    # Case 7
    set.seed(20)
    X = matrix(rcauchy(1000), nrow=1000, ncol=1)
    Y = matrix(rcauchy(1000), nrow=1000, ncol=1)
    expect_equal(
        as.vector(cor(X, Y)),
        matrix_pearson(X, Y)
    )
    # Case 8
    set.seed(20)
    X = matrix(rgamma(73*5000, shape = 10), ncol = 73, nrow=5000)
    Y = X + sample.int(100, size=73*5000, replace = TRUE)
    expect_equal(
        cor(X, Y),
        matrix_pearson(X, Y)
    )
    # Case 9
    set.seed(29)
    X = matrix(rnorm(100), nrow=10)
    Y = X
    expect_equal(
        cor(X, Y),
        matrix_pearson(X, Y)
    )
    # Case 10
    set.seed(29)
    X = data.frame(matrix(rnorm(100), nrow=10))
    Y = X
    expect_equal(
        unname(cor(X, Y)),
        matrix_pearson(X, Y)
    )
})

# test_matrix_pearson = function(reps){
#     X = matrix(rnorm(1000*500), nrow=1000, ncol=500)
#     Y = matrix(rnorm(1000*500), nrow=1000, ncol=500)
#     m = reps
#     result = matrix_pearson(X, Y)
#     for (i in 1:(m-1)) {
#         result[] = matrix_pearson(X, Y)
#     }
#     return(result)
# }
# set.seed(2004)
# profvis::profvis(test_matrix_pearson(10))
# #
# test_r_pearson = function(reps){
#     X = matrix(rnorm(1000*500), nrow=1000, ncol=500)
#     Y = matrix(rnorm(1000*500), nrow=1000, ncol=500)
#     m = reps
#     result = cor(X, Y)
#     for (i in 1:(m-1)) {
#         result[] = cor(X, Y)
#     }
#     return(result)
# }
# set.seed(2004)
# profvis::profvis(test_r_pearson(10))

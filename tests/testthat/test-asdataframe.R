context("AsDataFrame")


mat <- matrix(seq(1:100), nrow = 5, ncol = 20)
colnames(mat) <- letters[1:20]
rownames(mat) <- c("where", "is", "the", "love", "at")

df <- data.frame(matrix(seq(1:32), nrow = 4, ncol = 8))
v1 <- c(seq(101, 104))
v2 <- c(seq(1001, 1004))
v3 <- c(seq(10001, 10004))

l1 <- list(df, v1, v2)
l2 <- list(v1, v2, v3)

test_that("AsDataFrame",
{
    expect_equal(nrow(AsDataFrame(mat, ignore.columns = "k, p, the")), 4)
    expect_equal(ncol(AsDataFrame(mat, ignore.columns = "k, p, the")), 18)
    expect_equal(ncol(AsDataFrame(l1)), 10)
    expect_equal(ncol(AsDataFrame(l2)), 3)
})

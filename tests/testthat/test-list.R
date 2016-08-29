context("list")

test_that("ListToDataFrame works as expected",
{
    tdf <- data.frame("A" = c(1,2,3), "B.1" = c(1,0,0), "B.2" = c(0,1,0), "B.3" = c(0,0,1))
    colnames(tdf) <- c("A", "B.1", "B.2", "B.3")
    row.names(tdf) <- as.character(row.names(tdf))
    expect_equal(ListToDataFrame(list("A" = c(1,2,3), "B" = as.factor(c("a","b","c"))), binary = TRUE), tdf)
})

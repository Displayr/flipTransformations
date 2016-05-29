context("list")

test_that("ListToDataFrame works as expected",
{
    tdf <- data.frame("A" = c(1,2,3), "B:a" = c(1,0,0), "B:b" = c(0,1,0), "B:c" = c(0,0,1))
    colnames(tdf) <- c("A", "B:a", "B:b", "B:c")
    row.names(tdf) <- as.character(row.names(tdf))
    expect_equal(ListToDataFrame(list("A" = c(1,2,3), "B" = as.factor(c("a","b","c"))), coerce.to.numeric = TRUE), tdf)

    expect_error(ListToDataFrame(list("A" = LETTERS[1:3])), "Variable 'A' is a Text variable.")
})

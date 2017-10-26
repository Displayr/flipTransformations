context("list")

test_that("asNumericList works as expected",
{
    tdf <- data.frame("A" = c(1,2,3), "B.1" = c(1,0,0), "B.2" = c(0,1,0), "B.3" = c(0,0,1))
    colnames(tdf) <- c("A", "B.1", "B.2", "B.3")
    row.names(tdf) <- as.character(row.names(tdf))
    tdf1 <- asNumericList(list("A" = c(1,2,3), "B" = as.factor(c("a","b","c"))), binary = TRUE)
    expect_equal(names(tdf1), names(tdf))
})

test_that("QuestionListToDataFrame works as expected",
{
    q1 <- data.frame("A" = c(0,0,0), "B" = c(1,0,0), "C" = c(0,1,0), "NET" = c(1,1,0))
    q2 <- 1:3
    attr(q2, "question") <- "q2"
    q.list <- list(q1, q2)
    tdf <- data.frame("A" = c(0,0,0), "B" = c(1,0,0), "C" = c(0,1,0), "q2" = q2)
    expect_equal(QuestionListToDataFrame(q.list), tdf)
})


test_that("SplitVectorToList",
{
    y <- 1:4
    gr <- c("A", "B", "A", "A")
    expect_equal(SplitVectorToList(y, gr), as.array(list(A = c(1,3,4), B = 2)))
})



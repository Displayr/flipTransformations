context("list")

test_that("asNumericList works as expected",
{
    tdf <- data.frame("A" = c(1,2,3), "B.1" = c(1,0,0), "B.2" = c(0,1,0), "B.3" = c(0,0,1))
    colnames(tdf) <- c("A", "B.1", "B.2", "B.3")
    row.names(tdf) <- as.character(row.names(tdf))
    tdf1 <- asNumericList(list("A" = c(1,2,3), "B" = as.factor(c("a","b","c"))), binary = TRUE)
    expect_equal(names(tdf1), names(tdf))

    df.mixed <- structure(list(Name = c("Dragonfruit", "Beans", "Carrot", "Apple"
        ), Date = structure(c(954547200, 962409600, 966297600, 973036800
        ), class = c("POSIXct", "POSIXt"), tzone = "UTC"), Score = c(5,
        7, 3.3, 10)), .Names = c("Name", "Date", "Score"), row.names = c("B",
        "D", "C", "A"), class = "data.frame")
    expect_warning(res <- asNumericList(df.mixed, binary = FALSE))
    expect_equal(sapply(res, class), c(Name = "integer", Date = "numeric", Score = "numeric"))
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


test_that("SplitVectorToList numeric values",
{
    y <- 1:4
    gr <- c("A", "B", "A", "A")
    out <- SplitVectorToList(y, gr)
    expect_is(out, "list")
    expect_equal(out, list(A = c(1,3,4), B = 2))
})

test_that("SplitVectorToList error if groups wrong length",
{
    expect_error(SplitVectorToList(1:2, 1), "must have the same length.$")
})

test_that("SplitVectorToList NAs in groups",
{
    y <- 1:4
    gr <- c("A", NA, "A", NA)
    out <- SplitVectorToList(y, gr)
    expect_is(out, "list")
    expect_equal(names(out), levels(as.factor(gr)))
    expect_equal(out[[1]], which(!is.na(gr)))
})

test_that("SplitVectorToList factor values",
{
    y <- as.factor(c("dog", "sheep", "cat", "dog", "sheep"))
    gr <- c(0, 1, 1, 1, 0)
    out <- SplitVectorToList(y, gr)
    expect_is(out, "list")
    expect_equal(names(out), levels(as.factor(gr)))
    expect_is(out[[1]], "factor")
    expect_equal(levels(out[[2]]), levels(y))
})

test_that("AsNumeric, doesn't mangle non-categorical VS; DS-2906'",
{
    vs.env <- new.env()
    file <- system.file("extdata", "variable.sets.rda", package = "flipTransformations")
    load(file, vs.env)
    expect_false(flipTransformations:::isCategoricalMultiVariableSet(vs.env$binary.grid))
    expect_false(flipTransformations:::isCategoricalMultiVariableSet(vs.env$numeric.multi))
    expect_false(flipTransformations:::isCategoricalMultiVariableSet(vs.env$numeric.grid))
    expect_false(flipTransformations:::isCategoricalMultiVariableSet(vs.env$binary.multi))
    bg <- vs.env$binary.grid
    bg.an <- AsNumeric(vs.env$binary.grid, FALSE)
    expect_equal(bg.an, bg, check.attributes = FALSE)

    ## check asNumericList works with return.data.frame = FALSE
    ## Note: that this won't get called in production in this way because
    ## mutli variable sets from Q/Displayr are always data.frames so
    ## AsNumeric.data.frame (return.data.frame = TRUE) will get called
    nm <- vs.env$nominal.multi
    out <- asNumericList(nm, FALSE, return.data.frame = FALSE)
    out.df <- asNumericList(nm, FALSE, return.data.frame = TRUE)
    expect_is(out, "list")
    expect_length(out, ncol(nm))
    expect_is(out[[1]], "numeric")
    expect_equal(as.data.frame(out), out.df, check.attributes = FALSE)
})


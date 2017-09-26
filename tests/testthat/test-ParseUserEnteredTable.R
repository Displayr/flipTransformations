context("ParseUserEnteredTable")

test_that("label in 1x1 cell",
{
    m <- matrix("1", 3, 3)
    m[1, ] <- LETTERS[1:3]
    m[2:3, 1] <- letters[1:2]
    out <- ParseUserEnteredTable(m)
    expect_equal(dim(out), c(2, 2))
    expect_equal(attr(out, "statistic"), "A")
    expect_equal(rownames(out), letters[1:2])
    expect_equal(colnames(out), LETTERS[2:3])
})

test_that("no label in 1x1 cell",
{
    m <- matrix("1", 3, 3)
    m[1, 1] <- ""
    m[1, 2:3] <- LETTERS[1:2]
    m[2:3, 1] <- letters[1:2]
    out <- ParseUserEnteredTable(m)
    expect_equal(rownames(out), letters[1:2])
    expect_equal(colnames(out), LETTERS[1:2])
    expect_equal(dim(out), c(2, 2))
    expect_true(is.numeric(out))
})


test_that("DS-1471: TB comment",
{
    m <- matrix(as.character(sample(9, 5*9, replace = TRUE)), 5, 9)
    m[1, 1] <- ""
    m[2:nrow(m), 1] <- LETTERS[1:(nrow(m)-1)]
    #m[2:3, 1] <- letters[1:2]
    out <- ParseUserEnteredTable(m)
    expect_equal(colnames(out), m[1, -1])
    expect_true(is.numeric(out))
})

test_that("DS-1471: TB comment transposed",
{
    m <- matrix(as.character(sample(9, 5*9, replace = TRUE)), 5, 9)
    m[1, 1] <- ""
    m[2:nrow(m), 1] <- LETTERS[1:(nrow(m)-1)]
    m <- t(m)
    #m[2:3, 1] <- letters[1:2]
    out <- ParseUserEnteredTable(m)
    expect_equal(colnames(out), m[1, -1])
    expect_equal(colnames(out), m[1, -1])
    expect_true(is.numeric(out))
})

test_that("row names, no column names",
{
    m <- matrix(as.character(sample(9, 5*9, replace = TRUE)), 5, 9)
    m[1:nrow(m), 1] <- LETTERS[1:nrow(m)]

    out <- ParseUserEnteredTable(m)
    expect_null(colnames(out))
    expect_equal(rownames(out), m[, 1])
})

test_that("column names, no row names",
{
    m <- matrix(as.character(sample(9, 5*9, replace = TRUE)), 5, 9)
    # m[1, 1] <- ""
    m[1:nrow(m), 1] <- LETTERS[1:nrow(m)]
    m <- t(m)
    #m[2:3, 1] <- letters[1:2]
    out <- ParseUserEnteredTable(m)
    expect_null(rownames(out))
    expect_equal(colnames(out), m[1, ])
})

test_that("row vector",
{
    n.row <- 1
    n.col <- 10
    m <- matrix(rep("1", n.row*n.col), n.row, n.col)
    out <- ParseUserEnteredTable(m)
    expect_equal(out, rep.int(1, n.col))
})

test_that("named row vector",
{
    n.col <- 10
    m <- rbind(letters[1:n.col], rep("1", n.col))

    out <- ParseUserEnteredTable(m)
    expect_equal(names(out), m[1, ])
    expect_equal(out, as.numeric(m[2, ]), check.attributes = FALSE)
})

test_that("column vector",
{
    n.row <- 10
    n.col <- 1
    m <- matrix(rep("1", n.row*n.col), n.row, n.col)
    out <- ParseUserEnteredTable(m)
    expect_equal(out, rep.int(1, n.row))
    expect_null(names(out))
})

test_that("named column vector",
{
    n.col <- 6
    m <- cbind(letters[1:n.col], rep("1", n.col))

    out <- ParseUserEnteredTable(m)
    expect_equal(names(out), m[, 1])
    expect_equal(drop(out), as.numeric(m[, 2]), check.attributes = FALSE)
})

test_that("2 x 3",
{
    n.row <- 2
    n.col <- 3
    m <- matrix(as.character(seq_len(n.row*n.col)), n.row, n.col)
    # m[1, 1] <- ""
    m[1:nrow(m), 1] <- LETTERS[1:nrow(m)]
    m[1, 2:ncol(m)] <- letters[2:ncol(m)]
    m <- t(m)
    #m[2:3, 1] <- letters[1:2]
    out <- ParseUserEnteredTable(m)
    expect_equal(attr(out, "statistic"), m[1, 1])
    expect_equal(colnames(out), m[1, -1])
})


## start old tests for flipTransformations/R/dataentry.R
## taken from flipTransformations/tests/testthat/test-ParseUserEnteredTable.R
test_that("numeric matrix without names", {
    raw.matrix <- structure(c("", "", "", "", "", "", "", "", "1", "", "999", "-111",
                              "", "", "3", "2", "3.14", "", "", "", "", "6", "", "7"), .Dim = c(6L, 4L))
    expect_equal(ParseUserEnteredTable(raw.matrix),
                 structure(c(1, NA, 999, -111, 3, 2, 3.14, NA, NA, 6, NA, 7), .Dim = c(4L, 3L)))
})

test_that("numeric vector without names", {
    raw.matrix <- structure(c("", "", "", "", "", "", "", "", "", "", "", "", "1",
                              "2", "3", "", "5", "6"), .Dim = c(9L, 2L))
    expect_equal(ParseUserEnteredTable(raw.matrix), c(1, 2, 3, NA, 5, 6))
})

test_that("numeric vector with names", {
    raw.matrix <- structure(c("one", "two", "three", "", "five", "six", "1",
                              "2", "3", "", "5", "6"), .Dim = c(6L, 2L))
    expect_equal(ParseUserEnteredTable(raw.matrix), structure(c(1, 2, 3, NA, 5, 6),
                                                         .Names = c("one", "two", "three", "", "five", "six")))
})

test_that("numeric matrix with row names", {
    raw.matrix <- structure(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "1",
                "2", "5", "2", "5", "7", "3", "3", "1", "5", "2", "2", "4", "6",
                "7", "4", "3", "2"), .Dim = c(9L, 3L))
    expect_equal(ParseUserEnteredTable(raw.matrix),
                 structure(c(1, 2, 5, 2, 5, 7, 3, 3, 1, 5, 2, 2, 4, 6, 7, 4, 3, 2),
                           .Dim = c(9L, 2L),
                           .Dimnames = list(c("a", "b", "c", "d", "e", "f", "g", "h", "i"), NULL)))
})

test_that("numeric matrix with column names", {
    raw.matrix <- structure(c("x", "1", "2", "5", "2", "5", "7", "3", "3", "1",
                              "y", "5", "2", "2", "4", "6", "7", "4", "3", "2"), .Dim = c(10L, 2L))
    expect_equal(ParseUserEnteredTable(raw.matrix),
                 structure(c(1, 2, 5, 2, 5, 7, 3, 3, 1, 5, 2, 2, 4, 6, 7, 4, 3,
                             2), .Dim = c(9L, 2L), .Dimnames = list(NULL, c("x", "y"))))
})

test_that("numeric matrix with names", {
    raw.matrix <- structure(c("", "", "", "", "", "Height", "Weight", "Strength",
                "Australia", "8", "8", "7", "USA", "7", "10", "10", "Denmark",
                "10", "4", "2"), .Dim = 4:5)
    expect_equal(ParseUserEnteredTable(raw.matrix), structure(c(8, 8, 7, 7, 10, 10, 10, 4, 2), .Dim = c(3L, 3L), .Dimnames = list(
        c("Height", "Weight", "Strength"), c("Australia", "USA", "Denmark"))))
})

test_that("numeric matrix with names and titles", {
    raw.matrix <- structure(c("", "", "Product", "", "", "", "", "", "", "", "",
                              "", "Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi",
                              "Pepsi Max", "None of these", "NET", "Attribute", "Feminine",
                              "0.064220183", "0.574923547", "0.22324159", "0.085626911", "0.605504587",
                              "0.100917431", "0.097859327", "1", "", "Health-conscious", "0.018348624",
                              "0.587155963", "0.550458716", "0.021406728", "0.577981651", "0.308868502",
                              "0.174311927", "1", "", "Innocent", "0.091743119", "0.229357798",
                              "0.128440367", "0.097859327", "0.434250765", "0.073394495", "0.29969419",
                              "1", "", "Older", "0.651376147", "0.217125382", "0.04587156",
                              "0.379204893", "0.091743119", "0.064220183", "0.085626911", "1",
                              "", "Open to new experiences", "0.226299694", "0.091743119",
                              "0.519877676", "0.155963303", "0.162079511", "0.504587156", "0.119266055",
                              "1", "", "Rebellious", "0.262996942", "0.04587156", "0.314984709",
                              "0.177370031", "0.039755352", "0.44648318", "0.159021407", "1",
                              "", "Sleepy", "0.091743119", "0.235474006", "0.091743119", "0.143730887",
                              "0.296636086", "0.064220183", "0.388379205", "1", "", "Traditional",
                              "0.923547401", "0.146788991", "0.03058104", "0.5382263", "0.033639144",
                              "0.039755352", "0.027522936", "1", "", "Weight-conscious", "0.006116208",
                              "0.764525994", "0.645259939", "0", "0.764525994", "0.406727829",
                              "0.055045872", "1", "", "NET", "0.981651376", "0.923547401",
                              "0.908256881", "0.788990826", "0.951070336", "0.868501529", "0.574923547",
                              "1"), .Dim = c(10L, 12L))
    expect_equal(ParseUserEnteredTable(raw.matrix),
                 structure(c(0.064220183, 0.574923547, 0.22324159, 0.085626911,
                            0.605504587, 0.100917431, 0.097859327, 1, 0.018348624, 0.587155963,
                            0.550458716, 0.021406728, 0.577981651, 0.308868502, 0.174311927,
                            1, 0.091743119, 0.229357798, 0.128440367, 0.097859327, 0.434250765,
                            0.073394495, 0.29969419, 1, 0.651376147, 0.217125382, 0.04587156,
                            0.379204893, 0.091743119, 0.064220183, 0.085626911, 1, 0.226299694,
                            0.091743119, 0.519877676, 0.155963303, 0.162079511, 0.504587156,
                            0.119266055, 1, 0.262996942, 0.04587156, 0.314984709, 0.177370031,
                            0.039755352, 0.44648318, 0.159021407, 1, 0.091743119, 0.235474006,
                            0.091743119, 0.143730887, 0.296636086, 0.064220183, 0.388379205,
                            1, 0.923547401, 0.146788991, 0.03058104, 0.5382263, 0.033639144,
                            0.039755352, 0.027522936, 1, 0.006116208, 0.764525994, 0.645259939,
                            0, 0.764525994, 0.406727829, 0.055045872, 1, 0.981651376, 0.923547401,
                            0.908256881, 0.788990826, 0.951070336, 0.868501529, 0.574923547, 1),
                           .Dim = c(8L, 10L),
                           .Dimnames = list(c("Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max",
                                              "None of these", "NET"),
                                            c("Feminine", "Health-conscious", "Innocent", "Older", "Open to new experiences",
                                              "Rebellious", "Sleepy", "Traditional", "Weight-conscious", "NET")),
                           row.column.names = c("Product", "Attribute")))
})

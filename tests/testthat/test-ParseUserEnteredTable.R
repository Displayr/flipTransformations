context("ParseUserEnteredTable")

test_that("label in 1x1 cell",
{
    m <- matrix("1", 3, 3)
    m[1, ] <- c("%", LETTERS[1:2])
    m[2:3, 1] <- letters[1:2]
    out <- ParseUserEnteredTable(m)
    expect_equal(dim(out), c(2, 2))
    expect_equal(attr(out, "statistic"), "%")
    expect_equal(rownames(out), letters[1:2])
    expect_equal(colnames(out), LETTERS[1:2])
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
    expect_equal(out, matrix(1, ncol = n.col))
})

test_that("named row vector",
{
    n.col <- 10
    m <- rbind(letters[1:n.col], rep("1", n.col))

    out <- ParseUserEnteredTable(m)
    expect_equal(colnames(out), m[1, ])
    expect_equal(unname(out), as.numeric(m[2, ]), check.attributes = FALSE)
})

test_that("column vector",
{
    n.row <- 10
    n.col <- 1
    m <- matrix(rep("1", n.row*n.col), n.row, n.col)
    out <- ParseUserEnteredTable(m)
    expect_equal(out, matrix(1, 10, 1))
    expect_null(names(out))
})

test_that("named column vector",
{
    n.col <- 6
    m <- cbind(letters[1:n.col], rep("1", n.col))

    out <- ParseUserEnteredTable(m)
    expect_equal(rownames(out), m[, 1])
    expect_equal(drop(out), as.numeric(m[, 2]), check.attributes = FALSE)
})

test_that("2 x 3",
{
    n.row <- 2
    n.col <- 3
    m <- matrix(as.character(seq_len(n.row*n.col)), n.row, n.col)
    m[1:nrow(m), 1] <- LETTERS[1:nrow(m)]
    m[1, 2:ncol(m)] <- letters[2:ncol(m)]
    m <- t(m)
    out <- ParseUserEnteredTable(m)
    expect_equal(colnames(out), m[1,2])
})

test_that("Missing data",
          {
              n.row <- 3
              n.col <- 4
              m <- matrix(as.character(seq_len(n.row * n.col)), n.row, n.col)
              m[2, ] <- c("invalid", "NA", "NaN", "-")
              out <- ParseUserEnteredTable(m)
              expect_equal(sum(out, na.rm = TRUE), 52)
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
    expect_equal(as.numeric(ParseUserEnteredTable(raw.matrix)), c(1, 2, 3, NA, 5, 6))
})

test_that("numeric vector with names", {
    raw.matrix <- structure(c("one", "two", "three", "", "five", "six", "1",
                              "2", "3", "", "5", "6"), .Dim = c(6L, 2L))
    expect_error(out <- ParseUserEnteredTable(raw.matrix), NA)
    expect_true(is.numeric(out))
    expect_equal(rownames(out), c("one", "two", "three", "", "five", "six"))
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
    res <- ParseUserEnteredTable(raw.matrix)
    expect_equal(res,
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

    r2 <- rbind(matrix("", 3, 12), raw.matrix)
    r2[2,1] <- "Main title"
    res2 <- ParseUserEnteredTable(r2)
    expect_equal(dim(res), dim(res2))
    expect_equal(attr(res2, "title"), r2[2,1])
})


test_that("DS-1558: all character table, row/col labels",
{
    x <- structure(c("", "21 July", "5/8 Aug", "22 Aug", "1st Sep", "12/14 Sep",
    "", "Chris", "Immediate contact for new Displayr users who appear in Slack.",
    "Immediate contact for new Displayr users who appear in Slack.",
    "Immediate contact for new Displayr users who appear in Slack.",
    "Review long-resolution tickets", "Immediate contact for new Displayr users who appear in Slack.",
    "", "Matt E", "Follow up with 10 Q non users in our region. Continue with Phrase express",
    "Follow up with lapsed Displayr users - 3+ uses, not used for 7 days or more, restrict to outside of US + UK.Continue with time-saving macros in Zendesk.",
    "Follow up with lapsed Displayr users - 3+ uses, not used for 7 days or more, restrict to outside of US + UK.Continue with time-saving macros in Zendesk.",
    "Monitor pending in Zendesk, makes notes for areas for improvement.",
    "Follow up with 10 Q non users in our region.", "Review 10 tickets with longest resolution time from last 30 days and categorise as to reasons.",
    "Tim", "Contact people in app when first sign up and if notice they are seen \"just now\". Follow up with lapsed Displayr users.",
    "Contact people in app when first sign up and if notice they are seen \"just now\". Follow up with lapsed Displayr users - 3+ uses, not used for 7 days or more, restrict to US.",
    "", "Contact people in app when first sign up and if notice they are seen \"just now\". Triallist research.",
    "Contact people in app when first sign up and if notice they are seen \"just now\". Triallist research.",
    "Review 10 Displayr users, active last two weeks, who have had 5 or more uses or a demo. Review 5 tickets with longest resolution time from last 30 days and categorise as to reasons.",
    "Neal", "", "Contact 10 Q non-users.", "Contact 10 Q non-users.",
    "Contact 10 Q non-users.", "", "Research + Contact 5 US Displayr users. Review 5 tickets with longest resolution time from last 30 days and categorise as to reasons.",
    "Matt S", "Monitor new Displayr users in UK daytime and approach with in-app messages",
    "Monitor new Displayr users in UK daytime and approach with in-app messages",
    "", "", "", "Record tickets with large number of replies and classify reasons. Review 5 tickets with longest resolution time from last 30 days and categorise as to reasons.",
    "Nigel", "Contact 5-10 EU / UK Displayr users", "Contact 20 EU / UK Displayr users",
    "", "", "Contact 5 EU / UK Displayr users", "Contact 5 EU / UK Displayr users. Review 5 tickets with longest resolution time from last 30 days and categorise as to reasons."
  ), .Dim = c(7L, 7L))

    out <- suppressWarnings(ParseUserEnteredTable(x))
    expect_equal(dim(out), c(6, 6))
    expect_equal(colnames(out), x[1, -1])
    expect_equal(rownames(out), x[-1, 1])
})

test_that("ParseUserEnteredTable: input is non-numeric vector",
{
    x <- matrix(c("Date times", "22/06/2007 5:29:41 PM", "22/06/2007 6:09:10 PM",
                  "22/06/2007 5:36:35 PM", "22/06/2007 5:30:29 PM", "22/06/2007 5:40:53 PM",
                  "22/06/2007 5:32:22 PM", "22/06/2007 5:39:32 PM", "22/06/2007 5:39:14 PM",
                  "22/06/2007 5:40:11 PM", "22/06/2007 5:54:34 PM"), ncol = 1)
    out <- ParseUserEnteredTable(x, want.data.frame = TRUE)
    expect_is(out, "data.frame")
    expect_equal(dim(out), dim(x) - c(1, 0))

    out <- ParseUserEnteredTable(x, want.data.frame = FALSE)
    expect_is(out, "character")
    expect_equal(length(out), 10)

    unnamed.char <- matrix(c("dog", "dog", "cat", "cat", "cat"), ncol = 1)
    out <- ParseUserEnteredTable(unnamed.char, want.data.frame = FALSE)
    expect_true(is.character(out))
    expect_equal(length(out), 5)
})


test_that("Warnings can be toggled on/off",
{
    x <- matrix(c("", "r1", "r2", "c1", "a", "b", "c1", "c", "d"), 3, 3)
    expect_warning(ParseUserEnteredTable(x, warn = TRUE), "Some variables share the same name")
    expect_silent(ParseUserEnteredTable(x, warn = FALSE))
})

test_that("ParseUserEnteredTable: named row vector",
{
    rv <- t(as.matrix(c("b", "1")))
    out <- ParseUserEnteredTable(rv)
    expect_equal(out, 1, check.attributes = FALSE)
    expect_equal(attr(out, "name"), "b")

})

test_that("ParseUserEnteredTable: named column vector; DS-1782",
{
    cv <- as.matrix(c("a", 1:3))
    out <- ParseUserEnteredTable(cv)
    expect_equal(out, 1:3, check.attributes = FALSE)
    expect_equal(attr(out, "name"), "a")
})

#test_that("Dates are parsed as rownames",
#{
#    dat <- matrix(c("%", LETTERS[1:3], "2000", 1:3, "2001", 2:4, "2002", 3:5), 4, 4)
#    res <- ParseUserEnteredTable(dat)
#    expect_equal(dim(res), c(3, 3))
#    expect_equal(colnames(res), c("2000", "2001", "2002"))
#    res <- ParseUserEnteredTable(t(dat))
#    expect_equal(rownames(res), c("2000", "2001", "2002"))
#})

test_that("Numeric names with statistic",
{
    txt <- structure(c("%", "1", "2", "3", "2012", "1", "5", "3", "2013",
        "4", "7", "5", "2014", "6", "8", "7"), .Dim = c(4L, 4L))
    res <- ParseUserEnteredTable(txt)
    expect_equal(dim(res), c(3, 3))

    raw <- structure(c("", "", "", "", "", "", "%", "Cat", "Dog", "Pigeon",
            "", "Score", "20", "30", "50"), .Dim = c(5L, 3L))

    expect_error(res <- ParseUserEnteredTable(raw), NA)
    expect_equal(res[1:3], c(0.2, 0.3, 0.5))
    expect_equal(attr(res, "statistic"), "%")

    expect_error(resDF <- ParseUserEnteredTable(raw, want.data.frame = TRUE,
                            want.col.names = TRUE, want.row.names = TRUE), NA)
    expect_equal(resDF[1:3,1], c(0.2, 0.3, 0.5))
    expect_equal(attr(resDF, "statistic"), "%")

    # check for redundancies
    raw2 <- structure(c("", "", "", "", "", "", "", "", "", "", "", "", "",
        "", "", "", "", "", "", "", "", "", "", "", "", "", "%", "south america",
        "north america", "europe", "oceania", "asia", "", "", "VAL",
        "12%", "19%", "", "-10%", "40%"), .Dim = c(8L, 5L))
    res2a <- ParseUserEnteredTable(raw2)
    expect_equal(res2a[1:5], c(0.12, 0.19, NA, -0.10, 0.40))
    res2b <- ParseUserEnteredTable(raw2, want.data.frame = TRUE)
    expect_equal(res2b[1:5,2], c(0.12, 0.19, NA, -0.10, 0.40))

})

test_that("1-d statistic",
{
    txt <- structure(c("Mean", "Variance", "Minimum", "Maximum",
        "5", "2.5", "0.02", "6.7"), .Dim = c(4L, 2L))
    res <- ParseUserEnteredTable(txt)
    expect_equal(length(res), 4)

    txt <- structure(c("Mean", "5", "Variance", "2.5", "Minimum", "0.02",
        "Maximum", "6.7"), .Dim = c(2L, 4L))
    res <- ParseUserEnteredTable(txt)
    expect_equal(length(res), 4)

    txt <- structure(c("%", "Cat", "Dog", "Pigeon", "Score", "20", "30",
        "50"), .Dim = c(4L, 2L))
    res <- ParseUserEnteredTable(txt)
    expect_equal(dim(res), c(3, 1))
})

test_that("Percentages with trailing space",
{
    txt <- structure(c("McDonalds ", "KFC ", "Pizza Hut ", "Dominos ", "Oporto ",
        "40% ", "20% ", "18% ", "15% ", "7% "), .Dim = c(5L, 2L))
    res <- ParseUserEnteredTable(txt)
    expect_equal(is.numeric(res), TRUE)
    expect_equal(length(res), 5)

    res <- ParseUserEnteredTable(txt, want.data.frame = TRUE, want.col.names = FALSE,
                                 want.row.names = TRUE)
    expect_equal(is.numeric(res[[1]]), TRUE)
    expect_equal(dim(res), c(5, 1))
})

test_that("Row and Column titles",
{
    # Has both row and column titles
    raw2 <- structure(c("", "", "animals", "", "", "", "", "cat", "dog",
                "mouse", "attributes", "age", "2", "3", "4", "", "weight", "5",
                "4", "6"), .Dim = c(5L, 4L))
    # Only column titles
    raw1 <- structure(c("", "", "", "", "", "", "", "cat", "dog", "mouse",
                "attributes", "age", "2", "3", "4", "", "weight", "5", "4", "6"
                ), .Dim = c(5L, 4L))

    expect_error(res1 <- ParseUserEnteredTable(raw1), NA)
    expect_error(res2 <- ParseUserEnteredTable(raw2), NA)
    expect_error(res2b <- ParseUserEnteredTable(raw2, want.data.frame = TRUE, want.row.names = TRUE), NA)
    expect_equal(dim(res1), c(3,2))
    expect_equal(dim(res2), c(3,2))
    expect_equal(dim(res2b), c(3,2))
    expect_equal(attr(res2, "row.column.names"), c("animals", "attributes"))
    expect_equal(attr(res1, "row.column.names"), c("", "attributes"))
})

test_that("Parsing with the 1-1 entry",
{
    # 1-1 matches a statistic, with non-numeric row and columns names
    raw <- structure(c("% Column Share", "Other", "Burger Shack", "Nuovo Burger",
        "Arnold's", "Ma's burgers", "Burger Chef", "Apr-Jun 15", "60",
        "2", "0", "21", "2", "11"), .Dim = c(7L, 2L))
    res <- ParseEnteredData(raw)
    expect_equal(res, structure(c(0.6, 0.02, 0, 0.21, 0.02, 0.11),
        .Dim = c(6L, 1L), .Dimnames = list(
        c("Other", "Burger Shack", "Nuovo Burger", "Arnold's", "Ma's burgers",
        "Burger Chef"), "Apr-Jun 15"), statistic = "% Column Share"))

    # 1-1 does not exactly match statistic, but has non-numeric row and column names
    raw <- structure(c("Random statistic", "Other", "Burger Shack", "Nuovo Burger",
        "Arnold's", "Ma's burgers", "Burger Chef", "Apr-Jun 15", "60",
        "2", "0", "21", "2", "11"), .Dim = c(7L, 2L))
    res <- ParseEnteredData(raw)
    expect_equal(res, structure(c(60, 2, 0, 21, 2, 11),
        .Dim = c(6L, 1L), .Dimnames = list(
        c("Other", "Burger Shack", "Nuovo Burger", "Arnold's", "Ma's burgers",
        "Burger Chef"), "Apr-Jun 15"), statistic = "Random statistic"))

    # Ambiguous row label + Ambiguous col label + match statistic
    raw <- structure(c("", "2011", "2013", "2009",
      "2005", "2001", "2001", "2007", "2008", "2003", "2002", "2007", "2003", "2008", "2004",
      "2003", "2008", "2003", "2007", "2003", "2005", "2002", "2007", "2003", "2007"),
      .Dim = c(5L,5L))
    res <- ParseEnteredData(raw)
    expect_equal(dim(res), c(4, 4))

    # Ambiguous row label + No col label + match statistic
    raw2 <- raw
    raw2[1,4] <- 2000.1
    res <- ParseEnteredData(raw2)
    expect_equal(dim(res), c(5, 5))

    # Ambiguous row label + col label + match statistic
    raw3 <- raw
    raw3[1,] <- c("Index", letters[1:4])
    res <- ParseEnteredData(raw3)
    expect_equal(dim(res), c(4, 4))

    # Ambiguous row label + col label + !match statistic
    raw4 <- raw
    raw4[1,] <- letters[1:5]
    res <- ParseEnteredData(raw4)
    expect_equal(dim(res), c(4, 5))

    # Ambiguous row label + Ambiguous col label + !match statistic
    # note that dim 4,5 or 4,4 is equally valid. but not 5,5
    raw5 <- raw
    raw5[1,1] <- "X"
    res <- ParseEnteredData(raw5)
    expect_equal(dim(res), c(4, 5))

    # No row label + Ambiguous col label + !match statistic
    raw6 <- raw
    raw6[1:2,1] <- c("X", "2000.5")
    res <- ParseEnteredData(raw6)
    expect_equal(dim(res), c(4, 5))
})

test_that("Character matrices are conservatively treated",
{
    raw <- structure(c("Name", "Ann", "Bob", "Charlie", "Dave", "Ed", "Fred",
        "Gary", "Henry", "Ian", "Jo", "Role", "Cook", "Server", "Cook",
        "Server", "Manager", "Manager", "Cook", "Server", "Cook", "Server",
        "Shift", "Lunch", "Lunch", "Lunch", "Lunch", "Lunch", "Dinner",
        "Dinner", "Dinner", "Dinner", "Dinner", "Salary", "1000", "1200",
        "1400", "1500", "2200", "2000", "2000", "1500", "1600", "1800",
        "Age", "19", "24", "29", "24", "32", "41", "28", "30", "22",
        "25"), .Dim = c(11L, 5L))
    res <- ParseEnteredData(raw)
    expect_equal(dim(res), c(10, 5))

    raw <- matrix(c("", "Denmark", "Egypt", "Hong Kong", "Mexico",
          "Timezone", "GMT+1", "GMT+2", "GMT+8", "GMT-6"), 5, 2)
    res <- ParseEnteredData(raw)
    expect_equal(dim(res), c(4, 1))
})


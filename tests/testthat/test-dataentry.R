context("dataentry")

test_that("numeric matrix without labels", {
    raw.matrix <- structure(c("", "", "", "", "", "", "", "", "1", "", "999", "-111",
                              "", "", "3", "2", "3.14", "", "", "", "", "6", "", "7"), .Dim = c(6L, 4L))
    expect_equal(ParseEnteredData(raw.matrix), structure(c(1, NA, 999, -111, 3, 2, 3.14, NA, NA, 6, NA, 7), .Dim = c(4L, 3L)))
})

test_that("numeric vector without labels", {
    raw.matrix <- structure(c("", "", "", "", "", "", "", "", "", "", "", "", "1",
                              "2", "3", "", "5", "6"), .Dim = c(9L, 2L))
    expect_equal(ParseEnteredData(raw.matrix), c(1, 2, 3, NA, 5, 6))
})

test_that("numeric vector with labels", {
    raw.matrix <- structure(c("one", "two", "three", "", "five", "six", "1",
                              "2", "3", "", "5", "6"), .Dim = c(6L, 2L))
    expect_equal(ParseEnteredData(raw.matrix), structure(c(1, 2, 3, NA, 5, 6), .Names = c("one", "two", "three",
                                                                                         "", "five", "six")))
})

test_that("numeric matrix with labels", {
    raw.matrix <- structure(c("", "", "", "", "", "Height", "Weight", "Strength",
                "Australia", "8", "8", "7", "USA", "7", "10", "10", "Denmark",
                "10", "4", "2"), .Dim = 4:5)
    expect_equal(ParseEnteredData(raw.matrix), structure(c(8, 8, 7, 7, 10, 10, 10, 4, 2), .Dim = c(3L, 3L), .Dimnames = list(
        c("Height", "Weight", "Strength"), c("Australia", "USA", "Denmark"))))
})

test_that("numeric matrix with labels and titles", {
    raw.matrix <- structure(c("", "", "", "", "", "Attribute", "", "", "", "",
            "", "", "", "Height", "Weight", "Strength", "", "", "", "Country",
            "Australia", "8", "8", "7", "", "", "", "", "USA", "7", "10",
            "10", "", "", "", "", "Denmark", "10", "4", "2"), .Dim = c(8L, 5L))
    expect_equal(ParseEnteredData(raw.matrix), structure(c(8, 8, 7, 7, 10, 10, 10, 4, 2), .Dim = c(3L, 3L), .Dimnames = list(
        c("Height", "Weight", "Strength"), c("Australia", "USA", "Denmark")), row.column.names = c("Attribute", "Country")))
})

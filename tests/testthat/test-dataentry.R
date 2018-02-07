context("dataentry")

test_that("numeric matrix without names", {
    raw.matrix <- structure(c("", "", "", "", "", "", "", "", "1", "", "999", "-111",
                              "", "", "3", "2", "3.14", "", "", "", "", "6", "", "7"), .Dim = c(6L, 4L))
    expect_equal(ParseEnteredData(raw.matrix),
                 structure(c(1, NA, 999, -111, 3, 2, 3.14, NA, NA, 6, NA, 7), .Dim = c(4L, 3L)))
})

test_that("numeric vector without names", {
    raw.matrix <- structure(c("", "", "", "", "", "", "", "", "", "", "", "", "1",
                              "2", "3", "", "5", "6"), .Dim = c(9L, 2L))
    expect_equal(ParseEnteredData(raw.matrix), c(1, 2, 3, NA, 5, 6))
})

test_that("numeric vector with names", {
    raw.matrix <- structure(c("one", "two", "three", "", "five", "six", "1",
                              "2", "3", "", "5", "6"), .Dim = c(6L, 2L))
    expect_equal(ParseEnteredData(raw.matrix), structure(c(1, 2, 3, NA, 5, 6),
                                                         .Names = c("one", "two", "three",
                                                                                         "", "five", "six")))
})

test_that("numeric matrix with row names", {
    raw.matrix <- structure(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "1",
                "2", "5", "2", "5", "7", "3", "3", "1", "5", "2", "2", "4", "6",
                "7", "4", "3", "2"), .Dim = c(9L, 3L))
    expect_equal(ParseEnteredData(raw.matrix),
                 structure(c(1, 2, 5, 2, 5, 7, 3, 3, 1, 5, 2, 2, 4, 6, 7, 4, 3, 2),
                           .Dim = c(9L, 2L),
                           .Dimnames = list(c("a", "b", "c", "d", "e", "f", "g", "h", "i"), NULL)))
})

test_that("numeric matrix with column names", {
    raw.matrix <- structure(c("x", "1", "2", "5", "2", "5", "7", "3", "3", "1",
                              "y", "5", "2", "2", "4", "6", "7", "4", "3", "2"), .Dim = c(10L, 2L))
    expect_equal(ParseEnteredData(raw.matrix),
                 structure(c(1, 2, 5, 2, 5, 7, 3, 3, 1, 5, 2, 2, 4, 6, 7, 4, 3,
                             2), .Dim = c(9L, 2L), .Dimnames = list(NULL, c("x", "y"))))
})

test_that("numeric matrix with names", {
    raw.matrix <- structure(c("", "", "", "", "", "Height", "Weight", "Strength",
                "Australia", "8", "8", "7", "USA", "7", "10", "10", "Denmark",
                "10", "4", "2"), .Dim = 4:5)
    expect_equal(ParseEnteredData(raw.matrix), structure(c(8, 8, 7, 7, 10, 10, 10, 4, 2), .Dim = c(3L, 3L),
                                                         .Dimnames = list(c("Height", "Weight", "Strength"),
                                                                          c("Australia", "USA", "Denmark"))))
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
    expect_equal(ParseEnteredData(raw.matrix),
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
                           .Dimnames = list(c("Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max", "None of these",
                                              "NET"), c("Feminine", "Health-conscious", "Innocent", "Older", "Open to new experiences",
                                                        "Rebellious", "Sleepy", "Traditional", "Weight-conscious", "NET")),
                           row.column.names = c("Product", "Attribute")))
})

test_that("numeric matrix with percentages", {

    datW1 <- structure(c("%", "Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi",
        "Pepsi Max", "None of these", "NET", "Feminine", "6.13", "57.13",
        "22.38", "8.88", "61.5", "9.38", "9.25", "100", "Health-conscious",
        "2", "57.75", "53.5", "2.5", "57.88", "30.63", "17.38", "100",
        "Innocent", "10.5", "21.63", "11.38", "10", "44.63", "6.88",
        "29.88", "100", "Older", "64.63", "22.5", "5.38", "39", "9.88",
        "6.75", "7.25", "100", "Open to new experiences", "22.38", "8.88",
        "50.63", "16.75", "16.63", "49.25", "12.88", "100", "Rebellious",
        "25.5", "4.75", "64", "17.75", "3.75", "44.75", "15.25", "100",
        "Sleepy", "9.5", "23.25", "9.75", "13.5", "29.75", "5.5", "38.88",
        "100", "Traditional", "91.25", "14.63", "3", "54.75", "3.75",
        "4.38", "2.5", "100", "Weight-conscious", "0.5", "76.13", "63.88",
        "0", "76.63", "40.38", "5.75", "100", "NET", "98", "91.5", "94.88",
        "79.63", "94.75", "86.38", "57.5", "100"), .Dim = c(9L, 11L))

    datW2 <- structure(c("", "", "", "", "", "", "", "", "", "", "", "", "",
        "", "Cleanliness", "Health", "Safety", "Cost", "Food", "Not being understood",
        "Friendliness of the people", "Boredom", "", "", "Mexico", "52%",
        "51%", "79%", "9%", "23%", "17%", "11%", "3%", "", "", "France",
        "9%", "7%", "15%", "60%", "7%", "43%", "40%", "5%", "", "", "Great Britain",
        "6%", "6%", "10%", "51%", "11%", "5%", "8%", "5%", "", "", "Egypt",
        "36%", "39%", "78%", "44%", "28%", "48%", "34%", "4%", "", "",
        "Australia", "4%", "5%", "11%", "57%", "6%", "3%", "6%", "1%",
        "", "", "China", "37%", "45%", "46%", "52%", "30%", "60%", "25%",
        "5%"), .Dim = c(11L, 8L))

    datW3 <- structure(c("", "", "", "", "", "", "", "", "", "", "", "", "",
        "Total %", "Cleanliness", "Health", "Safety", "Cost", "Food", "Not being understood",
        "Friendliness of the people", "Boredom", "", "", "Mexico", "52%",
        "51%", "79%", "9%", "23%", "17%", "11%", "3%", "", "", "France",
        "9%", "7%", "15%", "60%", "7%", "43%", "40%", "5%", "", "", "Great Britain",
        "6%", "6%", "10%", "51%", "11%", "5%", "8%", "5%", "", "", "Egypt",
        "36%", "39%", "78%", "44%", "28%", "48%", "34%", "4%", "", "",
        "Australia", "4%", "5%", "11%", "57%", "6%", "3%", "6%", "1%",
        "", "", "China", "37%", "45%", "46%", "52%", "30%", "60%", "25%",
        "5%"), .Dim = c(11L, 8L))

    expect_error(res1 <- ParseEnteredData(datW1), NA)
    expect_error(res2 <- ParseEnteredData(datW2), NA)
    expect_error(res3 <- ParseEnteredData(datW3), NA)
    expect_equal(attr(res1, "statistic"), "%")
    expect_equal(res2[1,1], 0.52)
    expect_equal(res3[1,1], 0.52)
})

test_that("numeric matrix with commas", {
    raw.matrix <- structure(c("", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "A", "B", "C", "", "", "", "1,000", "2,000", "3,000"), .Dim = c(6L,
4L))
    expect_error(out <- ParseEnteredData(raw.matrix), NA)
    expect_equal(sum(out), 6000)
})


test_that("data frame", {
    raw.matrix <- structure(c("num", "1", "2", "", "4", "2", "", "5", "", "23",
                              "", "2", "3.14", "5", "", "6", "date", "1-Feb-16", "2-Feb-16",
                              "3-Feb-16", "4-Feb-16", "5-Feb-16", "6-Feb-16", "7-Feb-16", "8-Feb-16",
                              "9-Feb-16", "10-Feb-16", "11-Feb-16", "12-Feb-16", "13-Feb-16",
                              "14-Feb-16", "15-Feb-16", "char", "a", "b", "b", "a", "f", "f",
                              "c", "f", "c", "c", "c", "f", "f", "f", "c"), .Dim = c(16L, 3L))
    expect_warning(out <- ParseEnteredData(raw.matrix, want.data.frame = TRUE),
                   "^Supplied date formats are ambiguous")
    expect_equal(out,
                 structure(list(num = c(1, 2, NA, 4, 2, NA, 5, NA, 23, NA, 2, 3.14, 5, NA, 6),
                                date = structure(c(1454284800, 1454371200, 1454457600,
                                                   1454544000, 1454630400, 1454716800, 1454803200, 1454889600, 1454976000,
                                                   1455062400, 1455148800, 1455235200, 1455321600, 1455408000, 1455494400),
                                                 class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                char = structure(c(1L, 2L, 2L, 1L, 4L, 4L, 3L, 4L, 3L, 3L, 3L, 4L, 4L, 4L, 3L),
                                                 .Label = c("a","b", "c", "f"), class = "factor")),
                           .Names = c("num", "date", "char"), row.names = c(NA, -15L), class = "data.frame"))
})

test_that("data frame no factors", {
    raw.matrix <- structure(c("num", "1", "2", "", "4", "2", "", "5", "", "23",
                              "", "2", "3.14", "5", "", "6", "date", "1-Feb-2016", "2-Feb-2016",
                              "3-Feb-2016", "4-Feb-2016", "5-Feb-2016", "6-Feb-2016", "7-Feb-2016", "8-Feb-2016",
                              "9-Feb-2016", "10-Feb-2016", "11-Feb-2016", "12-Feb-2016", "13-Feb-2016",
                              "14-Feb-2016", "15-Feb-2016", "char", "a", "b", "b", "a", "f", "f",
                              "c", "f", "c", "c", "c", "f", "f", "f", "c"), .Dim = c(16L, 3L))
    expect_equal(ParseEnteredData(raw.matrix, want.data.frame = TRUE, want.factors = FALSE),
                 structure(list(num = c(1, 2, NA, 4, 2, NA, 5, NA, 23, NA, 2, 3.14, 5, NA, 6),
                                date = structure(c(1454284800, 1454371200, 1454457600,
                                                1454544000, 1454630400, 1454716800, 1454803200, 1454889600, 1454976000,
                                                1455062400, 1455148800, 1455235200, 1455321600, 1455408000, 1455494400),
                                                class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                char = c("a", "b", "b", "a", "f", "f", "c", "f", "c", "c", "c", "f", "f", "f", "c")),
                           .Names = c("num", "date", "char"), row.names = c(NA, -15L), class = "data.frame"))
})

test_that("data frame row names", {
    raw.matrix <- structure(c("", "row 1", "row 2", "row 3", "row 4", "row 5",
                              "row 6", "row 7", "row 8", "row 9", "row 10", "row 11", "row 12",
                              "row 13", "row 14", "row 15", "num", "1", "2", "", "4", "2",
                              "", "5", "", "23", "", "2", "3.14", "5", "", "6", "date", "1-Feb-2016",
                              "2-Feb-2016", "3-Feb-2016", "4-Feb-2016", "5-Feb-2016", "6-Feb-2016", "7-Feb-2016",
                              "8-Feb-2016", "9-Feb-2016", "10-Feb-2016", "11-Feb-2016", "12-Feb-2016",
                              "13-Feb-2016", "14-Feb-2016", "15-Feb-2016", "char", "a", "b", "b",
                              "a", "f", "f", "c", "f", "c", "c", "c", "f", "f", "f", "c"), .Dim = c(16L, 4L))
    expect_equal(ParseEnteredData(raw.matrix, want.data.frame = TRUE, want.row.names = TRUE),
                 structure(list(num = c(1, 2, NA, 4, 2, NA, 5, NA, 23, NA, 2, 3.14, 5, NA, 6),
                                date = structure(c(1454284800, 1454371200, 1454457600,
                                                1454544000, 1454630400, 1454716800, 1454803200, 1454889600, 1454976000,
                                                1455062400, 1455148800, 1455235200, 1455321600, 1455408000, 1455494400),
                                                class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                char = structure(c(1L, 2L, 2L, 1L, 4L, 4L, 3L, 4L, 3L, 3L, 3L, 4L, 4L, 4L, 3L),
                                                 .Label = c("a", "b", "c", "f"), class = "factor")),
                           .Names = c("num", "date", "char"),
                           row.names = c("row 1", "row 2", "row 3", "row 4", "row 5", "row 6", "row 7", "row 8",
                                         "row 9", "row 10", "row 11", "row 12", "row 13", "row 14", "row 15"),
                           class = "data.frame"))
})

test_that("data frame no names", {
    raw.matrix <- structure(c("1", "2", "", "4", "2", "", "5", "", "23", "", "2",
                "3.14", "5", "", "6", "1-Feb-16", "2-Feb-16", "3-Feb-16", "4-Feb-16",
                "5-Feb-16", "6-Feb-16", "7-Feb-16", "8-Feb-16", "9-Feb-16", "10-Feb-16",
                "11-Feb-16", "12-Feb-16", "13-Feb-16", "14-Feb-16", "15-Feb-16",
                "a", "b", "b", "a", "f", "f", "c", "f", "c", "c", "c", "f", "f",
                "f", "c"), .Dim = c(15L, 3L))
    expect_warning(out <- ParseEnteredData(raw.matrix, want.data.frame = TRUE,
                                           want.col.names = FALSE),
                   "^Supplied date formats are ambiguous")
    expect_equal(out,
                 structure(list(X1 = c(1, 2, NA, 4, 2, NA, 5, NA, 23, NA, 2, 3.14, 5, NA, 6),
                                X2 = structure(c(1454284800, 1454371200, 1454457600,
                                               1454544000, 1454630400, 1454716800, 1454803200, 1454889600, 1454976000,
                                               1455062400, 1455148800, 1455235200, 1455321600, 1455408000, 1455494400
                                       ), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                X3 = structure(c(1L, 2L, 2L, 1L, 4L, 4L, 3L, 4L, 3L, 3L, 3L, 4L, 4L, 4L, 3L),
                                               .Label = c("a", "b", "c", "f"), class = "factor")),
                           .Names = c("X1", "X2", "X3"), row.names = c(NA, -15L), class = "data.frame"))
})

test_that("data frame blank names", {
    raw.matrix <- structure(c("V1", "2", "", "4", "2", "", "5", "", "23", "", "2",
                              "3.14", "5", "", "6", "", "2-Feb-16", "3-Feb-16", "4-Feb-16",
                              "5-Feb-16", "6-Feb-16", "7-Feb-16", "8-Feb-16", "9-Feb-16", "10-Feb-16",
                              "11-Feb-16", "12-Feb-16", "13-Feb-16", "14-Feb-16", "15-Feb-16",
                              "V3", "b", "b", "a", "f", "f", "c", "f", "c", "c", "c", "f",
                              "f", "f", "c"), .Dim = c(15L, 3L))
    expect_warning(ParseEnteredData(raw.matrix, want.data.frame = TRUE),
                   "Some variables have been assigned blank names.")
})

test_that("data frame duplicate names", {
    raw.matrix <- structure(c("V1", "2", "", "4", "2", "", "5", "", "23", "", "2",
                              "3.14", "5", "", "6", "V1", "2-Feb-16", "3-Feb-16", "4-Feb-16",
                              "5-Feb-16", "6-Feb-16", "7-Feb-16", "8-Feb-16", "9-Feb-16", "10-Feb-16",
                              "11-Feb-16", "12-Feb-16", "13-Feb-16", "14-Feb-16", "15-Feb-16",
                              "V3", "b", "b", "a", "f", "f", "c", "f", "c", "c", "c", "f",
                              "f", "f", "c"), .Dim = c(15L, 3L))
    expect_warning(ParseEnteredData(raw.matrix, want.data.frame = TRUE),
                   "Some variables share the same name.")
})

test_that("ParseAsDataFrame", {
    raw.matrix <- structure(list(X__1 = c("2017-05-31", "2017-06-01", "2017-06-02",
                        "2017-06-03", "2017-06-04", "2017-06-05", "2017-06-06", "2017-06-07",
                        "2017-06-08", "2017-06-09"), `Pre-breakfast` = c(NA, 11.4, 10.8,
                        10.6, 8.7, 9.7, 7, 6.1, 4.6, 4.1), `Post-breakfast` = c(NA, 17.1,
                        12.6, 8.6, 11.4, 10.6, 11.4, 8.9, 8.6, NA), `Pre-lunch` = c(NA,
                        NA, 8.9, 5.6, 13.4, 9.2, 5.7, 5.2, NA, NA), `Post-lunch` = c(22.9,
                        13.9, 9.8, 12.2, 6.6, 8.4, 9.1, 5.3, 8.4, NA), `Pre-dinner` = c(6.55,
                        12.9, 10.3, 13.1, 8.9, 8.8, 7.3, 5.7, 7.7, NA), `Post-dinner` = c(13.8,
                        14.8, 10.3, 12.3, 14.5, 8.4, 6.8, 6.5, 7.5, NA)), .Names = c("X__1",
                        "Pre-breakfast", "Post-breakfast", "Pre-lunch", "Post-lunch",
                        "Pre-dinner", "Post-dinner"), class = c("tbl_df", "tbl", "data.frame"
                        ), row.names = c(NA, -10L))

    expect_warning(res1 <- ParseAsDataFrame(raw.matrix),
                   "Some variables have been assigned blank names")
    expect_equal(nrow(res1), 9)
    res2 <- ParseAsDataFrame(raw.matrix, want.col.names = FALSE)
    expect_equal(nrow(res2), 10)
    expect_equal(colnames(res2), c("X__1","Pre.breakfast", "Post.breakfast",
                                   "Pre.lunch", "Post.lunch", "Pre.dinner", "Post.dinner"))
    expect_equal("POSIXct" %in% class(res2[,1]), TRUE)
    m2 <- as.matrix(res2)
    expect_equal(is.numeric(m2), FALSE)
    res3 <- ParseAsDataFrame(raw.matrix, want.col.names = FALSE, want.row.names = TRUE)
    expect_equal(ncol(res3), 6)
    expect_equal(rownames(res3)[1], "2017-05-31")
    m3 <- as.matrix(res3)
    expect_equal(is.numeric(m3), TRUE)
})

test_that("ParseAsDataFrame assigns colnames if none provided; DS-1779",
{
    m <- cbind(letters[1:3], 1:3, 2:4)
    out <- ParseAsDataFrame(m, want.row.names = TRUE, want.col.names = FALSE)
    expect_equal(colnames(out), paste0("X", 1:(ncol(m) - 1L)))
})

test_that("ParseAsDataFrame adds statistic attr. if input has %s; DS-1780",
{
    m <- rbind(letters[1:3], paste0(1:3, "%"))
    out <- ParseAsDataFrame(m)
    expect_equal(attr(out, "statistic"), "%")
})

test_that("TextAsVector", {
    res1 <- TextAsVector("What,     is, this")
    res2 <- TextAsVector(c("'What'", "'is'", "'this'"))
    res3 <- TextAsVector("What's, this")
    expect_equal(res1, res2)
    expect_equal(grep("\'", res3), 1)

    if ("UTF-8" %in% localeToCharset())
    {
        s4 <- '“What”  ,is,this'
        res4 <- suppressWarnings(TextAsVector(s4))
        expect_equal(res1, res4)
        s5 <- '\u201CWhat\u201D,is,this'
        res5 <- suppressWarnings(TextAsVector(s5))
        expect_equal(res1, res5)
    }
    else
    {
        s4 <- '\x93What\x94,is,this'
        res4 <- suppressWarnings(TextAsVector(s4))
        expect_equal(res1, res4)
    }
})

test_that("ParseAsDataFrame, one row of data",
{
    x <- matrix(as.character(1:10), nrow = 1)
    expect_error(ParseAsDataFrame(x), "There is no data to display")
    out <- ParseAsDataFrame(x, want.row.name = FALSE,
                                 want.col.names = FALSE)
    expect_is(out, "data.frame")
    expect_equal(dim(out), dim(x))
    expect_is(out[[1]], "numeric")

    x <- matrix(c("Date times", "22/06/2007 5:29:41 PM", "22/06/2007 6:09:10 PM",
                  "22/06/2007 5:36:35 PM", "22/06/2007 5:30:29 PM", "22/06/2007 5:40:53 PM",
                  "22/06/2007 5:32:22 PM", "22/06/2007 5:39:32 PM", "22/06/2007 5:39:14 PM",
                  "22/06/2007 5:40:11 PM", "22/06/2007 5:54:34 PM"), nrow = 1)
    expect_error(ParseAsDataFrame(x), "There is no data to display")
    out <- ParseAsDataFrame(x, want.row.name = TRUE,
                                 want.col.names = FALSE)
    expect_is(out, "data.frame")
    expect_equal(dim(out), dim(x) - c(0, 1))
    expect_is(out[[1]], "POSIXct")

    cnx <- rbind(c("", LETTERS[1:10]), x)
    out <- ParseAsDataFrame(cnx, want.row.name = TRUE,
                                 want.col.names = TRUE)
    expect_is(out, "data.frame")
    expect_equal(dim(out), dim(cnx) - c(1, 1))
    expect_is(out[[1]], "POSIXct")
    expect_equal(colnames(out), LETTERS[1:10])
})

test_that("ParseAsDataFrame, statistic in 1,1 entry DS-1780 CSC comment",
{
    x <- rbind(c("column %", "score"),
               cbind(letters[1:3], 1:3))
    out <- ParseAsDataFrame(x, want.row.name = TRUE,
                            want.col.names = TRUE)
    expect_is(out, "data.frame")
    expect_equal(attr(out, "statistic"), x[1, 1])
    expect_equal(dim(out), dim(x) - c(1, 1))
    expect_equal(rownames(out), x[-1, 1])
    expect_equal(colnames(out), x[1, 2])
})


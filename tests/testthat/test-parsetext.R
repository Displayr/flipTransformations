context("ParseText")

test_that("ParseText",
{
    expect_equal(ParseText("1,200,111"), 1200111)
    expect_equal(ParseText("(1,200,111)"), -1200111)
    expect_equal(ParseText("45%"), structure(45, statistic = "%"))
    expect_equal(ParseText("45%", same.as = ParseText("0.15")), 0.45)
    expect_equal(ParseText("0.45", same.as = ParseText("15%")), structure(45, statistic = "%"))
    expect_equal(ParseText("-45%"), structure(-45, statistic = "%"))
    expect_equal(ParseText("(45%)"), structure(-45, statistic = "%"))
    expect_equal(ParseText("0.07"), 0.07)
    expect_equal(ParseText("0..07"), "0..07")
    expect_equal(ParseText("0..07", type = "Numeric"), NA_real_)
    expect_equal(ParseText("2018-01-25"), structure(1516838400, class = c("POSIXct", "POSIXt"), tzone = "UTC"))
    expect_equal(ParseText("2018-01-25", Sys.time()), structure(1516838400, class = c("POSIXct", "POSIXt"), tzone = "UTC"))
    expect_equal(ParseText("2018-01-25", Sys.Date()), structure(17556, class = "Date"))

})

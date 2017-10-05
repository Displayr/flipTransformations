
context("table")
test_that("RemoveRowsAndOrColumns works",
          {
              x <- matrix(NA, 3, 3, dimnames = list(LETTERS[1:3],LETTERS[1:3]))
              x <- RemoveRowsAndOrColumns(x, "A", c("C","A"))
              expect_equal(prod(dim(x)), 2)
          })

dat <- structure(c(39.9370629370629, 45.9459459459459, 43.1311475409836,
54.2222222222222, 43.7954545454545, 42, 40.2748538011696, 44.6754966887417,
42.3385093167702), .Dim = c(9L, 1L), .Dimnames = list(c("Coca Cola ",
"Diet Coke", "Coke Zero", "Pepsi Light ", "Pepsi Max", "Pepsi ",
"NET Sugarred", "NET Sugarless", "NET"), "Age in years"), statistic = "Average", name = "Q3. Age in years by Preferred cola", questions = c("Q3. Age in years",
"Preferred cola"))

test_that("RemoveRowsAndOrColumns handles NULL",
          {
              d2 <- RemoveRowsAndOrColumns(dat, row.names.to.remove = NULL)
              expect_equal(nrow(d2), 9)
          })

test_that("RemoveRowsAndOrColumns handles spaces in middle",
          {
              d3 <- RemoveRowsAndOrColumns(dat, row.names.to.remove = "NET Sugarless,NET Sugarred,   Net")
              expect_equal(nrow(d3), 6)
          })

test_that("RemoveRowsAndOrColumns handles trailing spaces",
          {
              d4 <- RemoveRowsAndOrColumns(dat, row.names.to.remove = "Pepsi")
              expect_equal(nrow(d4), 8)
          })

test_that("RemoveRowAndOrColumns ignores lower/upper case",
          {
              d5 <- RemoveRowsAndOrColumns(dat, row.names.to.remove = "diet coke, coke zero")
              expect_equal(nrow(d5), 7)
          })

test_that("RemoveRowsAndOrColumns perserves attriubtes",
{
    out <- RemoveRowsAndOrColumns(dat, row.names.to.remove = "diet coke, coke zero")
    expect_equal(attr(out, "name"), attr(dat, "name"))
    expect_equal(attr(out, "questions"), attr(dat, "questions"))
    expect_equal(attr(out, "statistic"), attr(dat, "statistic"))
})



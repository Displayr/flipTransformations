
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

        dat <- structure(list(Q6_A = structure(c(3L, 5L, 5L, 6L, 4L, 1L, 3L,
    6L, 5L, 6L, 6L, 5L, 5L, 4L, 3L, 6L, 6L, 5L, 5L, 4L), .Label = c("Don t Know",
    "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
    ), class = "factor", label = structure("Q6. Coca Cola", .Names = "Q6_A")),
        Q6_B = structure(c(5L, 2L, 6L, 3L, 6L, 1L, 4L, 3L, 5L, 6L,
        2L, 3L, 3L, 3L, 6L, 5L, 5L, 3L, 3L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Diet Coke", .Names = "Q6_B")),
        Q6_C = structure(c(3L, 5L, 3L, 3L, 4L, 1L, 5L, 5L, 1L, 6L,
        2L, 3L, 3L, 5L, 3L, 5L, 5L, 3L, 5L, 6L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Coke Zero", .Names = "Q6_C")),
        Q6_D = structure(c(4L, 5L, 4L, 3L, 4L, 1L, 3L, 4L, 5L, 5L,
        6L, 5L, 4L, 4L, 5L, 5L, 3L, 5L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi", .Names = "Q6_D")),
        Q6_E = structure(c(2L, 4L, 2L, 3L, 6L, 6L, 3L, 3L, 5L, 5L,
        2L, 3L, 3L, 4L, 6L, 1L, 2L, 3L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi Light", .Names = "Q6_E")),
        Q6_F = structure(c(6L, 6L, 2L, 3L, 3L, 6L, 3L, 5L, 4L, 4L,
        2L, 3L, 3L, 5L, 3L, 1L, 2L, 3L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi Max", .Names = "Q6_F"))), .Names = c("Q6_A",
    "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F"), row.names = c(NA, 20L
                                                           ), class = "data.frame")
    attr(dat, "statistic") <- "means"

test_that("RemoveRowsAndOrColumns keeps data.frame col. attrs",
{
    out <- RemoveRowsAndOrColumns(dat, column.names.to.remove = "Q6_A", row.names.to.remove = "3")
    expect_equal(flipFormat::Labels(out), flipFormat::Labels(dat)[-1])
    expect_equal(dim(out), dim(dat) - c(1, 1))
    expect_equal(attr(out, "statistic"), attr(dat, "statistic"))
})

test_that("RemoveRowsAndOrColumns keeps data.frame col. attrs, only one col remains",
{
    out <- RemoveRowsAndOrColumns(dat, column.names.to.remove = c("Q6_B", "Q6_F", "Q6_C", "Q6_D", "Q6_A"))
    expect_equal(flipFormat::Labels(out), flipFormat::Labels(dat)[5])
    expect_equal(dim(out), c(nrow(dat), 1L))
    expect_equal(attr(out, "statistic"), attr(dat, "statistic"))
})

test_that("RemoveRowsAndOrColumns data.frame no col. attrs",
{
    dat <- data.frame(x = 1:5, y = 1:5, z = as.factor(c(1,2,1,2,0)))
    attr(dat, "statistic") <- "SUM"
    out <- RemoveRowsAndOrColumns(dat, column.names.to.remove = "x", row.names.to.remove = "2")
    expect_equal(dim(out), dim(dat) - c(1, 1))
    expect_equal(colnames(out), colnames(dat)[-1])
    expect_equal(attr(out, "statistic"), attr(dat, "statistic"))

    out <- RemoveRowsAndOrColumns(dat, column.names.to.remove = c("x", "z"))
    expect_equal(dim(out), dim(dat) - c(0, 2))
    expect_is(out, "data.frame")
})




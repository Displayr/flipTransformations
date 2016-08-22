context("Linear regression")
data(bank, package = "flipExampleData")

test_that("Dichotomizing works",
{
    expect_true(all(table(DichotomizeFactor(factor(LETTERS[1:10]))) == c(5,5)))
    expect_true(all(table(DichotomizeFactor(bank$Overall)) == c(265, 497)))
    attr(bank$Fees, "label") <- "Bank Fees"
    expect_equal(attr(DichotomizeFactor(bank$Fees), "label"), "Bank Fees >= 3")
    attr(bank$Fees, "label") <- "Bank Fees"
    expect_equal(attr(DichotomizeFactor(bank$Overall), "label"), "bank$Overall >= 4")
})


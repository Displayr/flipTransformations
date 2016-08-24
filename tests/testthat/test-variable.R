context("Variables")
data(bank, package = "flipExampleData")

test_that("Dichotomizing works",
{
    expect_true(all(table(DichotomizeFactor(factor(LETTERS[1:10]))) == c(5,5)))
    expect_true(all(table(DichotomizeFactor(bank$Overall)) == c(265, 497)))
    attr(bank$Fees, "label") <- "Bank Fees"
    expect_equal(attr(DichotomizeFactor(bank$Fees), "label"), "Bank Fees >= 3")
    expect_equal(attr(DichotomizeFactor(bank$Overall), "label"), "bank$Overall >= 4")
    expect_equal(attr(suppressWarnings(CreatingBinaryVariableIfNecessary(bank, "Fees")), "label"), "Bank Fees >= 3")
    z <- suppressWarnings(CreatingBinaryDependentVariableIfNecessary(Fees ~ Overall, bank))$Fees
    expect_equal(attr(z, "label"), "Bank Fees >= 3")

})


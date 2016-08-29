context("AsNumeric")
data(bank, package = "flipExampleData")

X <- data.frame("a" = LETTERS[1:20],
                b = 1:20,
                c = runif(20),
                d = factor(LETTERS[1:4]),
                e = ordered(LETTERS[1:4]),
                stringsAsFactors = FALSE)

test_that("AsNumeric",
          {

              Q2 <- factor(bank$Overall)
              Q3 <- factor(bank$Fees)
              Q4 <- factor(bank$Fees)
              head(AsNumeric(data.frame(Q2, Q3, Q4),  binary = TRUE, remove.first = TRUE))
              head(AsNumeric(data.frame(Q2, Q3, Q4),  binary = TRUE, remove.first = FALSE))
              head(AsNumeric(data.frame(Q2, Q3, Q4),  binary = FALSE, remove.first = FALSE))


              AsNumeric(X$a)
              AsNumeric(X$b)
              AsNumeric(X$c)
              expect_equal(ncol(AsNumeric(X$d, binary = TRUE, remove.first = TRUE)) + 1,
                           ncol(AsNumeric(X$d, binary = TRUE, remove.first = FALSE)))
              AsNumeric(X$d, binary = TRUE)
              AsNumeric(X$d, binary = FALSE)
              AsNumeric(X$e, binary = FALSE)
              AsNumeric(X$e, binary = TRUE)

              AsNumeric(X, binary = FALSE)
              AsNumeric(X, binary = TRUE)
              # expect_true(all(table(DichotomizeFactor(factor(LETTERS[1:10]))) == c(5,5)))
              # expect_true(all(table(DichotomizeFactor(bank$Overall)) == c(265, 497)))
              # attr(bank$Fees, "label") <- "Bank Fees"
              # expect_equal(attr(DichotomizeFactor(bank$Fees), "label"), "Bank Fees >= 3")
              # expect_equal(attr(DichotomizeFactor(bank$Overall), "label"), "bank$Overall >= 4")
              # expect_equal(attr(suppressWarnings(CreatingBinaryVariableIfNecessary(bank, "Fees")), "label"), "Bank Fees >= 3")
              # z <- suppressWarnings(CreatingBinaryDependentVariableIfNecessary(Fees ~ Overall, bank))$Fees
              # expect_equal(attr(z, "label"), "Bank Fees >= 3")

          })


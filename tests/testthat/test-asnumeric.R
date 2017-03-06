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
              flipU::ExpectWarning(AsNumeric(data.frame(Q2, Q3, Q4),  binary = FALSE, remove.first = TRUE), "structure")
              head(suppressWarnings(AsNumeric(data.frame(Q2, Q3, Q4),  binary = TRUE, remove.first = TRUE)))
              head(suppressWarnings(AsNumeric(data.frame(Q2, Q3, Q4),  binary = TRUE, remove.first = FALSE)))
              head(suppressWarnings(AsNumeric(data.frame(Q2, Q3, Q4),  binary = FALSE, remove.first = FALSE)))


              expect_error(suppressWarnings(AsNumeric(X$a)), NA)
              expect_error(suppressWarnings(AsNumeric(X$b)), NA)
              expect_error(suppressWarnings(AsNumeric(X$c)), NA)
              expect_equal(ncol(suppressWarnings(AsNumeric(X$d, binary = TRUE, remove.first = TRUE))) + 1,
                           ncol(suppressWarnings(AsNumeric(X$d, binary = TRUE, remove.first = FALSE))))
              expect_error(AsNumeric(X$d, binary = TRUE), NA)
              expect_error(suppressWarnings(AsNumeric(X$d, binary = FALSE)), NA)
              expect_error(suppressWarnings(AsNumeric(X$e, binary = FALSE)), NA)
              expect_error(suppressWarnings(AsNumeric(X$e, binary = TRUE)), NA)

              expect_error(suppressWarnings(AsNumeric(X, binary = FALSE)), NA)
              expect_error(suppressWarnings(AsNumeric(X, binary = TRUE)), NA)
              # Checking that names are not changed.
              xz = data.frame(Q2, Q3, Q4)
              rownames(xz)[2] = "dog"
              xz1 = suppressWarnings(AsNumeric(xz,  binary = FALSE, remove.first = FALSE))
              expect_equal(rownames(xz)[2], rownames(xz1)[2])

          })


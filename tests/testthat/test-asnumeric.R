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
              # Data frame
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

              # Dates
              dd <- seq(ISOdate(2000,1,1,0), by="day", length.out=10)
              ds <- sprintf("2000-01-%02d", 1:10)
              dm <- c(ds[1:5], "dog", "other random thing")
              expect_equal(range(AsNumeric(dd, binary=F)), c(946684800, 947462400))
              expect_equal(range(AsNumeric(ds, binary=F)), c(946684800, 947462400))
              expect_equal(suppressWarnings(AsNumeric(dm, binary=F)), 1:7)

              # list - checking for variable names
              expect_error(suppressWarnings(AsNumeric(list(A = Q2, B = Q3, C = Q4),  binary = FALSE, remove.first = TRUE)), NA)
              expect_error(suppressWarnings(AsNumeric(list(Q2, Q3, Q4),  binary = TRUE, remove.first = TRUE)), NA)
              expect_error(suppressWarnings(AsNumeric(list(Q2, Q3, Q4),  binary = FALSE, remove.first = TRUE)), NA)

              # character vector - check order
              cc <- sprintf("Row %d", c(1:100, 1:3))
              expect_equal(suppressWarnings(AsNumeric(cc, binary = FALSE)), c(1:100, 1:3))
          })

df <- data.frame(a = 1:3, b = c("x", "y", "z"), c = 99:101)

test_that("OneHot",
          {
              expect_error(OneHot(df), NA)
              expect_error(OneHot(df, "NotPresent"), NA)
              expect_equal(ncol(OneHot(df, "a")$X), 4)
              expect_equal(ncol(OneHot(df, "b")$X), 2)
              expect_equal(ncol(OneHot(df, "c")$X), 4)
})

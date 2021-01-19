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
              df <- data.frame(Q2, Q3, Q4)
              expect_true(all(vapply(df, is.factor, logical(1))))
              warn.msg <- paste0("Data has been automatically converted to numeric. Values are ",
                                 "assigned in the order of the categories: 1, 2, 3, ...; To use ",
                                 "alternative numeric values, transform the data prior including ",
                                 "it in this analysis (e.g. by changing its structure). The ",
                                 "variables Q2, Q3 and Q4 have been converted.")
              expect_warning(output <- AsNumeric(data.frame(Q2, Q3, Q4), binary = FALSE, remove.first = TRUE),
                             warn.msg, fixed = TRUE)
              not.factor <- Negate(is.factor)
              expect_true(all(vapply(output, not.factor, logical(1))))
              expect_true(all(vapply(output, is.integer, logical(1))))
              possible.vals <- unique(unlist(lapply(output, as.numeric)))
              expect_setequal(unique(unlist(output)), possible.vals)
              expect_error(output <- AsNumeric(data.frame(Q2, Q3, Q4), binary = TRUE, remove.first = TRUE),
                           NA)
              not.factor <- Negate(is.factor)
              expect_true(all(vapply(output, not.factor, logical(1))))
              checkNumericTransformation <- function(vect, vect.nam, unique.vals) {
                  is.all.numeric <- is.numeric(as.vector(vect))
                  unique.ok <- setequal(unique(vect), unique.vals)
                  attr.ok <- attr(vect, "label") == sub(".", ": ", vect.nam, fixed = TRUE)
                  is.all.numeric && unique.ok && attr.ok
              }
              expect_true(all(mapply(checkNumericTransformation,
                                     output, names(output), MoreArgs = list(unique.vals <- c(NA, 0, 1)))))
              expect_error(output <- AsNumeric(data.frame(Q2, Q3, Q4), binary = TRUE, remove.first = FALSE),
                           NA)
              expect_true(all(mapply(checkNumericTransformation,
                                     output, names(output), MoreArgs = list(unique.vals <- c(NA, 0, 1)))))
              expect_warning(output <- AsNumeric(data.frame(Q2, Q3, Q4), binary = FALSE, remove.first = FALSE),
                             warn.msg, fixed = TRUE)
              expect_true(all(vapply(output, not.factor, logical(1))))
              expect_true(all(vapply(output, is.integer, logical(1))))
              possible.vals <- unique(unlist(lapply(output, as.numeric)))
              expect_setequal(unique(unlist(output)), c(NA, 1:7))
              expect_error(AsNumeric(X$a), NA)
              expect_error(AsNumeric(X$b), NA)
              expect_error(AsNumeric(X$c), NA)
              expect_equal(ncol(AsNumeric(X$d, binary = TRUE, remove.first = TRUE)) + 1,
                           ncol(AsNumeric(X$d, binary = TRUE, remove.first = FALSE)))
              expect_error(AsNumeric(X$d, binary = TRUE), NA)
              warn.msg.without.names <- sub(" The variables Q2, Q3 and Q4 have been converted.",
                                            "",
                                            warn.msg)
              expect_warning(AsNumeric(X$d, binary = FALSE), warn.msg.without.names, fixed = TRUE)
              expect_warning(AsNumeric(X$e, binary = FALSE), warn.msg.without.names, fixed = TRUE)
              expect_warning(AsNumeric(X$e, binary = TRUE), warn.msg.without.names, fixed = TRUE)

              expect_warning(AsNumeric(X, binary = FALSE),
                             paste0(warn.msg.without.names, " The variables a, d and e have been converted."),
                             fixed = TRUE)
              expect_warning(AsNumeric(X, binary = TRUE),
                             paste0(warn.msg.without.names, " The variable e has been converted."),
                             fixed = TRUE)
              # Checking that names are not changed.
              xz = data.frame(Q2, Q3, Q4)
              rownames(xz)[2] = "dog"
              expect_warning(xz1 <- AsNumeric(xz,  binary = FALSE, remove.first = FALSE),
                             warn.msg, fixed = TRUE)
              expect_equal(rownames(xz)[2], rownames(xz1)[2])

              # Dates
              dd <- seq(ISOdate(2000,1,1,0), by="day", length.out=10)
              ds <- sprintf("2000-01-%02d", 1:10)
              dm <- c(ds[1:5], "dog", "other random thing")
              expect_equal(range(AsNumeric(dd, binary=F)), c(946684800, 947462400))
              expect_equal(range(AsNumeric(ds, binary=F)), c(946684800, 947462400))
              expect_equal(expect_warning(AsNumeric(dm, binary = FALSE),
                                          warn.msg.without.names, fixed = TRUE),
                           1:7)

              # list - checking for variable names
              expect_warning(AsNumeric(list(A = Q2, B = Q3, C = Q4),  binary = FALSE, remove.first = TRUE),
                             paste0(warn.msg.without.names, " The variables A, B and C have been converted."),
                             fixed = TRUE)
              expect_error(AsNumeric(list(Q2, Q3, Q4),  binary = TRUE, remove.first = TRUE), NA)
              expect_error(AsNumeric(list(Q2, Q3, Q4),  binary = FALSE, remove.first = TRUE), NA)

              # character vector - check order
              cc <- sprintf("Row %d", c(1:100, 1:3))
              expect_equal(expect_warning(AsNumeric(cc, binary = FALSE),
                                          warn.msg.without.names, fixed = TRUE),
                           c(1:100, 1:3))

              attr(cc, "label") <- "lbl"
              expect_warning(output <- AsNumeric(cc, binary = FALSE),
                             warn.msg.without.names, fixed = TRUE)
              expect_equal(attr(output, "label"), "lbl")
              # Logical inputs
              expect_equal(AsNumeric(c(TRUE, FALSE)), 1:0)
              ## Attributes survive
              expect_equal(AsNumeric(structure(c(TRUE, FALSE), foo = "bar")),
                                     structure(1:0, foo = "bar"))
              ## Array/matrix ok
              expect_equal(AsNumeric(structure(matrix(c(TRUE, FALSE), nrow = 2), foo = "bar")),
                           structure(matrix(1:0, nrow = 2), foo = "bar"))
              # Data frame with logical element is coerced to integer
              df.in <- structure(list(x = structure(c(TRUE, FALSE), foo = "bar"),
                                      y = 1:2,
                                      z = structure(11:12, bar = "baz")),
                                 row.names = 1:2,
                                 class = "data.frame")
              df.out <- structure(list(x = structure(1:0, foo = "bar", name = "x"),
                                       y = structure(1:2, name = "y"),
                                       z = structure(11:12, bar = "baz", name = "z")),
                                  row.names = c("1", "2"),
                                  class = "data.frame")
              expect_equal(AsNumeric(df.in), df.out)
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

test_that("asNumericVector",
          {
              text <- c("abc", "$124", "($5)", "(.5)", "($.99)", "(123", "23%", "(50.1%)")
              expect_error(parsed <- asNumericVector(text), NA)
              expect_equal(parsed, c(NA, 124, -5, -0.5, -0.99, NA, 0.23, -0.501), check.attributes = FALSE)
          })

test_that("AsNumeric labels", {
    x <- factor(c(1,0,0,1,1,0))
    attr(x, "label") <- "lbl"
    expect_equal(attr(AsNumeric(x), "label"), "lbl")
    expect_equal(attr(suppressWarnings(AsNumeric(x, binary = FALSE)), "label"), "lbl")

    cc <- sprintf("Row %d", c(1:100, 1:3))
    attr(cc, "label") <- "lbl"
    expect_equal(attr(suppressWarnings(AsNumeric(cc, binary = FALSE)), "label"), "lbl")

    dd <- seq(ISOdate(2000,1,1,0), by="day", length.out=10)
    attr(dd, "label") <- "lbl"
    expect_equal(attr(suppressWarnings(AsNumeric(dd, binary = FALSE)), "label"), "lbl")
})

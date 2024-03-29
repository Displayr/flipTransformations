context("Variable")
data(bank, package = "flipExampleData")
data(cola, package = "flipExampleData")

test_that("Dichotomizing works",
{
    expect_true(all(table(DichotomizeFactor(factor(LETTERS[1:10]))) == c(5,5)))
    expect_true(all(table(DichotomizeFactor(bank$Overall)) == c(265, 497)))
    attr(bank$Fees, "label") <- "Bank Fees"
    expect_equal(attr(DichotomizeFactor(bank$Fees), "label"), "Bank Fees >= 3")
    expect_equal(attr(DichotomizeFactor(bank$Overall), "label"), "Overall >= 4")
    expect_equal(attr(suppressWarnings(CreatingBinaryVariableIfNecessary(bank, "Fees")), "label"), "Bank Fees >= 3")
    z <- suppressWarnings(CreatingBinaryDependentVariableIfNecessary(Fees ~ Overall, bank))$Fees
    expect_equal(attr(z, "label"), "Bank Fees >= 3")
})

test_that("Factor",
{
    expect_equal(attr(cola$Q2,"label"), attr(Factor(cola$Q2),"label"))
})


test_that("OrderedToNumeric",
          {
              expect_warning(OrderedToNumeric(factor(c("A", "D", "B", "A", "D"), ordered = TRUE)),
                             "Data has been automatically converted to numeric.")
              expect_warning(OrderedToNumeric(factor(c(3,6,8,2,5,3,6,2), ordered = TRUE)),
                             "Data has been automatically converted to numeric.")
          })


test_that("Ordered",
{
    expect_equal(attr(cola$Q2,"label"), attr(Ordered(cola$Q2),"label"))
})


test_that("Dates",
{

    date.var <- structure(list(date = structure(c(1327795200, 1330041600, 1328486400, 1330214400, 1331596800, 1325376000,
                                                  1326412800, 1329609600, 1327017600, 1331942400),
                                                class = c("POSIXct", "POSIXt", "QDate"),
                                                QDate = structure(c(1L, 2L, 2L, 2L, 3L, 1L, 1L, 2L, 1L, 3L),
                                                                  class = c("ordered", "factor"),
                                                                  .Label = c("January 2012", "February 2012", "March 2012",
                                                                             "April 2012", "May 2012", "June 2012", "July 2012",
                                                                             "August 2012", "September 2012")),
                                                questiontype = "Date", name = "date", label = "Interview Date", question = "Interview Date")),
                          .Names = "date", row.names = c(NA, -10L), class = "data.frame")

    processed.date.var <- structure(list(date = structure(c(1L, 2L, 2L, 2L, 3L, 1L, 1L, 2L, 1L, 3L),
                                                          class = c("ordered", "factor"),
                                                          .Label = c("January 2012","February 2012", "March 2012", "April 2012",
                                                                     "May 2012", "June 2012", "July 2012", "August 2012", "September 2012"),
                                                          name = "date", label = "Interview Date", question = "Interview Date",
                                                          questiontype = "Date")),
                                    .Names = "date", row.names = c(NA, -10L), class = "data.frame")

    test_that("ProcessQVariables", expect_equal(ProcessQVariables(date.var), processed.date.var))
    ## integer rownames are preserved as integers
    expect_equal(attr(ProcessQVariables(date.var), "row.names"), 1:10)
    rownames(processed.date.var) <- rownames(date.var) <- letters[1:10]
    test_that("ProcessQVariables preserves rownames", expect_equal(ProcessQVariables(date.var), processed.date.var))
})

test_that("asNumericWarning messages", {
    single.variable <- "Coca-Cola"
    two.variables <- c("Coca-Cola", "Pepsi")
    three.variables <- c("Coca-Cola", "Pepsi", "Red Bull")
    message.head <- paste0("Data has been automatically converted to numeric. Values are assigned in the ",
                           "order of the categories: 1, 2, 3, ...; To use alternative numeric values, ",
                           "transform the data prior including it in this analysis (e.g. by changing its structure).")
    expect_equal(flipTransformations:::asNumericWarning(single.variable, to.factor.levels = FALSE),
                 paste(message.head, "The variable Coca-Cola has been converted."))
    expect_equal(flipTransformations:::asNumericWarning(two.variables, to.factor.levels = FALSE),
                 paste0(message.head, " The variables Coca-Cola and Pepsi have been converted."))
    expect_equal(flipTransformations:::asNumericWarning(three.variables, to.factor.levels = FALSE),
                 paste0(message.head, " The variables Coca-Cola, Pepsi and Red Bull have been converted."))
    # Catch case where variable has empty label variable name in asNumericWarning is NULL
    expect_equal(flipTransformations:::asNumericWarning(NULL, to.factor.levels = FALSE),
                 paste0(message.head, " "))
})

test_that("Single level factor", {
    vals <- c(1,1,1,1,1,1, NA, NA)
    ff <- factor(vals, levels = 1)
    expect_equal(FactorToIndicators(ff), data.frame("ff.1" = vals))
})


test_that("ProcessAndStackDataForRegression", {
    # Originally from Cola Tracking - January to September.sav
    data.to.stack <- readRDS("data.for.stacking.unit.test.rds")
    cola.stacked <- readRDS("cola.stacked.data.rds")

    unstacked.data <- data.to.stack$unstacked.data
    subset <- data.to.stack$subset
    weights <- data.to.stack$weights
    interaction <- data.to.stack$interaction
    exclude.vars.warning  <- "have been removed"
    expect_warning(stacked <- ProcessAndStackDataForRegression(unstacked.data = unstacked.data, 
                                                formula = NULL, 
                                                interaction = interaction,
                                                subset = subset, 
                                                weights = weights),
                    exclude.vars.warning)
    expect_equal(as.character(stacked$formula),
                c("~", "Y", "X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9"))
    expect_equal(stacked$data, cola.stacked)

    # Can run on data only
    expect_error(suppressWarnings(ProcessAndStackDataForRegression(unstacked.data = unstacked.data, 
                                                 formula = NULL, 
                                                 interaction = NULL,
                                                 subset = NULL, 
                                                 weights = NULL),
                NA))
    
    # Mismatched data
    wrong.cases <- unstacked.data
    wrong.cases$Y <- wrong.cases$Y[-1, ]
    expect_error(ProcessAndStackDataForRegression(unstacked.data = wrong.cases, 
                                                 formula = NULL, 
                                                 interaction = interaction,
                                                 subset = subset, 
                                                 weights = weights),
                "Size of variables doesn't agree")
})


test_that("StackTextAndCategorization", {
    data.to.stack <- readRDS("text.analysis.stacking.rds")
    stacked.predicted.multi <- readRDS("stacked.predicted.categorization.multi.rds")
    stacked.predicted.multi <- stacked.predicted.multi$predicted
    stacked.predicted.single <- readRDS("stacked.predicted.categorization.single.rds")
    stacked.predicted.single <- stacked.predicted.single$predicted
    # No errors
    # Text - Multi with Binary - Grid
    expect_error(stacked <- StackTextAndCategorization(text = data.to.stack$text.multi, 
                                            existing.categorization = data.to.stack$binary.grid),
                NA)
    expect_equal(length(stacked$text), 1800)
    expect_equal(nrow(stacked$existing), 1800)
    # Text - Multi with Nominal - Multi
    expect_error(stacked <- StackTextAndCategorization(text = data.to.stack$text.multi, 
                                            existing.categorization = data.to.stack$nominal.multi),
                NA)
    # Several text variables with Binary - Grid
    expect_error(StackTextAndCategorization(text = data.to.stack$multiple.text, 
                                            existing.categorization = data.to.stack$binary.grid),
                NA)
    # Several text variables with Nominal - Multi
    expect_error(StackTextAndCategorization(text = data.to.stack$multiple.text, 
                                            existing.categorization = data.to.stack$nominal.multi),
                NA)
})
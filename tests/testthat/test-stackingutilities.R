
test_that("ProcessAndStackDataForRegression", {
    # Originally from Cola Tracking - January to September.sav
    data.to.stack <- readRDS("data.for.stacking.unit.test.rds")
    cola.stacked <- readRDS("cola.stacked.data.rds")

    unstacked.data <- data.to.stack$unstacked.data
    subset <- data.to.stack$subset
    weights <- data.to.stack$weights
    interaction <- data.to.stack$interaction
    exlude.vars.warning  <- "'None of these' have been removed from the set of predictor variables in 'q5' since they don't appear in the set of outcome variables in 'Brand attitude scores'"
    expect_warning(stacked <- ProcessAndStackDataForRegression(unstacked.data = unstacked.data, 
                                                formula = NULL, 
                                                interaction = interaction,
                                                subset = subset, 
                                                weights = weights),
                    "'None of these' have been removed")
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

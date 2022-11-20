
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



# Test Stacking and Unstacking of Text and Categorization
data.to.stack <- readRDS("text.analysis.stacking.rds")
stacked.predicted.multi <- readRDS("stacked.predicted.categorization.multi.rds")
stacked.predicted.multi <- stacked.predicted.multi$predicted
stacked.predicted.single <- readRDS("stacked.predicted.categorization.single.rds")
stacked.predicted.single <- stacked.predicted.single$predicted
existings <- c("NULL", "binary.grid", "nominal.multi")
texts <- c("text.multi", "multiple.text")
weights <- subset <- rep(1, nrow(data.to.stack$text.multi))
for (text.case in texts) {
    for (existing.case in existings) {
        existing <- if (existing.case == "NULL") NULL else data.to.stack[[existing.case]]
        text <- data.to.stack[[text.case]]
        test_that(paste0("StackTextAndCategorization: ", text.case, " : ", existing.case), {
            expect_error(stacked <- StackTextAndCategorization(text = data.to.stack$text.multi, 
                                    existing.categorization = existing,
                                    weights = weights,
                                    subset = subset),
            NA)

            # Correct dimensions
            expect_equal(length(stacked$text), 1800)
            expect_equal(length(stacked$weights), 1800)
            expect_equal(length(stacked$subset), 1800)
            expect_equal(length(stacked$inds), 1800)
            if (existing.case == "binary.grid") {
                expect_equal(nrow(stacked$existing), 1800)
            } else if (existing.case == "nominal.multi") {
                expect_equal(length(stacked$existing), 1800)
            }

            # Can unstack predicted categorization using the inds
            # obtained during stacking
            if (!is.null(existing)) {
                stacked.predicted <- if (existing.case == "binary.grid") stacked.predicted.multi else stacked.predicted.single
                expect_error(unstacked <- UnstackCategorization(stacked.predicted, stacked$inds), NA)
                expect_equal(nrow(unstacked), 600)
            }
            

        })
    }
}




test_that("ProcessAndStackDataForRegression", {
    # Originally from Cola Tracking - January to September.sav
    data.to.stack <- readRDS("data.for.stacking.unit.test.rds")
    cola.stacked <- readRDS("cola.stacked.data.rds")

    unstacked.data <- data.to.stack$unstacked.data
    subset <- data.to.stack$subset
    weights <- data.to.stack$weights
    interaction <- data.to.stack$interaction
    exclude.vars.warning  <- paste0("The variable(s): ",
                                    sQuote("None of these"),
                                    " have been removed from the set of predictor variables in ",
                                    sQuote("q5"), 
                                    " since they don't appear in the set of outcome variables in ",
                                    sQuote("Brand attitude scores"))
    expect_warning(stacked <- ProcessAndStackDataForRegression(unstacked.data = unstacked.data, 
                                                formula = NULL, 
                                                interaction = interaction,
                                                subset = subset, 
                                                weights = weights),
                    exclude.vars.warning, fixed = TRUE)
    expect_equal(as.character(stacked$formula),
                c("~", "Y", "X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9"))
    expect_equal(stacked$data, cola.stacked)

    # Can run on data only
    expect_warning(ProcessAndStackDataForRegression(unstacked.data = unstacked.data, 
                                                 formula = NULL, 
                                                 interaction = NULL,
                                                 subset = NULL, 
                                                 weights = NULL),
                exclude.vars.warning, fixed = TRUE)
    
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
data.to.stack[["binary.grid.not.transposed"]] <- readRDS("binary.grid.not.transposed.rds")
stacked.predicted.multi <- readRDS("stacked.predicted.categorization.multi.rds")
stacked.predicted.multi <- stacked.predicted.multi$predicted
stacked.predicted.single <- readRDS("stacked.predicted.categorization.single.rds")
stacked.predicted.single <- stacked.predicted.single$predicted
existings <- c("NULL", "binary.grid", "binary.grid.not.transposed", "nominal.multi")
texts <- c("text.multi", "multiple.text")
weights <- subset <- rep(1, nrow(data.to.stack$text.multi))
for (text.case in texts) {
    for (existing.case in existings) {
        existing <- if (existing.case == "NULL") NULL else data.to.stack[[existing.case]]
        text <- data.to.stack[[text.case]]
        test_that(paste0("StackTextAndCategorization: ", text.case, " : ", existing.case), {
            expect_error(stacked <- StackTextAndCategorization(text = text,
                                    existing.categorization = existing,
                                    weights = weights,
                                    subset = subset),
            NA)

            # Correct dimensions
            expect_equal(length(stacked$text), 1800)
            expect_equal(length(stacked$weights), 1800)
            expect_equal(length(stacked$subset), 1800)
            expect_equal(length(stacked$inds), 1800)
            if (existing.case != "NULL")
                expect_equal(NROW(stacked$existing), 1800)

            # Correct values
            if (!is.data.frame(text)) 
                text <- as.data.frame(text, optional = TRUE)
            expect_equal(stacked$text[601], text[1, 2])
            expect_equal(stacked$text[650], text[50, 2])
            expect_equal(stacked$text[1201], text[1, 3])
            expect_equal(stacked$text[1250], text[50, 3])

            if (existing.case == "nominal.multi") {
                stacked.vals <- unname(stacked$existing)
                expect_equal(stacked.vals[601], existing[1, 2])
                expect_equal(stacked.vals[650], existing[50, 2])
                expect_equal(stacked.vals[1201], existing[1, 3])
                expect_equal(stacked.vals[1250], existing[50, 3])
            } else if (grepl("binary.grid", existing.case)) {
                unstacked.names <- colnames(existing)
                stacked.names <- colnames(stacked$existing)
                is.transposed <- attr(existing, "transposed")
                name.elements <- lapply(unstacked.names,
                                        FUN = function (x) {
                                            sp <- strsplit(x, ", ", fixed = TRUE)[[1]]
                                        })
                first.name.elements <- vapply(name.elements, `[[`, character(1), 1)
                second.name.elements <- vapply(name.elements, `[[`, character(1), 2)
                inds <- stacked$inds
                inds <- vapply(inds,
                               FUN = function (x) strsplit(x, ".", fixed = TRUE)[[1]][2],
                               FUN.VALUE = character(1))
                unique.inds <- unique(inds)
                for (cname in stacked.names) {
                    for (ind in unique.inds) {
                        stacked.match <- stacked$existing[inds == ind, cname]
                        col.match <- which(first.name.elements == ind & second.name.elements == cname)
                        if (length(col.match) == 0) browser()
                        unstacked.match <- existing[[col.match]]
                        if (!identical(stacked.match, unstacked.match)) browser()
                        expect_equal(stacked.match, unstacked.match)
                    }
                }
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


test_that("Text Stacking Errors when mismatched names", {
    text.bad.names <- data.to.stack$text.multi
    colnames(text.bad.names) <- c("Hello darkness", "My old", "Friend")
    nominal.multi <- data.to.stack$nominal.multi
    binary.grid <- data.to.stack$binary.grid
    expect_error(StackTextAndCategorization(text = text.bad.names, existing.categorization = nominal.multi),
                 "Unable to match the labels from the Text variables to the labels of Pick One - Multi Coke v Pepsi. Please modify the labels so that the text variables may be matched.")
    expect_error(StackTextAndCategorization(text = text.bad.names, existing.categorization = binary.grid),
                 "Unable to match the labels from the Text variables to the labels of Spontaneous Awareness: Spontaneous Awareness: 1st mention - Categorized2. Please modify the labels so that the text variables may be matched.")
    
    text.duplicate.names <- data.to.stack$text.multi
    colnames(text.duplicate.names) <- c("A", "B", "A")
    expect_error(StackTextAndCategorization(text = text.duplicate.names, existing.categorization = binary.grid),
                 "Unable to match the labels from the Text variables to the labels of Spontaneous Awareness: Spontaneous Awareness: 1st mention - Categorized2. Please modify the labels so that the text variables may be matched.")
})

test_that("No stacking when single text", {
    single.nominal <- data.to.stack$nominal.multi[1]
    attr(single.nominal, "questiontype") <- "PickOne"
    single.stack <- StackTextAndCategorization(data.to.stack$text.multi[1], 
                                            existing.categorization = single.nominal)
    expect_equal(single.stack$text,
                 data.to.stack$text.multi[[1]])
    expect_equal(single.stack$existing.categorization,
                 single.nominal)

    err.msg <- "The existing categorization should be a Nominal/Ordinal or Binary - Multi variable set"
    expect_error(StackTextAndCategorization(data.to.stack$text.multi[1], 
                                            existing.categorization = data.to.stack$nominal.multi),
                 err.msg)
    expect_error(StackTextAndCategorization(data.to.stack$text.multi[1], 
                                            existing.categorization = data.to.stack$binary.grid),
                 err.msg)
    
    err.msg <- "The existing categorization should be a Nominal/Ordinal - Multi or Binary - Grid variable set"
    expect_error(StackTextAndCategorization(data.to.stack$text.multi, 
                                            existing.categorization = single.nominal),
                 err.msg)
})


test_that("Variables matched correctly for PickOneMuli", {
    nominal.multi <- data.to.stack$nominal.multi
    nominal.multi.reversed <- nominal.multi[c(3, 2, 1)]
    nominal.multi.reversed <- CopyAttributes(nominal.multi.reversed, nominal.multi)
    stacked <- StackTextAndCategorization(data.to.stack$text.multi,
                                            existing.categorization = nominal.multi)
    stacked.reversed <- StackTextAndCategorization(data.to.stack$text.multi,
                                            existing.categorization = nominal.multi.reversed)
    expect_equal(stacked, stacked.reversed)
})
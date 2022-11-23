#' Helper function to stack data for a regression model.
#'
#' Takes the input unstacked data, interaction, subset, weights and formula terms,
#' processes the unstacked data, and stacks it.
#' If the stacking is successful, the interaction, subset, weights are also updated
#' to be the appropriate size
#'
#' @param unstacked.data A named list of two data frames. Element "X" is the data frame containing
#'  containing the outcome variable (usually a Numeric - Multi or Nominal/Ordinal - Multi 
#'  variable set in Displayr), and element "Y" is the data frame containing the predictor
#'  variables (a Binary - Grid or Numeric - Grid variable set in Displayr).
#' @param formula A formula object for the regression.
#' @param interaction Optional variable to test for interaction with other variables in the model.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process.
#' @param weights An optional vector of sampling weights.
#' @export
ProcessAndStackDataForRegression <- function(unstacked.data, formula, interaction, subset, weights)
{
    checkDataFeasibleForStacking(unstacked.data)
    unstacked.data <- removeDataReduction(unstacked.data)
    validated.unstacked.output <- validateDataForStacking(unstacked.data)
    unstacked.data <- validated.unstacked.output[["data"]]
    stacks <- validated.unstacked.output[["stacks"]]
    data <- stackData(unstacked.data)
    # Update interaction, subset and weights if necessary
    # if interaction vector supplied
    # it should be original n, needs to be stacked to n = nv where v is number oof outcome vars
    if (!is.null(interaction))
    {
        if (length(interaction) != nrow(data))
        {
            old.interaction <- interaction
            interaction <- rep(old.interaction, stacks)
            interaction <- CopyAttributes(interaction, old.interaction)
        }

        # Update subset to be consistent with interaction
        old.subset <- subset
        subset.description <- Labels(subset)
        tmp.sub <- !is.na(interaction)
        if (is.null(subset) || length(subset) <= 1)
        {
            subset <- tmp.sub
            attr(subset, "label") <- ""
        } else
        {
            subset <- subset & tmp.sub
            attr(subset, "label") <- subset.description
        }
    } else if (!is.null(subset) && length(subset) > 1)
    {
        old.subset <- subset
        subset <- rep(old.subset, stacks)
        subset <- CopyAttributes(subset, old.subset)
    }
    # Update weights
    if (!is.null(weights) && length(weights) != nrow(data))
    {
        old.weights <- weights
        weights <- rep(weights, stacks)
        weights <- CopyAttributes(weights, old.weights)
    }

    # Update formula
    formula <- updateStackedFormula(data, formula)
    list(data = data, formula = formula, interaction = interaction, subset = subset, weights = weights)
}

# Removes the data reduction columns and the reduction via the codeframe attribute, if available
# Also warn the user if necessary for NETs with both hidden and visible codes.
# Input is expected to be a list with two elements, Y, the outcome variable dataset and element X,
# containing the Predictor variable dataset
removeDataReduction <- function(data)
{
    # This list keeps track of the metadata for the warning
    reduction.list <- list(nets = NULL, variable.type = NULL)

    # Inspect the outcome multi first
    secondary.codeframe <- attr(data[["Y"]], "secondarycodeframe")
    codeframe <- attr(data[["Y"]], "codeframe")
    if (is.null(secondary.codeframe) && !is.null(codeframe))
    {
        reduction.columns <- flagCodeframeReduction(codeframe)
        data[["Y"]][reduction.columns] <- NULL
        attr(data[["Y"]], "codeframe")[reduction.columns] <- NULL
        reduction.list[['nets']] <- attr(reduction.columns, "nets")
        reduction.list[['variable.type']] <- "Outcome"
    } else if(!is.null(attr(data[["Y"]], "questiontype")))
    {# If older Q user, check question type and remove NET or SUM (default reduction)
        reduction.columns <- names(data[["Y"]]) %in% c("NET", "SUM")
        data[["Y"]][reduction.columns] <- NULL
    }

    # Clean the DataReduction for the predictor variables
    if (!is.null(question.type <- attr(data[["X"]], "questiontype")))
    {
        if (!all(c("codeframe", "secondarycodeframe") %in% names(attributes(data[["X"]]))))
        { # Use the default reduction names if reduction cant be deduced
            data.reduction.string <- if(question.type == "PickAnyGrid") "NET" else "SUM"
            grep.pattern <- paste0("(^", data.reduction.string, ", )|(, ", data.reduction.string,"$)")
            reduction.columns <- grepl(grep.pattern, names(data$X))
        } else
        { # Determine the data reduction columns from the codeframe and secondary codeframe
            codeframe <- attr(data[["X"]], "codeframe")
            secondary.codeframe <- attr(data[["X"]], "secondarycodeframe")
            reduction.rows <- flagCodeframeReduction(codeframe)
            reduction.columns <- flagCodeframeReduction(secondary.codeframe)
            if (!is.null(attr(reduction.rows, "net")) || !is.null(attr(reduction.columns, "net")))
            { # Retain the metadata
                reduction.list[['nets']] <- c(reduction.list[['nets']],
                                             c(attr(reduction.rows, "nets"), attr(reduction.columns, "nets")))
                reduction.list[['variable.type']] <- c(reduction.list[['variable.type']], "Predictor")
            }
            # Remove the data reduction from the codeframes for later use when determining the names
            if (any(reduction.rows))
                attr(data[["X"]], "codeframe")[reduction.rows] <- NULL
            if (any(reduction.columns))
                attr(data[["X"]], "secondarycodeframe")[reduction.columns] <- NULL
            # Remap to the columns in the data.frame
            reduction.columns <- apply(expand.grid(reduction.rows, reduction.columns), 1, any)
        }
        if (any(reduction.columns))
            data[["X"]][reduction.columns] <- NULL
    }
    # Warn the user if necessary
    if (!is.null(reduction.list[['nets']]))
        throwCodeReductionWarning(reduction.list)
    data
}

# Identify if there are any elements in a codeframe (or secondarycodeframe)
# that are completely redundant data reductions.
# This is achieved by looking at the longest list element in the codeframe and
# checking that all the numeric indicies in the longest element exist
# in other elements.
# Input x is the codeframe attribute
flagCodeframeReduction <- function(x)
{
    flags <- rep(FALSE, length(x))
    names(flags) <- names(x)
    # Check if there are any duplicated variables and flag them for removal
    # unname incase the user renames the duplicated variable
    if (any(duplicated.vars <- duplicated(unname(x))))
        flags[duplicated.vars] <- TRUE
    # Check the remaining non duplicates
    non.duplicated <- x[!duplicated.vars]
    lengths <- vapply(non.duplicated, length, numeric(1))
    # Inspect possible NETs after duplicated vars are removed
    possible.nets <- lengths > 1
    if (any(possible.nets))
    {
        removed.vals <- list() # Keep track of values that have been removed.
        complete.reduction <- list() # Keep track of nets that are complete data reductions.
        possible.redundant.nets <- which(possible.nets)
        # Order by decreasing size and remove one-by-one to handle supersets.
        net.sizes <- sort(lengths[possible.redundant.nets], decreasing = TRUE)
        for (net in names(net.sizes))
        {
            # Remove current net candidate from search list
            reduced.codeframe <- non.duplicated[-which(names(non.duplicated) == net)]
            if (any(non.duplicated[[net]] %in% unlist(reduced.codeframe)))
            { # Update flags and remove from original list
                flags[which(names(flags) == net)] <- TRUE
                removed.vals[[net]] <- non.duplicated[[net]]
                complete.reduction[[net]] <- all(unlist(reduced.codeframe) %in% non.duplicated[[net]])
                non.duplicated[net] <- NULL
            }
        }
        # If there are some nets to be removed, check the codes and warn if necessary (NET not mutually exclusive or complete)
        if (length(removed.vals) != 0)
        {
            # Check if there are any codes in the removed constructed nets that are not observed anywhere else
            unobserved.codes <- lapply(removed.vals, function(x) {
                y <- !x %in% unlist(reduced.codeframe)
                names(y) <- x
                y
            })
            # Only worry about nets that have unobserved codes and are not a complete reduction (complete NET or SUM)
            unobserved.in.nets <- vapply(unobserved.codes, any, logical(1)) & !vapply(complete.reduction, any, logical(1))
            # Add the metadata to the return logical vector for a simpler warning message.
            if (any(unobserved.in.nets))
            { # Reverse the the order of identified NETs with unobserved codes to show smaller NET groups first.
                attr(flags, "nets")  <- rev(names(which(unobserved.in.nets)))
            }
        }
    }
    flags
}

# Checks to be coded
checkDataFeasibleForStacking <- function(data)
{
    checkListStructure(data)
    checkNumberObservations(data)
    validMultiOutcome(data[["Y"]])
    validGridPredictor(data[["X"]])
}

checkListStructure <- function(data)
{
    named.elements <- c("X", "Y") %in% names(data)
    if ((is.null(data) || !(is.list(data) && all(named.elements))))
        stop("'unstacked.data' needs to be a list with two elements, ",
             "'Y' containing a data.frame with the outcome variables and ",
             "'X' containing a data.frame with the predictor variables.")
}

validateDataForStacking <- function(data)
{
    outcome.names <- getMultiOutcomeNames(data[["Y"]])
    # Validate the Grid predictors, transpose if necessary and error if no matches between X and Y
    data[["X"]] <- validateNamesInGrid(data)
    names.in.predictor.grid <- getGridNames(data[["X"]])

    outcome.names.in.grid.elements <- vapply(names.in.predictor.grid, function(x) {
        any(outcome.names %in% x)
        }, logical(1))
    outcome.names.in.grid <- names.in.predictor.grid[[which(outcome.names.in.grid.elements)]]
    unique.outcome.names.in.grid <- unique(outcome.names.in.grid)
    # Remove any outcome variables that aren't seen in predictors and warn
    data[["Y"]] <- validateOutcomeVariables(data, outcome.names, unique.outcome.names.in.grid)
    outcome.names <- getMultiOutcomeNames(data[["Y"]])

    # Remove any predictor variables that aren't seen in outcome variables and warn
    data[["X"]] <- validatePredictorVariables(data, outcome.names,
                                              unique.outcome.names.in.grid,
                                              outcome.names.in.grid)
    names.in.predictor.grid <- getGridNames(data[["X"]])
    outcome.names.in.grid.elements <- vapply(names.in.predictor.grid, function(x) {
        any(outcome.names %in% x)
    }, logical(1))
    outcome.names.in.grid <- names.in.predictor.grid[[which(outcome.names.in.grid.elements)]]
    unique.outcome.names.in.grid <- unique(outcome.names.in.grid)
    # Ensure columns align before stacking
    data[["Y"]] <- checkStackAlignment(data, outcome.names, unique.outcome.names.in.grid)
    return(list(data = data, stacks = ncol(data[["Y"]])))
}

# The stacking requires names of the grid data.frame to be in the form predictor, outcome (comma separated)
# If metadata available in the codeframe, the names are uniquely identified
# If metadata is unavailable, no commas allowed in names to avoid ambiguity.
validateNamesInGrid <- function(data)
{
    outcome.names <- getMultiOutcomeNames(data[["Y"]])
    if (!is.null(outcome.question.name <- attr(data[["Y"]], "question")))
        outcome.variable.set.name <- sQuote(outcome.question.name)
    else
        outcome.variable.set.name <- sQuote("Y")
    # Determine which dimension labels in the grid match the outcome.names
    # getGridNames extracts a list with two elements, the "a, b" parts of the grid names
    grid.names <- getGridNames(data[["X"]])
    # Check if any labels match
    matches <- lapply(grid.names, function(x) outcome.names %in% x)
    any.matches <- vapply(matches, any, logical(1))
    # No labels match at all, error since there is nothing to align for stacking
    if (all(!any.matches))
        stop("It is not possible to stack these variables since none of the outcome variable labels ",
             "match the variable labels in the predictor variables. The outcome variables ",
             outcome.variable.set.name, " have labels: ", paste0(sQuote(outcome.names), collapse = ", "),
             " which don't appear in the labels of the grid of predictor variables.")
    # Check if is a clear match (no clash of predictor names with outcome names) and no codeframe available,
    # then 'transpose' the grid labels, i.e. outcome, predictor labels changed to predictor, outcome
    dimensions.matching <- sum(any.matches)
    if (dimensions.matching == 1 && any.matches[1] && is.null(attr(data[["X"]], "codeframe")))
        names(data[["X"]]) <- paste0(grid.names[[2]], ", ", grid.names[[1]])
    # Throw error for ambiguous cases, i.e. outcome labels appear in both grid label dimensions.
    if (dimensions.matching == 2)
    {
        matched.outcomes <- outcome.names[unique(unlist(lapply(matches, which)))]
        ambiguous.message <- paste0("The outcome variable ", outcome.variable.set.name, " has labels: ",
                                    paste0(sQuote(matched.outcomes), collapse = ", "), " and these labels appear ",
                                    "in both dimensions of the grid predictor variables. Please rename the ",
                                    "labels in either the outcome variables or grid predictor variables to ",
                                    "stack the variables and proceed.")
        stop("Ambiguous labels in the grid predictors need to be reconciled before stacking can occur. ",
             ambiguous.message)
    }
    return(data[["X"]])
}

#' @importFrom methods is
validMultiOutcome <- function(data)
{
    if (!is(data, "data.frame"))
        stop("Outcome variable to be stacked needs to be a data.frame. ",
             "Please assign a data.frame to the \"Y\" element of the 'unstacked.data' argument.")
}

checkNumberObservations <- function(data)
{
    if (!diff(unlist(nrows <- lapply(data, NROW))) == 0)
    {
        stop("Size of variables doesn't agree, the provided outcome variables have ", nrows[["Y"]],
             " observations while the provided predictor variables have ", nrows[["X"]],
             " observations. Please input variables that have the same size.")
    }
}

#' @importFrom methods is
validGridPredictor <- function(data)
{
    if (!is(data, "data.frame"))
        stop("Predictor variables to be stacked needs to be a data.frame. ",
             "Please assign a data.frame to the \"X\" element of the 'unstacked.data' argument.")
}

validateOutcomeVariables <- function(data, outcome.names, predictor.names)
{
    if (any(missing.stack <- !outcome.names %in% predictor.names))
    {
        data[["Y"]][missing.stack] <- NULL
        removed.outcome.variables <- paste0(sQuote(outcome.names[missing.stack]), collapse = ", ")
        outcome.variable.set.name <- attr(data[["Y"]], "question")
        predictor.variable.set.name <- attr(data[["X"]], "question")
        if (is.null(outcome.variable.set.name) | is.null(predictor.variable.set.name))
            warning("The variable(s): ", removed.outcome.variables, " have been removed from the set of outcome ",
                    "variables since these variables don't appear in the set of predictor variables.")
        else
            warning("The variable(s): ", removed.outcome.variables, " have been removed from the set of outcome ",
                    "variables in ", sQuote(outcome.variable.set.name), " since they don't appear in the set of ",
                    "predictor variables in ", sQuote(predictor.variable.set.name))
        # Remove the name from the codeframe too,
        if (!is.null(attr(data[["Y"]], "secondarycodeframe")))
            attr(data[["Y"]], "secondarycodeframe")[missing.stack] <- NULL
        else if (!is.null(attr(data[["Y"]], "codeframe")))
            attr(data[["Y"]], "codeframe")[missing.stack] <- NULL
    }
    return(data[["Y"]])
}

validatePredictorVariables <- function(data, outcome.names, predictor.names, unstacked.names)
{
    if (any(unstackable.predictors <- !predictor.names %in% outcome.names))
    {
        unstackable.predictor.names <- predictor.names[unstackable.predictors]
        data[["X"]][unstacked.names %in% unstackable.predictor.names] <- NULL
        removed.predictor.variables <- paste0(sQuote(unstackable.predictor.names), collapse = ", ")
        outcome.variable.set.name <- attr(data[["Y"]], "question")
        predictor.variable.set.name <- attr(data[["X"]], "question")
        if (is.null(outcome.variable.set.name) | is.null(predictor.variable.set.name))
            warning("The variable(s): ", removed.predictor.variables, " have been removed from the set of predictor ",
                    "variables since these variables don't appear in the outcome variables.")
        else
            warning("The variable(s): ", removed.predictor.variables, " have been removed from the set of predictor ",
                    "variables in ", sQuote(predictor.variable.set.name), " since they don't appear in the set of ",
                    "outcome variables in ", sQuote(outcome.variable.set.name))

        # Remove the name from the codeframe too
        if (!is.null(attr(data[["X"]], "codeframe")))
        {
            # Determine if the outcome labels are stored in codeframe or secondarycodeframe
            codeframe.names <- names(attr(data[["X"]], "codeframe"))
            correct.codeframe <- if (any(predictor.names %in% codeframe.names))
                                    "codeframe"
                                 else
                                    "secondarycodeframe"
            attr(data[["X"]], correct.codeframe)[unstackable.predictors] <- NULL
        }

    }
    return(data[["X"]])
}

#' @importFrom flipU CopyAttributes
checkStackAlignment <- function(data, outcome.names, predictor.names)
{
    if (!identical(outcome.names, predictor.names))
    {
        new.column.order <- match(predictor.names, outcome.names)
        tmp <- data[["Y"]]
        if (!is.null(attr(tmp, "secondarycodeframe")))
            attr(tmp, "secondarycodeframe") <- attr(tmp, "secondarycodeframe")[new.column.order]
        else if (!is.null(attr(tmp, "codeframe")))
            attr(tmp, "codeframe") <- attr(tmp, "codeframe")[new.column.order]
        data[["Y"]] <- data[["Y"]][new.column.order]
        data[["Y"]] <- CopyAttributes(data[["Y"]], tmp)
    }
    return(data[["Y"]])
}

stackData <- function(data)
{
    outcome.names <- getMultiOutcomeNames(data[["Y"]])
    stacked.outcome <- stackOutcome(data[["Y"]], outcome.names)
    stacked.predictors <- stackPredictors(data[["X"]], outcome.names)
    if (!all(row.names(stacked.outcome) == row.names(stacked.predictors)))
        stop("Stacked variables are not aligned properly. Contact support for further help.")
    stacked.data <- cbind(stacked.outcome, stacked.predictors)
    return(stacked.data)
}

#' @importFrom stats reshape
stackPredictors <- function(data, outcome.names)
{
    question.label <- attr(data, "question")
    if (!is.null(codeframe <- attr(data, "codeframe")) &&
        !is.null(secondary.codeframe <- attr(data, "secondarycodeframe")))
    {
        if (any(outcome.names %in% names(codeframe)))
        {
            predictor.names <- names(secondary.codeframe)
            variables.to.stack <- lapply(predictor.names, function(x) paste0(x, ", ", outcome.names))
            names(variables.to.stack) <- predictor.names
        } else
        {
            predictor.names <- names(codeframe)
            variables.to.stack <- lapply(predictor.names, function(x) paste0(outcome.names, ", ", x))
            names(variables.to.stack) <- predictor.names
        }
        stacked.data <- reshape(data, varying = variables.to.stack, times = outcome.names, sep = ",",
                                v.names = predictor.names, direction = "long")
    }
    else
        stacked.data <- reshape(data, varying = names(data), sep = ", ",
                                times = outcome.names, direction = "long")
    stacked.data <- removeReshapingHelperVariables(stacked.data)
    stacked.data <- addLabelAttribute(stacked.data, label = question.label)
    names(stacked.data) <- paste0("X", 1:ncol(stacked.data))
    stacked.data
}

#' @importFrom stats reshape
stackOutcome <- function(data, outcome.names)
{
    v.name <- if (!is.null(question.attr <- attr(data ,"question"))) question.attr else "Y"
    stacked.data <- reshape(data, varying = names(data), v.names = v.name,
                            times = outcome.names, direction = "long")
    stacked.data <- removeReshapingHelperVariables(stacked.data)
    stacked.data <- addLabelAttribute(stacked.data)
    names(stacked.data) <- "Y"
    stacked.data
}

addLabelAttribute <- function(data, label = NULL)
{
    variable.names <- colnames(data)
    if (!is.null(label))
        variable.names <- paste0(label, ": ", variable.names)
    for (i in seq_along(data))
        attr(data[[i]], "label") <- variable.names[i]
    data
}

removeReshapingHelperVariables <- function(data)
{
    data[["id"]] <- NULL
    data[["time"]] <- NULL
    data
}

# Return the names of the predictors and their associated matched response values
# It assumes that outcome names are given as the second comma separate value
# and predictor names would be the first or alternatively
# it assumes that the codeframe has the outcome names and secondarycodeframe has
# the predictor names.
# If this is incorrect, then it will be corrected or matched in either
# validateNamesInGrid or validateDataForStacking
getGridNames <- function(data)
{
    if (all(c("codeframe", "secondarycodeframe") %in% names(attributes(data))))
    {
        outcome.names <- names(attr(data, "codeframe"))
        m <- length(outcome.names)
        predictor.names <- names(attr(data, "secondarycodeframe"))
        p <- length(predictor.names)
        predictor.names <- rep(predictor.names, each = m)
        outcome.names <- rep(outcome.names, p)
    } else
    {
        split.names <- strsplit(names(data), ", ")
        splits <- vapply(split.names, length, numeric(1))
        if (any(ambiguous.splits <- splits != 2))
            stop("The variable labels in the predictor grid should be comma separated to determine the columns ",
                 "that belong to the appropriate outcome variable. This means that the variable labels cannot ",
                 "use commas. Please remove the commas in the names in the predictor grid to continue ",
                 "the analysis. The variable labels that are ambiguous and require fixing are: ",
                 paste0(sQuote(names(data)[ambiguous.splits]), collapse = ", "))
        outcome.names <- sapply(split.names, "[", 2)
        predictor.names <- sapply(split.names, "[", 1)
    }
    list(predictor.names, outcome.names)
}

getMultiOutcomeNames <- function(data)
{
    if (!is.null(secondary.codeframe <- attr(data, "secondarycodeframe")))
        names(secondary.codeframe)
    else if (!is.null(code.frame <- attr(data, "codeframe")))
        names(code.frame)
    else
        names(data)
}

#' @importFrom stats as.formula
updateStackedFormula <- function(data, formula)
{
    new.formula <- as.formula(paste0("Y ~ ", paste0("X", 1:(ncol(data) - 1), collapse = " + ")),
                              env = environment(formula))
    return(new.formula)
}

#' Throw warning to the user that there are some codes used in a data reduction that are not seen
#' elsewhere in the codeframe, e.g. a NET has code A and B and the code A is seen in the codeframe
#' but the code B is not seen elsewhere in the codeframe. So removing the data reduction will lose
#' the information about code B.
#' @param reduction.list List that contains two elements, \itemize{
#'  \item nets Character vector of net(s) that exhibit this situation
#'  \item variable.type Character vector stating the variable set structure that this applies to.
#' e.g. The predictor grid or outcome variables. Could be a single element or vector with two elements
#' if both outcome and predictor grids are affected.
#' }
#' @noRd
throwCodeReductionWarning <- function(reduction.list)
{
    nets <- reduction.list[["nets"]]
    variable.type <- reduction.list[["variable.type"]]
    net.name.txt <- paste0(sQuote(nets, q = FALSE))
    if (length(nets) > 1)
        net.name.txt <- paste0(": (", paste0(net.name.txt, collapse = "; "), ")")
    net.txt <- ngettext(length(nets), "a NET, ", "NETs")
    variable.type.and <- paste0(paste0(variable.type, collapse = " and "), " variables")
    variable.type.or <- paste0(paste0(variable.type, collapse = " or "), " variables")
    net.txt.2 <- ngettext(length(nets), "this NET was", "these NETs were")
    net.txt.3 <- ngettext(length(nets), "this NET", "any of these NETs")
    warning("NETs are removed from this analysis unless all their source values are mutually exclusive ",
            "to other codes. The ", variable.type.and, " have ", net.txt, net.name.txt, " that contains ",
            "source values that partially overlap with other codes. Consequently, ", net.txt.2, " not used in ",
            "the analysis. If you wish ", net.txt.3, " to be used in the analysis then please modify the ",
            variable.type.or, " via the Table view options appropriately.")
}

#' Stacks several text variables and an existing categorization 
#' in a consisten manner for text analysis functions.
#' 
#' @param text A data frame containing one or more character vectors. Typically
#' these start their life as variables in a Text - Multi variable set in Displayr
#' or are several related Text variable sets.
#' @param existing.categorization A data frame containing the corresponding
#' categories. This could be a data frame of factor variables (a single-response 
#' categorization), or a data frame of binary numeric variables (a multiple-response
#' categorization). This can be NULL if no existing categorization is to be used
#' in the analysis.
#' @param subset A boolean vector indicating the subset of cases that the user
#' wishes to include later in a classification of the text. Usually comes from QFilter.
#' This will be expanded to align with the stacked data.
#' @param weights A numeric vector of case weights for later us in the analysis.
#' This will be expanded to align with the stacked data.
#' @importFrom utils stack
#' @importFrom flipFormat TidyLabels ExtractCommonPrefix
#' @export
StackTextAndCategorization <- function(text, existing.categorization = NULL, subset = TRUE, weights = NULL) {
    
    # One text variable, nothing to stack
    if (is.list(text) && length(text) == 1) {
        if (!is.null(existing.categorization) && 
            ! attr(existing.categorization, "questiontype") %in% c("PickOne", "PickAny"))
            stop("The existing categorization should be a Nominal/Ordinal or Binary - Multi variable set")
        return(list(text = text[[1]],
                    existing.categorization = existing.categorization,
                    subset = subset,
                    weights = weights))
    }

    if (!is.null(existing.categorization) && 
        ! attr(existing.categorization, "questiontype") %in% c("PickOneMulti", "PickAnyGrid"))
        stop("The existing categorization should be a Nominal/Ordinal - Multi or Binary - Grid variable set")

    if (!is.data.frame(text)) {
        text <- as.data.frame(text, optional = TRUE)
        text.names <- TidyLabels(colnames(text))
        colnames(text) <- text.names
        attr(text, "codeframe") <- fakeCodeFrame(text.names)
        attr(text, "questiontype") <- "TextMulti"
        question.names <- vapply(text, FUN = function(x) attr(x, "question"), FUN.VALUE = character(1))
        text.label <- ExtractCommonPrefix(question.names)$common.prefix
    } else {
        text.label <- attr(text, "question")
    }
    
    n.text.vars <- ncol(text)

    if (is.null(existing.categorization)) { 
        # No existing categorization to match against.
        # Stack the text and the filter and weight
        st <- stack(lapply(text, as.vector))
        inds <- paste0(rownames(st), ".", st[, "ind"])
        input.data <- list(text = st[, "values"],
                          existing = NULL,
                          subset = if (length(subset) == 1) subset else rep(subset, times = n.text.vars),
                          weights = rep(weights, times = n.text.vars),
                          inds = inds)
    } else {
        text <- prepareTextVariableLabelsForStackingWithGrids(text, existing.categorization)
        if (attr(existing.categorization, "questiontype") == "PickAnyGrid") {
            text.names <- names(attr(text, "codeframe"))
            stacking <- ProcessAndStackDataForRegression(list(Y = text, X = existing.categorization), 
                                                            formula = NULL, 
                                                            interaction = NULL, 
                                                            subset = subset, 
                                                            weights = weights)
            colnames(stacking$data) <- vapply(stacking$data, 
                                            FUN = function (x) attr(x, "label"), 
                                            FUN.VALUE = character(1))
            text <- stacking$data[, 1]
            existing <- stacking$data[, 2:length(stacking$data)]
            colnames(existing) <- TidyLabels(colnames(existing))
            input.data <- list(text = text,
                                existing = existing,
                                subset = stacking$subset,
                                weights = stacking$weights,
                                inds = rownames(stacking$data))
        } else if (attr(existing.categorization, "questiontype") == "PickOneMulti") {
            # Reorder the variables in the categorization to match those
            # of the text.
            m <- match(colnames(existing.categorization), colnames(text))
            existing.categorization <- existing.categorization[, m]
            st <- stack(lapply(text, as.vector))
            inds <- paste0(rownames(st), ".", st[, "ind"])
            input.data <- list(text = st[, "values"],
                                existing = unlist(existing.categorization),
                                subset = if (length(subset) == 1) subset else rep(subset, times = n.text.vars),
                                weights = rep(weights, times = n.text.vars),
                                inds = inds)
        }
    }
    attr(input.data$text, "label") <- text.label
    input.data
}

# This function tries to modify the labels of a collection of
# text variables, or variables within a TextMulti set to match
# those of a PickOneMulti or PickAnyGrid.
# The reason for its existence is that there are common label
# tidying patterns that are applied to grids in Displayr
# which mean that text labels may not ititially match. That is
# labels on the code frames of grid questions commonly have
# aditional common prefix and suffix text removed.
prepareTextVariableLabelsForStackingWithGrids <- function(text, categorical.question) {
    categorical.codeframe.labels <- names(attr(categorical.question, "codeframe"))
    categorical.secondary.codeframe.labels <- names(attr(categorical.question, "secondarycodeframe"))
    text.labels <- colnames(text)

    if (!any(text.labels %in% categorical.codeframe.labels) && !any(text.labels %in% categorical.secondary.codeframe.labels)) {
        # No matches for the text labels anywhere
        # Remove common prefixes
        text.labels <- flipFormat::TidyLabels(text.labels)
    }

    if (!any(text.labels %in% categorical.codeframe.labels) && !any(text.labels %in% categorical.secondary.codeframe.labels)) {
        # Still No matches for the text labels anywhere
        # Remove common suffixes
        text.labels <- removeCommonSuffixText(text.labels)
    }

    if (!any(text.labels %in% categorical.codeframe.labels) && !any(text.labels %in% categorical.secondary.codeframe.labels)) {
        stop(paste0("Unable to match the labels from the Text variables to the labels of ", 
                    attr(categorical.question, "question"), 
                    ". Please modify the labels so that the text variables may be matched."))
    }

    # Update labels and return
    attr(text, "codeframe") <- fakeCodeFrame(text.labels)
    colnames(text) <- text.labels
    text
}

# Generates a vector which mimics the "codeframe"
# attribute for a Displayr variable/variable set.
# This is an integer vector named according to
# 'labels'.
fakeCodeFrame <- function(labels) {
    fake.codes <- 1:length(labels)
    names(fake.codes) <- labels
    fake.codes
}

# Function to identify and remove the common text
# from ends of the supplied labels. This is useful
# when trying to match labels to labels from
# variable sets in Displayr where redundant text
# is often removed from labels.
#' @importFrom stringi stri_reverse
removeCommonSuffixText <- function(labels) {
    reversed.labels <- stri_reverse(labels)
    common <- longestCommonPrefix(reversed.labels)
    pattern <- paste0("^", common)
    reversed.labels <- gsub(pattern, "", reversed.labels)
    labels <- stri_reverse(reversed.labels)
}

longestCommonPrefix <- function(x) {
    # sort the vector
    x <- sort(x)
    # split the first and last element by character
    chars <- strsplit(x[c(1, length(x))], "")
    # Find shortest string length
    n.shortest <- length(chars[[1L]])
    # search for the first not common element and so, get the last matching one
    if (length(chars[[2L]]) != n.shortest)
        chars[[2L]] <- chars[[2L]][1:n.shortest]
    common.prefix.length <- which.max(!chars[[1]] == chars[[2]]) - 1L
    if (common.prefix.length == 0L)
        return(character(0L))
    substr(x[1], 1L, common.prefix.length)
}

#' Unstack a data frame which corresponds to a categorization of mulitple
#' text variables in Displayr.
#' 
#' @param categorization The data frame to be unstacked. Its columns are factors
#' for a single-response categorization (Pick One - Multi) or binary numeric
#' vectors for a multiple-response categorization (Pick Any - Grid)
#' @param inds A character vector whose entries are of the form "<casenumber>.<variablename>"
#' such as would be produced by the \code{stack} function. Usually would be 
#' generated by when originally stacking the text data via \code{StackTextAndCategorization}
#' @export
UnstackCategorization <- function(categorization, inds) {
    # inds of the form "<casenumber>.<variablename>"
    # extract variable names
    split.inds <- vapply(inds, FUN = function (x) strsplit(x, "\\.")[[1]][2], FUN.VALUE = character(1))
    var.names <- unique(split.inds)
    single.response <- is.factor(categorization)
    unstacked <- lapply(var.names, 
                       FUN = function (v, inds, single.response) if (single.response) categorization[inds == v] else categorization[inds == v, ], 
                       inds = split.inds,
                       single.response = single.response)
    if (single.response) {
        names(unstacked) <- var.names
        unstacked <- as.data.frame(unstacked, optional = TRUE)
    } else {
        unstacked <- do.call(cbind, unstacked)
        colnames(unstacked) <- paste0(rep(var.names, each = NCOL(categorization)), ": ", colnames(unstacked))
    }                   
    unstacked  
}

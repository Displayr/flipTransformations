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
#' @importFrom verbs Sum
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
#' @importFrom verbs Sum
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
    dimensions.matching <- Sum(any.matches, remove.missing = FALSE)
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

updateStackedFormula <- function(data, formula)
{
    new.formula <- as.formula(paste0("Y ~ ", paste0("X", 1:(ncol(data) - 1), collapse = " + ")),
                              env = environment(formula))
    return(new.formula)
}
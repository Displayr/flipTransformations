#' \code{Factor}
#' @description Converts to a factor, but retains the label attribute.
#' @param x A vector of data, usually taking a small number of distinct values.
#' @param ... Further arguments passed to \code{factor}.
#' @details If the variable is already a factor, removes any empty levels
#' @export
Factor <- function(x, ...)
{
    result <- factor(x, ...)
    for (a in c("label", "name", "question"))
         attr(result, a) <- attr(x, a)
    result
}

#' \code{Ordered}
#' @description Converts to an ordered, but retains the label attribute.
#' @param x A vector of data, usually taking a small number of distinct values.
#' @param ... Further arguments passed to \code{factor}.
#' @details If the variable is already a factor, nothing is done to it.
#' @export
Ordered <- function(x, ...)
{
    result <- ordered(x, ...)
    for (a in c("label", "name", "question"))
         attr(result, a) <- attr(x, a)
    result
}

#' \code{Unclass}
#' @description Unclasses, and removes the levels attribute.
#' @param x An ordered factor.
#' @importFrom stats model.matrix
#' @param warn Show warning.
#' @importFrom flipFormat Labels
#' @export
Unclass <- function(x, warn = TRUE)
{
    result <- unclass(x)
    attr(result, "levels") <- NULL
    if (warn & is.null(attr(x, "InLoop")))
        warning(asNumericWarning(Labels(x, show.name = TRUE)))
    else
        attr(result, "Unclassed") <- Labels(x)
    result
}

#' \code{UnclassIfNecessary}
#' @description Unclasses a variable if it is a factor. Otherwise, returns x.
#' @param x A vector.
#' @param warn Show warning.
#' @return A vector
#' @export
UnclassIfNecessary <- function(x, warn = TRUE)
{
    if(is.factor(x))
        return(Unclass(x, warn));
    return(x);
}

asNumericWarning <- function(variables)
{
    paste("Data has been automatically converted from a categorical to numeric variable within R. Values are assigned in the order of the categories: 1, 2, 3...  To use alternative numeric values you should instead transform the data prior including it in this analysis (e.g., by changing its Question Type): ",
    paste0(variables, collapse = ", "))
}

#' \code{OrderedToNumeric}
#' @description Convert an ordered factor to a numeric vector.
#' @param x An ordered factor.
#' @importFrom stats model.matrix
#' @export
OrderedToNumeric <- function(x)
{
    return(Unclass(x))
}


#' \code{FactorToNumeric}
#' @description Convert a factor variable to a numeric vector (when the factor is ordered),
#' or a matrix of indicator variables (when the factor is not ordered).
#' @param x A factor or ordered factor.
#' @param binary Returns the factor as binary variables.
#' @param name The name of the variable.
#' @param remove.first Remove the first binary variable, if a binary variable is being created.
#' @importFrom flipFormat RemoveParentName
#' @export
FactorToNumeric <- function(x, binary = TRUE, name = RemoveParentName(deparse(substitute(x))), remove.first = TRUE)
{
    if (!binary)#(is.ordered(x))
        return(OrderedToNumeric(x))
    indicators <- FactorToIndicators(x, name)
    if (nrow(indicators) < length(x))
    {
        new.indicators <- matrix(NA, length(x), ncol(indicators))
        row.names <- as.numeric(dimnames(indicators)[[1]])
        colnames(new.indicators) <- colnames(indicators)
        new.indicators[row.names, ] <- as.matrix(indicators)
        new.indicators <- as.data.frame(new.indicators)
        for (i in 1:ncol(indicators))
            attr(new.indicators[,i], "label") <- attr(indicators[,i], "label")
        indicators <- as.data.frame(new.indicators)
    }
    if (remove.first)
        indicators <- indicators[, -1]
    return(indicators)
}

#' \code{FactorToIndicators}
#' @description Convert a factor variable to a matrix whose columns are binary variables
#' representing one of the levels from the factor variable.
#' @param variable The factor variable to convert.
#' @param name The name of the input variable.
#' @importFrom stats model.matrix
#' @importFrom flipFormat RemoveParentName
#' @export
FactorToIndicators <- function(variable, name = RemoveParentName(deparse(substitute(variable))))
{
    result <- stats::model.matrix( ~ variable - 1)
    levs <- levels(variable)
    colnames(result) <- paste0(name, ".", 1:nlevels(variable))
    result <- as.data.frame(result)
    label <- attr(variable, "label")
    if (!is.null(label))
    {
        labels <- paste0(label, ": ", levs)
        for (i in 1:nlevels(variable))
            if (!is.null(label))
                attr(result[, i], "label") <- labels[i]
    }
    result
}

#' \code{DichotomizeFactor} Converts a list of variable or data frames into a
#' data.frame.
#' @param variable A variable in a DataSet or data.frame.
#' @param cutoff The cutoff point to split the variable into.
#' @param warning If TRUE, raise a warning showing the new levels.
#' @param name An alternate name to show instead of the deparsed variable name.
#' @importFrom flipFormat RemoveParentName
#' @export
DichotomizeFactor <- function(variable, cutoff = 0.5, warning = FALSE, name = RemoveParentName(deparse(substitute(variable)))) {
    label <- attr(variable, "label")
    if (is.null(label))
        label <- name
    if (!is.factor(variable))
        variable <- factor(variable)
    if (nlevels(variable) == 1)
        stop(paste(deparse(substitute(variable)), "cannot be dichotimized as it only contains one level."))
    else if (nlevels(variable) == 2)
        return(variable)
    cumulative.probs <- cumsum(prop.table(table(variable)))
    cut.point <- match(TRUE, cumulative.probs > cutoff)
    if (cut.point == 1)
        stop(paste(name, "cannot be dichotimized (e.g., perhaps only has 1 value)."))
    new.factor <- factor(suppressWarnings(Unclass(variable)) >= cut.point)
    levels(new.factor) <- paste(c("<=", ">="), levels(variable)[c(cut.point - 1, cut.point )])
    if (warning)
        warning(paste(name, "has been dichotimized into", paste(levels(new.factor), collapse = " & ")))
    attr(new.factor, "label") <- paste(label, levels(new.factor)[2])
    new.factor
}


#' \code{CreatingFactorDependentVariableIfNecessary}
#' @description Turns a numeric outcome variable into a factor outcome variable in a data.frame if not already a factor.
#' @param formula A formula.
#' @param data A data.frame
#' @importFrom flipU OutcomeName
#' @export
CreatingFactorDependentVariableIfNecessary <- function(formula, data)
{
    outcome.name <- OutcomeName(formula)
    data[, outcome.name] <- Factor(data[[outcome.name]])
    data
}


#' \code{CreatingBinaryDependentVariableIfNecessary}
#' @description Dichotomizes the dependent variable in a data.frame if not already dichotomized.
#' @param formula A formula.
#' @param data A data.frame
#' @importFrom flipU OutcomeName
#' @export
CreatingBinaryDependentVariableIfNecessary <- function(formula, data)
{
    outcome.name <- OutcomeName(formula)
    data[, outcome.name] <- CreatingBinaryVariableIfNecessary(data, outcome.name)
    data
}

#' \code{CreatingBinaryVariableIfNecessary}
#' @description Dichotomizes a variable.
#' @param data A data.frame
#' @param name The name of the variable in the data.frame.
#' @export
CreatingBinaryVariableIfNecessary <- function(data, name)
{
    variable <- data[[name]]
    n.unique <- length(unique(variable))
    if (n.unique < 2)
        warning("The Outcome variable needs to contain two or more categories. It does not.")
    else
    {
        if(!is.factor(variable))
        {
            variable <- Factor(variable)
        }
        if (nlevels(variable) > 2)
            variable <- DichotomizeFactor(variable, warning = TRUE, name = name)
    }
    variable
}


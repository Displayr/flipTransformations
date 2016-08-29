#' \code{Factor}
#' @description Converts to a factor, but retains the label attribute.
#' @param x A vector of data, usually taking a small number of distinct values.
#' @param ... Further arguments passed to \code{factor}.
#' @details If the variable is already a factor, nothing is done to it.
#' @export
Factor <- function(x, ...)
{
    if (is.factor(x))
        return(x)
    result <- factor(x, ...)
    attr(result, "label") <- attr(x, "label")
    result
}

#' \code{Unclass}
#' @description Unclasses, and removes the levels attribute.
#' @param x An ordered factor.
#' @importFrom stats model.matrix
#' @export
Unclass <- function(x)
{
    result <- unclass(x)
    attr(result, "levels") <- NULL
    result
}

#' \code{OrderedToNumeric}
#' @description Convert an ordered factor to a numeric vector.
#' @param x An ordered factor.
#' @importFrom stats model.matrix
#' @export
OrderedToNumeric <- function(x)
{
    # if (is.ordered(x))
    # {
        return(Unclass(x))
    #}
    # return(model.matrix( ~ x - 1))
}

#' \code{UnclassIfNecessary}
#' @description Unclasses a variable if it is a factor. Otherwise, returns x.
#' @param x A vector.
#' @return A vector
#' @export
UnclassIfNecessary <- function(x)
{
    if(is.factor(x))
        return(Unclass(x));
    return(x);
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
        new.indicators[row.names, ] <- indicators
        indicators <- new.indicators
    }
    if (remove.first)
        return(indicators[, -1])
    return(indicators)
}

#' \code{FactorsToIndicators}
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
    colnames(result) <- paste0(name, ":", levels(variable))
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
    new.factor <- factor(Unclass(variable) >= cut.point)
    levels(new.factor) <- paste(c("<=", ">="), levels(variable)[c(cut.point - 1, cut.point )])
    if (warning)
        warning(paste(name, "has been dichotimized into", paste(levels(new.factor), collapse = " & ")))
    attr(new.factor, "label") <- paste(label, levels(new.factor)[2])
    new.factor
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
        if (n.unique > 2) {
            if(!is.factor(variable))
            {
                label <- attr(variable, "label")
                variable <- factor(variable)
                attr(variable, "label") <- label
            }
            if (nlevels(variable) > 2)
                variable <- DichotomizeFactor(variable, warning = TRUE, name = name)
        }
    }
    variable
}


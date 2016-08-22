#' \code{FactorsToIndicators}
#' @description Convert a factor variable to a matrix whose columns are binary variables
#' representing one of the levels from the factor variable.
#' @param variable The factor variable to convert.
#' @param variable.name The name of the input variable.
#' @export
FactorToIndicators <- function(variable, variable.name = deparse(substitute(variable)))
{
    result <- stats::model.matrix( ~ variable - 1)
    colnames(result) <- paste0(variable.name, ":", levels(variable))
    result
}

#' \code{OrderedToNumeric}
#' @description Convert an ordered factor to a numeric vector.
#' @param x An ordered factor.
#' @export
OrderedToNumeric <- function(x)
{
    if (is.ordered(x))
    {
        return(unclass(x))
    }
    return(stats::model.matrix( ~ x - 1))
}

#' \code{UnclassIfNecessary}
#' @description Unclasses a variable if it is a factor. Otherwise, returns x.
#' @param x A vector.
#' @return A vector
#' @export
UnclassIfNecessary <- function(x)
{
    if(is.factor(x))
        return(unclass(x));
    return(x);
}

#' \code{FactorToNumeric}
#' @description Convert a factor variable to a numeric vector (when the factor is ordered),
#' or a matrix of indicator variables (when the factor is not ordered).
#' @param x A factor or ordered factor.
#' @param variable.name The name of the variable.
#' @export
FactorToNumeric <- function(x, variable.name = deparse(substitute(x)))
{
    if (is.ordered(x))
    {
        return(OrderedToNumeric(x))
    }
    indicators <- FactorToIndicators(x, variable.name)
    if (nrow(indicators) < length(x))
    {
        new.indicators <- matrix(NA, length(x), ncol(indicators))
        row.names <- as.numeric(dimnames(indicators)[[1]])
        colnames(new.indicators) <- colnames(indicators)
        new.indicators[row.names, ] <- indicators
        return(new.indicators)
    }
    return(indicators)
}

#' \code{DichotomizeFactor} Converts a list of variable or data frames into a
#' data.frame.
#'
#' @param variable A variable in a DataSet or data.frame.
#' @param cutoff The cutoff point to split the variable into.
#' @param warning If TRUE, raise a warning showing the new levels.
#' @param variable.name An alternate name to show instead of the deparsed
#'   variable name.
#' @export
DichotomizeFactor <- function(variable, cutoff = 0.5, warning = FALSE, variable.name = deparse(substitute(variable))) {
    if (!is.factor(variable))
        variable <- factor(variable)
    if (nlevels(variable) == 1)
        stop(paste(deparse(substitute(variable)), "cannot be dichotimized as it only contains one level."))
    else if (nlevels(variable) == 2)
        return(variable)
    cumulative.probs <- cumsum(prop.table(table(variable)))
    cut.point <- match(TRUE, cumulative.probs > cutoff)
    if (cut.point == 1)
        stop(paste(variable.name, "cannot be dichotimized (e.g., perhaps only has 1 value)."))
    new.factor <- factor(unclass(variable) >= cut.point)
    levels(new.factor) <- paste0(c("<=", ">="), levels(variable)[c(cut.point - 1, cut.point )])
    if (warning)
        warning(paste(variable.name, "has been dichotimized into", paste(levels(new.factor), collapse = " & ")))
    attr(new.factor, "label") <- levels(new.factor)[2]
    new.factor
}



#' @export
CreatingBinaryDependentVariableIfNecessary <- function(formula, data)
{
    outcome.name <- flipU::OutcomeName(formula)
    data[, outcome.name] <- CreatingBinaryVariableIfNecessary(data, outcome.name)
    data
}

#' @export
CreatingBinaryVariableIfNecessary <- function(data, variable.name)
{
    variable <- data[[variable.name]]
    n.unique <- length(unique(variable))
    if (n.unique < 2)
        stopTooFewForBinary()
    else
    {
        if (n.unique > 2) {
            if(!is.factor(variable))
                variable <- factor(variable)
            if (nlevels(variable) > 2)
                variable <- DichotomizeFactor(variable, warning = TRUE, variable.name = variable.name)
        }
    }
    variable
}


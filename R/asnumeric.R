#' \code{AsNumeric}
#' @description Coerces a \code{\link{data.frame}} or \code{\link{vector}} that may contain
#' factors into a \code{\link{vector}} or \code{\link{data.frame}} that does not contain factors.
#' @param x The \code{\link{data.frame}} or \code{\link{vector}}.
#' @param binary If \code{TRUE}, unordered factors are represented as dummy variables.
#' Otherwise, they are represented as sequential integers.
#' @param name Used if \code{binary} is \code{TRUE} to construct variable names. This parameter is
#' ignored if x is a \code{\link{data.frame}}.
#' @param remove.first Remove the first binary variable.
#' @details Characters are treated as factors.
#' @importFrom flipFormat RemoveParentName
#' @export
AsNumeric <- function(x, binary = TRUE, name = NULL, remove.first = FALSE)
{
    UseMethod("AsNumeric")
}

#' @importFrom flipFormat RemoveParentName Names
#' @importFrom flipTime AsDateTime
#' @export
AsNumeric.default <- function(x, binary = TRUE, name = NULL, remove.first = FALSE)
{
    if (is.numeric(x))
        return(x)

    if (is.character(x))
    {
        x.tmp <- AsDateTime(x)
        if (!any(is.na(x.tmp)))
            x <- x.tmp
    }
    if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt"))
        return(as.numeric(x))
    if (is.null(name))
        name <- RemoveParentName(Names(x))
    if (is.character(x))
        x <- Factor(x)
    else if (!is.vector(x) & !is.factor(x))
        stop("'AsNumeric' is only applicable to vectors, factors, and data.frames.")
    if (!is.factor(x))
        return(x)
    FactorToNumeric(x, binary & !is.ordered(x), name = name, remove.first = remove.first)
}

#' @export
AsNumeric.data.frame <- function(x, binary = TRUE, name = NULL, remove.first = FALSE)
{
    new.x = ListToDataFrame(x, binary = binary, remove.first = remove.first)
    row.names(new.x) = row.names(x)
    new.x
}


#' \code{OneHot}
#'
#' Produces a numeric \code{\link{matrix}} of binary predictor variables and a numeric \code{\link{vector}}
#' of an outcome variable from an input \code{\link{data.frame}}.
#' With the exception of \code{outcome}, all \code{\link{factor}} variables of \code{data} are
#' converted to one binary column per factor level.  Numeric variables are unchanged.
#' @param data A \code{\link{data.frame}}.
#' @param outcome Optionally, the name of a variable in \code{data} to be converted to a numeric vector.
#'
#' @export
OneHot <- function(data, outcome = NULL)
{
    # convert predictor data to numeric matrix with factors converted to multiple binary columns
    X <- AsNumeric(data[, !names(data) %in% outcome, drop = FALSE])
    for (i in 1:ncol(X))
        if (!is.null(attr(X[, i], "label")))
            colnames(X)[i] <- attr(X[, i], "label")
    X <- as.matrix(apply(X, 2, as.numeric))

    # convert outcome variable to numeric vector (encoding from 0 to nlevels(outcome)-1)
    y <- NULL
    outcome.levels <- NULL
    if (!is.null(outcome) && outcome %in% names(data))
        if (is.factor(data[, outcome]))
        {
            outcome.levels <- levels(data[, outcome])
            y <- as.numeric(data[, outcome]) - 1
        }
        else
            y <- data[, outcome]

    return(list(X = X, y = y, outcome.levels = outcome.levels))
}



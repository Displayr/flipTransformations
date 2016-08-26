#' \code{AsNumeric}
#' @description Coerces a \code{\link{data.frame}} or \code{\link{vector}} that may contain
#' factors into a \code{\link{vector}} or \code{\link{data.frame}} that does not contain factors.
#' @param x The \code{\link{data.frame}} or \code{\link{vector}}.
#' @param binary If \code{TRUE}, unordered factors are represented as dummy variables.
#' Otherwise, they are represented as sequential integers.
#' @param name Used if \code{binary} is \code{TRUE} to construct variable names.
#' @details Characters are treated as factors.
#' @importFrom flipFormat RemoveParentName
#' @export
AsNumeric <- function(x, binary = TRUE, name = RemoveParentName(deparse(substitute(x))))
{
    UseMethod("AsNumeric")
}

#' @importFrom flipFormat RemoveParentName
#' @export
AsNumeric.default <- function(x, binary = TRUE, name = RemoveParentName(deparse(substitute(x))))
{
    if (is.character(x))
        x <- factor(x)
    else if (!is.vector(x) & !is.factor(x))
        stop("'AsNumeric' is only applicable to vectors, factors, and data.frames.")
    if (!is.factor(x))
        return(x)
    FactorToNumeric(x, binary & !is.ordered(x), name)
    # {
    #     x <- unclass(x)
    #     attr(x, "levels") <- NULL
    # }
    # else if (is.factor(x))
    #     x <- FactorToNumeric(x, name)
    #x
}

#' @export
AsNumeric.data.frame <- function(x, binary = TRUE)
{
    ListToDataFrame(x, binary = binary)
}




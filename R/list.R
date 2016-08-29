#' \code{ListToDataFrame}
#' @description Coerce a list of numeric or factor variables into a data frame.
#' @param list.of.variables A list containing the variables to combine. The
#'   elements of the list should be of class numeric, factor, or ordered factor.
#' @param binary If \code{TRUE}, unordered factors are represented as dummy variables.
#' Otherwise, they are represented as sequential integers.
#' @param remove.first Remove the first binary variable.
#' @export
ListToDataFrame <- function(list.of.variables, binary = TRUE, remove.first = FALSE)
{
    result <- NULL
    for (counter in seq(along = list.of.variables))
    {
        variable <- list.of.variables[[counter]]
        name <- names(list.of.variables)[counter]
        if (is.factor(variable) | is.character(variable))
            variable <- AsNumeric(variable, binary = binary, name = name, remove.first = remove.first)
        if (is.null(result))
        {
            result <- as.data.frame(variable)
        }
        else
        {
            result <- cbind(result, as.data.frame(variable))
        }

        if (is.null(ncol(variable)))
        {
            colnames(result)[ncol(result)] <- names(list.of.variables)[counter]#variable.name
        }
    }
    return (result)
}

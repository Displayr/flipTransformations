#' \code{ListToDataFrame}
#' @description Coerce a list of numeric or factor variables into a data frame.
#' @param list.of.variables A list containing the variables to combine. The
#'   elements of the list should be of class numeric, factor, or ordered factor.
#' @param binary If \code{TRUE}, unordered factors are represented as dummy variables.
#' Otherwise, they are represented as sequential integers.
#' @export
ListToDataFrame <- function(list.of.variables, binary = TRUE)
{
    result <- NULL
    for (counter in seq(along = list.of.variables))
    {
        variable <- list.of.variables[[counter]]
        name <- names(list.of.variables)[counter]
        if (is.factor(variable) | is.character(variable))
            variable <- AsNumeric(variable, binary, name)
#
#         if (is.null(variable.name) || variable.name == "")
#         {
#             variable.name <- counter
#         }
#         transformed.variable <- AsNumeric(variables)
#         if (is.character(variable))
#         {
#             stop("Variable '", variable.name,
#                 "' is a Text variable. It needs to be converted to numeric data if to be used in cluster analysis.")
#         }
#         if (is.data.frame(variable))
#         {
#             transformed.variable <- ListToDataFrame(variable, coerce.to.numeric)
#             colnames(transformed.variable) <- paste0(variable.name, ":", colnames(transformed.variable))
#         }
#         else
#         {
#             if (coerce.to.numeric && is.factor(variable) & !is.ordered(variable))
#             {
#                 transformed.variable <- FactorToNumeric(variable, variable.name)#,                 variable.name = nms[counter])
#             }
#             else
#             {
#                 transformed.variable <- variable
#             }
#         }

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

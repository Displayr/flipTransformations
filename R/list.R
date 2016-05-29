
#' \code{ListToDataFrame}
#' @description Coerce a list of numeric or factor variables into a data frame.
#' @param list.of.variables A list containing the variables to combine. The
#'   elements of the list should be of class numeric, factor, or ordered factor.
#' @param coerce.to.numeric A boolean value specifying whether or not factor
#'   variables should be coerced to numeric.
#' @export
ListToDataFrame <- function(list.of.variables, coerce.to.numeric = FALSE)
{
    result <- NULL
    for (counter in seq(along = list.of.variables))
    {
        variable <- list.of.variables[[counter]]
        variable.name <- names(list.of.variables)[counter]

        if (is.null(variable.name) || variable.name == "")
        {
            variable.name <- counter
        }

        if (is.character(variable))
        {
            stop("Variable '", variable.name,
                "' is a Text variable. It needs to be converted to numeric data if to be used in cluster analysis.")
        }

        if (is.data.frame(variable))
        {
            transformed.variable <- ListToDataFrame(variable, coerce.to.numeric)
            colnames(transformed.variable) <- paste0(variable.name, ":", colnames(transformed.variable))
        }
        else
        {
            if (coerce.to.numeric && is.factor(variable))
            {
                transformed.variable <- FactorToNumeric(variable, variable.name)#,                 variable.name = nms[counter])
            }
            else
            {
                transformed.variable <- variable
            }
        }

        if (is.null(result))
        {
            result <- as.data.frame(transformed.variable)
        }
        else
        {
            result <- cbind(result, as.data.frame(transformed.variable))
        }

        if (is.null(ncol(transformed.variable)))
        {
            colnames(result)[ncol(result)] <- variable.name
        }
    }

    return (result)
}

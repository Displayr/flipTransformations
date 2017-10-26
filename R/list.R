#' \code{AsNumericList}
#' @description Coerce a list of numeric or factor variables into a list where the elements of the list are numeric.
#' @param x A list containing the variables to combine. The
#'   elements of the list should be of class numeric, factor, or ordered factor.
#' @param binary If \code{TRUE}, unordered factors are represented as dummy variables.
#' Otherwise, they are represented as sequential integers.
#' @param remove.first Remove the first binary variable.
#' @param return.data.frame Returns as a \code{\link{data.frame}}.
#' @return A \code{\link{data.frame}} or \code{\link{list}}.
asNumericList <- function(x, binary = TRUE, remove.first = FALSE, return.data.frame = TRUE)
{
    result <- NULL
    unclassed <- list()
    nms <- names(x)
    if (!return.data.frame)
    {
        result <- vector("list", length(x))
        names(result) <- names(x)
    }
    for (counter in seq(along = x))
    {
        variable <- x[[counter]]
        if (is.null(attr(variable, "name")))
            attr(variable, "name") <- nms[counter]
        attr(variable, "InLoop") <- TRUE
        if (is.factor(variable) | is.character(variable))
        {
            variable <- AsNumeric(variable, binary = binary, name = attr(variable, "name"), remove.first = remove.first)
            uc <- attr(variable, "Unclassed")
            if (!is.null(uc))
            {
                unclassed <- c(unclassed, uc)
                attr(result, "Unclassed") <- NULL
            }
        }
        attr(variable, "InLoop") <- NULL
        if (return.data.frame)
        {
            if (is.null(result))
            {
                result <- as.data.frame(variable)
            }
            else
            {
                result <- cbind(result, as.data.frame(variable))
            }

            if (is.null(ncol(variable))) # Numeric variables (i.e., not exploded factors).
            {
                colnames(result)[ncol(result)] <- names(x)[counter]
            }
        }
        else
        {
            result[[counter]] <- variable
        }
    }
    if (length(unclassed) > 0)
        warning(asNumericWarning(unclassed))
    return (result)
}

#' \code{QuestionListToDataFrame}
#' @description Combine a list of questions into a data frame.
#' @param list.of.questions A list containing the questions to combine.
#' @param names.to.remove A vector of column names to exclude.
#' @export
QuestionListToDataFrame <- function(list.of.questions, names.to.remove = c("NET", "Total", "SUM"))
{
    result <- NULL
    for (c in seq(list.of.questions))
    {
        q <- list.of.questions[[c]]
        df <- if (is.data.frame(q))
        {
            question.df <- q[, !(tolower(names(q)) %in% tolower(names.to.remove))]
            question.df
        }
        else # Single variable questions
        {
            var.df <- as.data.frame(q)
            names(var.df) <- attr(q, "question")
            var.df
        }

        if (is.null(result))
            result <- df
        else
            result <- cbind(result, df)
    }
    result
}


#' \code{SplitVectorToList}
#' @description Splits a vector \code{values} by unique values of another vector, \code{groups}.
#' @param values A \code{\link{vector}} to be split.
#' @param groups A \code{\link{vector}} which determines how \code{vakuees} is split.
#' @return A \code{\link{list}}.
#' @export
SplitVectorToList <- function(values, groups)
{
    tapply(values, groups, c)
}

#' \code{AsDataFrame}
#'
#' @description Converts input data to a numeric \code{\link{data.frame}}.
#' @param input.data Either a \code{\link{data.frame}}, a \code{\link{list}} of
#'   \code{\link{data.frame}}s and/or \code{\link{vector}}s, or a \code{\link{matrix}}.
#' @param use.names Whether to use names in place of labels, used only for a
#'   \code{\link{list}} of \code{\link{vector}}s.
#' @param ignore.columns A list of names of columns to ignore. When \code{input.data}
#'   is a \code{\link{matrix}}, rows are also ignored.
#' @param categorical.as.binary Whether to convert factors to dummy binary variables,
#'   or else their levels are converted to integers.
#' @param remove.first Whether to remove the first binary variable.
#' @importFrom flipFormat ExtractCommonPrefix Labels Names
#' @importFrom flipU RemoveAt
#' @export
AsDataFrame <- function(input.data,
                        use.names = FALSE,
                        ignore.columns = "",
                        categorical.as.binary = FALSE,
                        remove.first = FALSE)
{
    dat <- if (is.matrix(input.data))
        {
            mat <- RemoveAt(input.data, at = list(ignore.columns, ignore.columns), split = ",")
            AsNumeric(data.frame(mat, check.names = FALSE))
        }
        else if (is.data.frame(input.data) || !any(sapply(input.data, is.data.frame))) # coerce list of variables to data.frame
        {
            input.data <- data.frame(input.data, check.names = FALSE)
            colnames(input.data) <- Names(input.data)
            var.dat <- AsNumeric(ProcessQVariables(input.data),
                                 binary = categorical.as.binary,
                                 remove.first = remove.first)
            # Changing names to labels.
            if (!use.names)
                names(var.dat) <- ExtractCommonPrefix(Labels(var.dat))$shortened.labels
            var.dat

        }
        else if (is.list(input.data)) # list of questions and potentially variables
        {
            names.to.remove <- trimws(unlist(strsplit(ignore.columns, split = ",")))
            qns.dat <- AsNumeric(QuestionListToDataFrame(input.data, names.to.remove = names.to.remove),
                                 binary = categorical.as.binary)
        }
        else
            stop("input.data must be a data.frame, list of data.frames/vectors or a matrix")

    return(dat)
}

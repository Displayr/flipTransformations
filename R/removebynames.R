#' Remove indices from a vector by name
#'
#' Remove elements from a vector while preserving attributes
#' @param x named vector, data.frame, or list to remove entries from
#' @param rnames either 1) a character, vector giving entry names to remove; 2) a single
#' string containing comma or semi-colon separated names to remove, 3) a list where each
#' element is either 1) or 2)
#' @return \code{x} with entries specified in \code{rnames} removed
#' they will be combined using \code{\link{union}}
#' @note An error is thrown if removal would result in an empty vector
#' @details Will also work for data.frame \code{x}
#' @importFrom flipU CopyAttributes
#' @examples
#' x <- c(a = 1, b = 2, c = 3, d = 4, e = 5)
#' RemoveByName(x, "a")
#' RemoveByName(x, "a; b")
#' RemoveByName(x, list(c("a", "b"), " c,  d"))
#' @export
RemoveByName <- function(x, rnames)
{
    xnames <- names(x)
    if (is.null(xnames) || !length(rnames) || !nzchar(rnames))
        return(x)

    rnames <- if(is.list(rnames))
                  unique(unlist(lapply(rnames, sepNames)))  # Reduce("union", lapply(rnames, sepNames))
              else
                  sepNames(rnames)

    ## deal with possible comma or semi-colon separated names
    rnames <- sepNames(rnames)

    if (all(xnames %in% rnames))
        stop("Removing entries gives empty vector")

    CopyAttributes(x[setdiff(xnames, rnames)], x)
}

#' @noRd
sepNames <- function(rnames)
{
    sep <- if (length(rnames) == 1)
               gsub("[^;,]", "", rnames)
    if (!is.null(sep) && nzchar(sep))
        rnames <- trimws(strsplit(rnames, sep)[[1L]])
    rnames
}

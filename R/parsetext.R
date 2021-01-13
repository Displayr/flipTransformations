#' @ Parse text into the appropriate type
#'
#' Convert text into the specified type. This can be used with \code{form.textBox}.
#' The return value is a single value. Use \link{TextAsVector} first if multiple
#' entries separated by a comma (or other deliminator) is expected.
#' @param x The input character string to be converted.
#' @param same.as A variable of the same class as the desired output. If this is supplied then \code{class(same.as)} will be used instead of \code{type}.
#' @param type The desired type of the output. If set to "Automatic"
#' (and \code{same.as} is not supplied) then \code{numeric}, \code{POSIXct},
#' \code{Date}, and \code{character} will be tried sequentially. However,
#' if a specific type is specified, then \code{NA} will be returned if
#' it cannot be appropriately parsed. Note that \code{factors} will be
#' treated as characters
#' @importFrom flipTime AsDateTime AsDate
#' @export
ParseText <- function(x, same.as = NULL, type = "Automatic")
{
    if (!is.null(same.as))
    {
        if (is.numeric(same.as))
            type <- "numeric"
        else if (is.character(same.as) || is.factor(same.as))
            type <- "character"
        else if (inherits(same.as, "POSIXct"))
            type <- "POSIXct"
        else if (inherits(same.as, "Date"))
            type <- "Date"
        else
            class(same.as)[1]
    }
    type <- tolower(type)
    tmp.out <- NA

    if (type %in% c("automatic", "numeric"))
        tmp.out <- asNumericVector(x)
    if (!is.na(tmp.out) || type %in% c("numeric"))
        return(tmp.out)

    if (type %in% c("automatic", "posixct", "posixt"))
        tmp.out <- AsDateTime(x, on.parse.failure = "silent")
    if (!is.na(tmp.out) || type %in% c("posixct", "posixt"))
        return(tmp.out)

    if (type %in% c("automatic", "date"))
        tmp.out <- AsDate(x, on.parse.failure = "silent")
    if (!is.na(tmp.out) || type %in% c("date"))
        return(tmp.out)

    return(x)
}

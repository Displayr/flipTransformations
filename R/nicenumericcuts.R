#' Create factor variables from numeric variables using one of a variety of
#' methods.
#' 
#' @param input.data The \code{\link{data.frame}} or \code{\link{vector}} 
#'      containing the data to be categorized. Data should be numeric.
#' @param method A string describing which method is to be used to categorize
#'      the data. Options are \code{tidy.intervals}, \code{percentiles},
#'      \code{equal.width}, and \code{custom}.
#' @param num.categories An integer sepcifying the number of categories for
#'      the new factor. This does not apply for \code{percentiles} or for
#'      \code{Custom} because in those cases the number of categories is
#'      imlpied by the \code{percents} and \code{custom.breaks} argumens
#'      respectively.
#' @param right A boolean value specifying that when determining breaks
#'      in the data, the close side of the inerval should be on the right 
#'      (i.e on the larger end of the interval).
#' @param round.input.data A boolean value specifying whether the input
#'      data should be rounded before categorization.
#' @param decimals An integer which determines to how many decimals the
#       data should be rounded when \code{round.input.data} is \code{TRUE}.
#' @param label.decimals An integer value which determines how many
#'      decimal places should be included in formatted labels.
#' @param open.ends A boolean value which determines if labels at the upper
#'      and lower ends of the range should be open-ended or should contain
#'      both end points of the interval. E.g. if \code{TRUE} then interval
#'      (0, 17] would be "Less than 18", otherwise it would be "0 to 17".
#' @param label.style A character indicating the style of labels to use
#'      for the new factor levels. Options are \code{tidy.labels}, 
#'      \code{inequality.notation}, \code{interval.notation}, and
#'      \code{percentiles}, with the latter option only being applicable
#'      if \code{method} is also \code{percentiles}.
#' @param number.prefix A character to be appended before numbers in new
#'      factor labels.
#' @param number.suffix A character to be after before numbers in new
#'      factor labels.
#' @param open.bottom.string A character indicating text to be placed at the
#'      beginning of open-ended labels at the start of the range for open
#'      intervals.
#' @param closed.bottom.string A character indicating text to be placed at the
#'      end of open-ended labels at the start of the range for closed
#'      intervals.
#' @param open.top.string A character indicating text to be placed at the
#'      beginning of open-ended labels at the end of the range for open
#'      intervals.
#' @param closed.top.string A character indicating text to be placed at the
#'      end of open-ended labels at the end of the range for closed
#'      intervals.
#' @param equal.intervals.start A numeric value indicating the start of the
#'      range when using \code{method} of \code{equal.width}.
#' @param equal.intervals.end A numeric value indicating the end of the
#'      range when using \code{method} of \code{equal.width}.
#' @param custom.breaks A character containing a comma-seprated list of 
#'      numeric values to be used as custom break points when the 
#'      \code{method} is \code{Custom}.
#' @param percents A single numeric value, or a character containing a comma
#'      -separated list of numeric values to be used when the \code{method}
#'      of \code{percentiles} is used. Values should be between 0 and 100.
#' @param quantile.type An interger between 1 and 9 to be passed to
#'      \code{quantile} which determines the algorithm for creating quantiles.
#' @param factors.use.labels A logical value indicating whether numeric
#'      information should be extracted from factor labels. If \code{FALSE}
#'      the function will instead try to extract the underlying numerice
#'      values from Q/Displayr.
#' @param grouping.mark A character to be used as the thousands-grouping
#'      mark when inferring numeric information from factor labels. 
#' @param decimals.mark A character to be used as the decimals-grouping
#'      mark when inferring numeric information from factor labels. 
#'
#' @importFrom flipU ConvertCommaSeparatedStringToVector
#' @importFrom plyr mapvalues
#' @importFrom stats var quantile
#' @export
NiceNumericCuts <- function(input.data,
                           method = "tidy.intervals",
                           num.categories = 2,
                           right = TRUE,
                           round.input.data = FALSE,
                           decimals = 1,
                           label.decimals = 1,
                           open.ends = TRUE,
                           label.style = "tidy.labels",
                           number.prefix = "",
                           number.suffix = "",
                           open.bottom.string = "Less than ",
                           closed.bottom.string = " and below",
                           open.top.string = "More than ",
                           closed.top.string = " and over",
                           equal.intervals.start = 0,
                           equal.intervals.end = 100,
                           custom.breaks = NULL,
                           percents = NULL,
                           quantile.type = 7,
                           factors.use.labels = TRUE,
                           grouping.mark = ",", 
                           decimals.mark = "."
                           ) { 
    
    # Protect against empty strings supplied
    # by Displayr UI
    if(nzchar(equal.intervals.start)) {
        equal.intervals.start = as.numeric(equal.intervals.start)
    } else {
        equal.intervals.start = NULL
    }

    if(nzchar(equal.intervals.end)) {
        equal.intervals.end = as.numeric(equal.intervals.end)    
    } else {
        equal.intervals.end = NULL
    }


    if (method == "custom" && is.null(custom.breaks)) {
        stop("No custom breakpoints have been entered for the custom intervals.")
    }

    if (method == "percentile" && is.null(custom.breaks)) {
        stop("No percentages have been entered for the percentiles.")
    }


    # Convert factors if user wants to use numbers present in labels
    get.data.as.numeric <- function(x,
                                    factors.use.labels = TRUE,
                                    grouping.mark = ",", 
                                    decimals.mark = "\\." ) {
        
        # Coerce characters to factors for ease of mapping
        if (is.character(x)) {
            x = factor(x)
            factors.use.labels = TRUE
        }


        if (is.factor(x)) {
            if (factors.use.labels) {
                # Scrape numeric information from labels, check
                # the numbers are appropriate, and then map
                # to numeric.
                label.chunks = lapply(levels(x), 
                          FUN = extractRangeInformationFromLabel, 
                          grouping.mark = grouping.mark, 
                          decimals.mark = decimals.mark)

                # Obtain numeric values from each label and count them
                numbers.from.labels = lapply(label.chunks, 
                                             FUN = function (x) return(x$numbers))

                number.of.numbers = vapply(numbers.from.labels, 
                                           FUN = length, 
                                           FUN.VALUE = numeric(1))

                if (any(number.of.numbers == 0)) {
                    cant.use.labels = paste0("\'", levels(x)[number.of.numbers == 0], "\'")
                    cant.use.text = cant.use.labels[1]
                    if (length(cant.use.labels) > 1) {
                        cant.use.text = paste0(cant.use.text[1:2], collapse = " and ")
                    }
                    warning("Some data labels do not contain numbers and will not be combined.",
                            "These labels include ", cant.use.text)
                }

                if (any(number.of.numbers > 1)) {
                    cant.use.labels = paste0("\'", levels(x)[number.of.numbers > 1], "\'")
                    cant.use.text = cant.use.labels[1]
                    if (length(cant.use.labels) > 1) {
                        cant.use.text = paste0(cant.use.text[1:2], collapse = " and ")
                    }
                    warning("Some data labels contain more than one numeric value and ",
                        "will be not be combined. These labels include ", cant.use.text,
                        ". If your data labels contain ranges of values, change ",
                        "\'Labels contain\' to \'Ranges of values\'.")
                }


                new.values = rep(NA, length(x))
                for (j in 1L:length(levels(x))) {
                    lev = levels(x)[j]
                    if (number.of.numbers[j] == 1) {
                        new.values[x == lev] = numbers.from.labels[[j]][1]   
                    }
                }
                x.raw = new.values
            } else {
                # Use attribute info from Q/Displayr
                q.levels = attr(x, "levels")
                q.source.values = attr(x, "sourcevalues")
                if (!is.null(q.levels) && !is.null(q.source.values)) {
                    if (length(q.levels) < length(q.source.values)) {
                        warning("Some categories in ", attr(x, "label"), 
                            " have been combined and the average value for the combined ",
                            "categories has been used. Consider reverting any combined categories.")
                    }
                }
                x.raw = AsNumeric(x, binary = FALSE)
            }
        } else if (is.numeric(x)) {
            x.raw = x
        } else {
            stop("Cannot transform data of type ", class(x)[1])
        }
        return(x.raw)
    }

    input.data = as.data.frame(input.data)
    original.data = input.data
    classes = lapply(input.data, FUN = class)
    numerics = vapply(classes, FUN = function (x) return("numeric" %in% x), FUN.VALUE = logical(1))
    input.data = as.data.frame(lapply(input.data, 
                                      FUN = get.data.as.numeric,
                                      factors.use.labels = factors.use.labels,
                                      grouping.mark = grouping.mark, 
                                      decimals.mark = decimals.mark))

    

    # factors = vapply(classes, FUN = function (x) return("factor" %in% x), FUN.VALUE = logical(1))
    # chararacters = vapply(classes, FUN = function (x) return("character" %in% x), FUN.VALUE = logical(1))

    # if (is.numeric(input[[1]] && !all(numerics))) {
    #     stop("All Variables should be the same type (e.g. numeric).")
    # }

    # if (!all(numerics)) {
    #     stop("Nice numeric cuts requires numeric input data.")
    # }



    # Obtain the full set of raw values from the inputs
    raw.data = unlist(input.data)
    raw.data = raw.data[!is.na(raw.data)]

    # User may find tidier intervals if they round first
    if (round.input.data) {
        raw.data = round(raw.data, decimals)
        input.data = as.data.frame(lapply(input.data, 
                                          FUN = round, 
                                          digits = decimals))
    }

    # Data which are all integers are handled differently later
    all.integers = all(raw.data %% 1 == 0)
    min.val = min(raw.data)
    max.val = max(raw.data)
    uniques = unique(raw.data)
    n.unique = length(uniques)

    

    # Handle each of the allowed methods for forming categories

    if (method == "percentiles") {
        percents = ConvertCommaSeparatedStringToVector(percents)
        percents = as.numeric(percents)
        if (any(is.na(percents))) {
            stop("There is a problem with the entries in the Percentages field. ",
                 "Please enter a single number or a comma-seperated list of numbers.")
        }
        if (length(percents) == 1) {
            if (100 %% percents != 0) {
                stop("The entered percentage ", 
                     percents, 
                     " does not divide evenly into 100%. ",
                     "Try setting \'Number of categories\' to 5, 10, 20, etc")
            } else {
                percents = seq(0, 100, by = percents)
            } 
        } else {
            percents = c(0, percents, 100)
            percents = sort(percents)
        }
        percents = unique(percents)
        percents = percents / 100
        qq = quantile(raw.data, probs = percents, 
                      na.rm = TRUE, include.lowest = TRUE, 
                      type = quantile.type)
        
        if (length(which(duplicated(qq))) > 0) {
            warning("Some percentiles are empty in the range you have specified and will not be shown");
        }
        
        # Remove duplicate cut points
        qq = qq[!duplicated(qq, fromLast = TRUE)]

        if (label.style == "percentiles") {
            lower.labels = names(qq)[-length(qq)]
            upper.labels = names(qq)[-1]

            # Percentiles always start at 0%
            # and end at 100%
            lower.labels[1] = "0%"
            upper.labels[length(upper.labels)] != "100%"

            # Paste inequalities into labels depending on whether the closed interval
            # boundary is on the left or the right.
            if (right) {
                lower.labels[-1] = paste0("> ", lower.labels[-1])
            } else {
                upper.labels[-length(upper.labels)] = paste0("< ", upper.labels[-length(upper.labels)])
            }


            percentile.labels = paste0(lower.labels, " to ", upper.labels)
            new.factors = lapply(input.data, 
                                 FUN = cut, 
                                 breaks = qq, 
                                 right = right, 
                                 include.lowest = TRUE, 
                                 labels = percentile.labels)
        } else {
            new.factors = lapply(input.data, 
                                 FUN = cut, 
                                 breaks = qq, 
                                 right = right, 
                                 include.lowest = TRUE, 
                                 labels = NULL, 
                                 dig.lab = 4)
            new.labels = tidyIntervalLabels(levels(new.factors[[1]]),
                                            raw.data = raw.data, 
                                            decimals = label.decimals, 
                                            style = label.style,
                                            integer.data = all.integers,
                                            open.ended = open.ends, 
                                            prefix = number.prefix, 
                                            suffix = number.suffix,
                                            open.bottom.string = open.bottom.string,
                                            closed.bottom.string = closed.bottom.string,
                                            open.top.string = open.top.string,
                                            closed.top.string = closed.top.string,
                                            grouping.mark = grouping.mark, 
                                            decimals.mark = decimals.mark)
            new.factors = lapply(new.factors, 
                                 FUN = mapvalues, 
                                 from = levels(new.factors[[1]]), 
                                 to = new.labels)
        }
        new.factors = as.data.frame(new.factors)    

    } else {
        if (method == "equal.width") {
            if (is.null(equal.intervals.start)) {
                warning("The start point for the combined categories has been set to the minimum value of ", 
                    formatC(min.val, digits = 2, format = "f", big.mark = grouping.mark, decimal.mark = decimals.mark), 
                    ". To change this, enter a value in \'Start point\'.")
                equal.intervals.start = min.val        
            }
            if (is.null(equal.intervals.end)) {
                warning("The end point for the combined categories has been set to the maximum value of ", 
                    formatC(max.val, digits = 2, format = "f", big.mark = grouping.mark, decimal.mark = decimals.mark), 
                    ". To change this, enter a value in \'End point\'.")
                equal.intervals.end = max.val        
            }
            if (min.val < equal.intervals.start) {
                n.lower = length(which(raw.data < equal.intervals.start))
                warning(n.lower, " values in the data are less than the start point of ", equal.intervals.start, 
                ". These will be assigned a missing value. Consider setting the Start point setting to ", 
                floor(min.val), " or lower.")
            }
            if (max.val > equal.intervals.end) {
                n.higher = length(which(raw.data > equal.intervals.end))
                warning(n.higher, " values in the data are greater than the end point of ", equal.intervals.end, 
                ". These will be assigned a missing value. Consider setting the End point setting to ", 
                ceiling(max.val), " or greater.")
            } 
            start = equal.intervals.start
            end = equal.intervals.end
            if (nchar(start) == 0) { 
                start = min.val
            } else {
                start = as.numeric(start)
            }

            if (nchar(end) == 0) {
                end = max.val
            } else {
                end = as.numeric(end)
            }
            
            cuts = seq(start, end, length.out = num.categories + 1)
        } else if (method == "custom") {
            if (is.character(custom.breaks)) {
                cuts = as.numeric(ConvertCommaSeparatedStringToVector(custom.breaks))    
            } else {
                cuts = custom.breaks
            }
            if (any(is.na(cuts))) {
                stop("Some of the break points could not be interpreted as numbers. ",
                    "Please ensure Category Boundaries contains a comma-seperated list of numbers.")
            }
            start = min(cuts)
            end = max(cuts)
            if (min.val < start) {
                n.lower = length(which(raw.data < start))
                warning(n.lower, " values in the data are less than the start point of ", start, 
                ". These will be assigned a missing value. Consider setting the samllest value in \'Break points\' to ", floor(min.val))
            }
            if (max.val > end) {
                n.higher = length(which(raw.data > end))
                warning(n.higher, " values in the data are greater than the end point of ", end, 
                ". These will be assigned a missing value. Consider setting the largest value in \'Break points\' to ", ceiling(max.val))
            } 
        } else if (method == "tidy.intervals") {

            cuts = pretty(raw.data, n = num.categories)
            if (length(cuts) != num.categories + 1) {
                warning("Could not find a pretty solution with exactly ", num.categories, 
                    " categories. Pretty solutions break the interval into multiples of ",
                    "2, 5, 10, and some combinations are not guaranteed. Consider changing ",
                    "the \'Target number of categories\' option.")
            }

        } else {
            stop("Method ", method, " is not recognized. Use one of: tidy.intervals, equal.width, percentiles, or custom.")
        }
        new.factors = as.data.frame(lapply(input.data, 
                                           FUN = cut, 
                                           breaks = cuts, 
                                           right = right, 
                                           include.lowest = TRUE, 
                                           labels = NULL, 
                                           dig.lab = 4))
        new.labels = tidyIntervalLabels(levels(new.factors[[1]]),
                                            raw.data = raw.data, 
                                            decimals = label.decimals, 
                                            style = label.style, 
                                            integer.data = all.integers,
                                            open.ended = open.ends,
                                            prefix = number.prefix, 
                                            suffix = number.suffix,
                                            open.bottom.string = open.bottom.string,
                                            closed.bottom.string = closed.bottom.string,
                                            open.top.string = open.top.string,
                                            closed.top.string = closed.top.string,
                                            grouping.mark = grouping.mark, 
                                            decimals.mark = decimals.mark)                                        
        new.factors = as.data.frame(lapply(new.factors, 
                                           FUN = mapvalues, 
                                           from = levels(new.factors[[1]]), 
                                           to = new.labels))
    }

    # Fill in missing values for non-numeric labels when input data is
    # factor/character
    if (any(!numerics)) {
        for (j in 1L:ncol(original.data)) {
            if (!is.numeric(original.data[, j])) {
                missings = is.na(new.factors[, j])
                level.list = levels(new.factors[, j])
                if (is.factor(original.data[, j])) {
                    level.list = c(level.list, as.character(levels(droplevels(original.data[missings, j]))))
                } else {
                    level.list = c(level.list, unique(as.character(original.data[missings, j])))    
                }
                levels(new.factors[, j]) = level.list
                new.factors[missings, j] = as.character(original.data[missings, j])
            }

        }    
    }

    
    colnames(new.factors) = colnames(original.data)
    return(new.factors)
}


# Loop through a given set of interval labels
# produced by the cut() function,
# of the form [x , y], (x, y], [x,y), (x,y)
# extract the numbers and create new labels
# based on requested style.
tidyIntervalLabels <- function(labels, 
                               raw.data, 
                               decimals = NULL, 
                               style = "tidy.labels", 
                               integer.data = FALSE, 
                               prefix = "", 
                               suffix = "", 
                               open.ended = FALSE,
                               open.bottom.string = "Less than ",
                               closed.bottom.string = " and below",
                               open.top.string = "More than ",
                               closed.top.string = " and over",
                               grouping.mark = ",", 
                               decimals.mark = "."
                               ) {
    new.labels = labels
    for (k in 1:length(labels)) {
        is.first = k == 1
        is.last = k == length(labels)
        previous.val = NULL
        next.val = NULL
        if(!is.first) {
            previous.val = getValuesInInterval(labels[k-1])[2]
        }
        if(!is.last) {
            next.val =  getValuesInInterval(labels[k+1])[1]
        }

        new.labels[k] = tidyIntervalLabel(labels[k], 
                                          raw.data = raw.data,
                                          decimals = decimals,
                                          style = style,
                                          integer.data = integer.data,
                                          prefix = prefix,
                                          suffix = suffix,
                                          open.ended = open.ended,
                                          first.label = is.first,
                                          last.label = is.last,
                                          open.bottom.string = open.bottom.string,
                                          closed.bottom.string = closed.bottom.string,
                                          open.top.string = open.top.string,
                                          closed.top.string = closed.top.string,
                                          previous.val = previous.val,
                                          next.val = next.val,
                                          grouping.mark = grouping.mark, 
                                          decimals.mark = decimals.mark)
    }
    return(new.labels)
}

# Extract upper and lower values from interval labels
# produced by the cut() function (see above).
getValuesInInterval <- function(label) {
    bounds = strsplit(label, ",")[[1]]
    bounds = as.numeric(gsub("\\(|\\)|\\[|\\]", "", bounds))
    return(bounds)
}

# Tidy an individual interval label accorsing to the 
# desired style, taking into account whether the
# label is first or last in the sequence, and which
# values directly precede or follow this label.
tidyIntervalLabel <- function(label, 
                              raw.data, 
                              decimals = 2, 
                              style = "tidy.labels", 
                              integer.data = FALSE, 
                              prefix = "", 
                              suffix = "", 
                              open.ended = FALSE,
                              first.label = FALSE,
                              last.label = FALSE,
                              open.bottom.string = "Less than ",
                              closed.bottom.string = " and below",
                              open.top.string = "More than ",
                              closed.top.string = " and over",
                              grouping.mark = ",", 
                              decimals.mark = ".",
                              previous.val = NULL,
                              next.val = NULL) {

    raw.data = sort(raw.data)

    # Determine upper and lower boundary of interval
    bounds = strsplit(label, ",")[[1]]
    lower = bounds[1]
    lower.num = as.numeric(gsub("(", "", gsub("[", "", lower, fixed = TRUE), fixed = TRUE))
    upper = bounds[2]
    upper.num = as.numeric(gsub("]", "", gsub(")", "", upper, fixed = TRUE), fixed = TRUE))

    # Determine whether boundar intervals are open or closed
    upper.is.open = grepl(")", upper, fixed = TRUE)
    lower.is.open = grepl("(", lower, fixed = TRUE)  

    # Tidy up data values when using
    # "tidy.labels" option including
    # special handling for integer data
    # and identifying next largest / next smallest
    # value in data for open intervals
    if (style == "tidy.labels") {
        if (lower.is.open) {
            if (integer.data) {
                lower.num = floor(lower.num) + 1
                lower.is.open = FALSE
            } else {
                if (last.label && open.ended) {
                    lower.num = previous.val
                } else {
                    lower.num = min(raw.data[raw.data > lower.num])
                } 
            }     
        }
        if (upper.is.open) {
            if (integer.data) {
                upper.num = floor(upper.num) - 1
                upper.is.open = FALSE
            } else {
                if (first.label && open.ended) {
                    upper.num = next.val
                } else {
                    upper.num = max(raw.data[raw.data < upper.num])
                }      
            }
        }
    }

    # Format desired number of decimal places
    lower.num = formatC(lower.num, digits = decimals, format = "f", big.mark = grouping.mark, decimal.mark = decimals.mark) 
    upper.num = formatC(upper.num, digits = decimals, format = "f", big.mark = grouping.mark, decimal.mark = decimals.mark)


    # Add prefix and suffix
    lower.string = paste0(prefix, lower.num, suffix)
    upper.string = paste0(prefix, upper.num, suffix)

    # Paste values as labels based on desired style
    if (style == "tidy.labels") {
        if (upper.num == lower.num) {
            new.label = upper.string
        } else if (first.label && open.ended) {
            new.label = ifelse(upper.is.open, 
                        paste0(open.bottom.string, upper.string), 
                        paste0(upper.string, closed.bottom.string))
        } else if (last.label && open.ended) {
            new.label = ifelse(lower.is.open, 
                               paste0(open.top.string, lower.string), 
                               paste0(lower.string, closed.top.string))
        } else {
            new.label = paste0(lower.string, " to ", upper.string)
        }
            
    } else if (style == "inequality.notation") {
        # Only add inequality signs when boundary is open
        # except for first and last label if user wants
        # open-ended labels
        lower.ineq = ifelse(lower.is.open, "> ", "")
        upper.ineq = ifelse(upper.is.open, "< ", "")
        if (first.label && open.ended) {
            upper.ineq = ifelse(upper.is.open, "< ", "<=")
            new.label = paste0(upper.ineq, upper.string)
        } else if (last.label && open.ended) {
            lower.ineq = ifelse(lower.is.open, "> ", ">=")
            new.label = paste0(lower.ineq, lower.string)
        } else {
            new.label = ifelse(upper.num == lower.num, 
                               lower.string, 
                               paste0(lower.ineq, lower.string, " to ", upper.ineq, upper.string))
        }
    } else if (style == "interval.notation") {
        lower.symbol = ifelse(lower.is.open, "(", "[")
        upper.symbol = ifelse(upper.is.open, ")", "]")
        new.label = ifelse(upper.num == lower.num, 
                           paste0("[", lower.string, "]"), 
                           paste0(lower.symbol, lower.string, "," , 
                           upper.string, upper.symbol))
    } else {
        stop("Label style ", style, "not recognized. Use one of: tidy.labels, inequality.notation, interval.notation")
    }

    return(new.label)

}




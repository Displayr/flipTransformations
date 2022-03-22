
#' Automatically combine factor levels whose levels represent ranges.
#'
#' Create new factor variables from existing factors whose
#' labels contain numeric ranges, like 18 to 24 or $2,000 to $3,000,
# 'by merging adjacent ranges. The new factors have distributions
#' which are as even as possible.
#'
#' @param input.data A \code{data.frame} or \code{vector} containing factors
#'      to be merged.
#' @param num.categories An integer indicating the number of categories
#'      in the new factors, excluding any levels that do not contain range
#'      information.
#' @param method A character indicating how the merging is to be performed.
#'      The available options are \code{"even.proportions"} and 
#'      \code{even.ranges}.
#' @param grouping.mark A character indicating how thousands are grouped within
#'      the labels of the input data. Usally a comma, but sometimes a period
#'      for example in European locales.
#' @param decimals.mark A character indicating how decimal values are separated
#'      in the input data. Usually a period.
#' @param lower.bound A numeric value which indicates the lower bound of the
#'      numeric ranges when the number is not explicitly included in the 
#'      level labels. For example, when the lowest level is "Under 18", you
#'      may wish to specify that the lowest possible value in this level
#'      is 12, 15, 0, etc. Default value of "" means that an estimate will
#'      be used based on the width of the next highest range.
#' @param upper.bound As above, for the higher end of the range.
#'
#'
#' @return A data frame containing the new factor variables as columns.

#' @importFrom plyr mapvalues
#' @export
MergeRangeCategories <- function(input.data, 
                                num.categories = 2, 
                                method = c("even.proportions", "even.ranges"),
                                grouping.mark = ",", 
                                decimals.mark = ".", 
                                lower.bound = "", 
                                upper.bound = "") {

    method = match.arg(method)

    if (is.data.frame(input.data)) {
        raw.data <- unlist(input.data)
    } else {
        raw.data <- input.data
    }


    if (num.categories >= length(unique(raw.data))) {
        warning("The number of categories requested is", num.categories,
                " but there are only ", length(unique(raw.data)), 
                " different values in the data. No merging has been done.")
        return(input.data)
    }

    # Compute counts.
    counts <- table(raw.data)

    # Try to pull apart labels into numbers and non-numbers
    # Extract information about non-numeric data surrounding the numbers in each
    # label which represents a pair or range. This includes the delimiter between
    # the numbers, as well as prefixes and suffixes that are common to both
    # numeric values within the label
    label.chunks <- lapply(X = names(counts), 
                          FUN = extractRangeInformationFromLabel, 
                          grouping.mark = grouping.mark, 
                          decimals.mark = decimals.mark)


    # Obtain numeric values from each label and count them
    numbers.from.labels <- lapply(label.chunks, 
                                 FUN = function (x) return(x$numbers))

    number.of.numbers <- vapply(numbers.from.labels, 
                               FUN = length, 
                               FUN.VALUE = numeric(1))

    # Using the counts, determine if this is a candiate for merging as ranges
    pairs <- number.of.numbers == 2
    singles <- number.of.numbers == 1
    non.number <- number.of.numbers == 0
    is.ranges <- (length(which(singles)) < 3 && length(which(pairs)) > 0)
    if (!is.ranges) {
        stop("Could not detect sufficient numeric data to merge ranges. When combining ",
            "data whose labels represent ranges, all labels should contain pairs of ",
            "numbers except for those labels at the higher and lower end of the range. ",
            "In this case there are ", length(which(singles)), " labels which do not ",
            "pairs of numbers.")
    }




    # Begin collating information about the labels
    label.data <- data.frame(original.labels = names(counts), 
                            new.labels = names(counts), 
                            counts = as.vector(counts), 
                            n.numbers = number.of.numbers)
    label.data$label.character <- vapply(label.data$original.labels, 
                                        FUN = identifyClosedOrOpenBoundariesFromText, 
                                        FUN.VALUE = character(1))
    label.data$label.character[pairs & label.data$label.character == "ambiguous"] <- "closed"

    if (sum(non.number) > 0) {
        no.number.labels <- label.data[non.number, "original.labels"]
        no.number.label.text <- paste0("\'", no.number.labels[1], "\'")
        if (length(no.number.labels) > 1) {
            no.number.label.text <- paste0(paste0("\'", no.number.labels[1:2], "\'"), collapse = " and ")
        }
    }


    # Labelling is ambiguous if we cannot determine whether those labels
    # which contain a single numeric value refer to an open or closed interval.
    # This is used later on when deciding how to form new labels for
    # merged categories
    labels.are.ambiguous <- any(label.data$label.character[singles] == "ambiguous")


    
    non.numeric.label.elements <- label.chunks[pairs]

    delimiter.candidates <- vapply(non.numeric.label.elements, 
                                  FUN = function(x) return(x$delimiter), 
                                  FUN.VALUE = character(1))
    prefix.candidates <- lapply(non.numeric.label.elements, 
                               FUN = function(x) return(x$prefix.candidate))
    suffix.candidates <- lapply(non.numeric.label.elements, 
                               FUN = function(x) return(x$suffix.candidate))

    label.data$delimiter[pairs] <- delimiter.candidates


    all.delimiters.equal <- length(unique(delimiter.candidates)) == 1
    all.prefixes.equal <- length(unique(prefix.candidates)) == 1
    all.suffixes.equal <- length(unique(suffix.candidates)) == 1

    if (all.delimiters.equal) {
        delimiter <- delimiter.candidates[1]
    } else {
        warning("Could not find a common pattern to all ranges. It is generally best for each pair of numbers ",
            "to be separated by a common delimiter, like \' - \' or \' to \'.")
        unique.delimiter.candidates <- unique(delimiter.candidates)
        delimiter.freqs <- vapply(unique.delimiter.candidates, 
                                 FUN = function(x) return(length(which(delimiter.candidates == x))), 
                                 FUN.VALUE = numeric(1))
        delimiter.candidates <- delimiter.candidates[order(delimiter.freqs, decreasing = TRUE)]
        delimiter <- delimiter.candidates[1]

        if (nchar(delimiter) == 0) {
            delimiter <- " to "
        }

    }


    get.element <- function(x, index) {
        if(length(x) < index) {
            return (NA)
        } else {
            return (x[index])
        }
    }

    # Organize the first and second numeric value from each range.
    first.value <- vapply(numbers.from.labels, 
                         FUN = get.element, 
                         FUN.VALUE = numeric(1), 
                         index = 1)
    second.value <- vapply(numbers.from.labels, 
                          FUN = get.element, 
                          FUN.VALUE = numeric(1), 
                          index = 2)
    label.data$first.value <- first.value
    label.data$second.value <- second.value

    # Disambiguate first.value when sorting to ensure that 
    # open-ended labels are sorted correctly. Otherwise causes
    # a problem when same number appears twice in labels and
    # when the categories are initially out of order. For example
    # if the first two labels are "18 to 24" and "Under 18".
    sort.values = label.data$first.value
    for (j in seq_len(nrow(label.data))) {
        if (grepl("lower", label.data[j, "label.character"])) {
            sort.values[j] = sort.values[j] - 0.00001
        } else if (grepl("upper", label.data[j, "label.character"])) {
            sort.values[j] = sort.values[j] + 0.00001
        }
    }

    label.data <- label.data[order(sort.values), ]

    # If we sort labels by the first value, and the second value is out of order
    # it implies inconsistency.
    if (is.unsorted(label.data$second.value, na.rm = TRUE)) {
        stop("The numbers in the data labels suggest that the ranges are inconsistent or overlapping. ",
            "This can happen if labels refer to different units of measurement, for example some ",
            "referring to months and others referring to years. Please ensure your labels contain consistent units.")
    }

    

    # Discard labels without numbers. These cannot be merged
    # with labels that contain numbers as they are
    # not of the same kind.
    label.data <- label.data[label.data$n.numbers > 0, ]


    # Fill in ranges for first and last labels
    # with user-supplied values if supplied
    if (is.na(label.data[1, "second.value"])) {
        label.data[1, "second.value"] <- label.data[1, "first.value"]
        if (nzchar(lower.bound)) {
            label.data[1, "first.value"] <- as.numeric(lower.bound)
        } else {
            lower.interp <- label.data[1, "second.value"] - (label.data[2, "second.value"] - label.data[2, "first.value"])
            if (is.na(lower.interp) | lower.interp < 0) {
                lower.interp <- 0
            }
            label.data[1, "first.value"] <- lower.interp
        }
    }

    if (is.na(label.data[nrow(label.data), "second.value"])) {
        if (nzchar(upper.bound)) { 
            label.data[nrow(label.data), "second.value"] <- as.numeric(upper.bound)
        } else {
            last.index = nrow(label.data) - 1
            upper.interp = label.data[nrow(label.data), "first.value"] + (label.data[last.index, "second.value"] - label.data[last.index, "first.value"])   
            label.data[nrow(label.data), "second.value"] <- upper.interp
        }
    }

    # Compute ranges for open-ended labels at top and bottom
    label.data$range <- label.data[, "second.value"] - label.data[, "first.value"]

    label.data$ID <- letters[1:nrow(label.data)]

    merge.data <- label.data[, c("ID", "original.labels", "counts", "range")]
    if (method[1] == "even.proportions") {
        values = merge.data$counts
    } else if (method[1] == "even.ranges") {
        no.range <- is.na(merge.data$range)
        
        if (any(no.range)) {
            no.range.labels <- merge.data[no.range, "original.labels"]
            no.range.label.text <- paste0("\'", no.range.labels[1], "\'")
            if (length(no.range.labels) > 1) {
                no.range.label.text <- paste0(paste0("\'", no.range.labels[1:2], "\'"), collapse = " and ")
            }
            
        }
        merge.data <- merge.data[!no.range, ]
        values <- merge.data$range

    } 

    if (length(values) <= num.categories) {
        warning("There is insufficient numeric information in the data labels to ",
            "determine how to combine these categories into ", num.categories,
            " categories. No merging has been done.")
        return(input.data)
    }

    merge.solution <- mostEvenOrdinalMerge(merge.data$ID, 
                                          values = values, 
                                          target.number = num.categories)

    
    for (j in seq_along(merge.solution)) {
        current.ids <- strsplit(merge.solution[j], "\\.")[[1]]
        matches <- label.data$ID %in% current.ids
        if (length(which(matches)) > 1) {
            if (labels.are.ambiguous) {
                new.label <- paste0(label.data$original.labels[matches], collapse = " + ")
            } else {
                current.label.data <- label.data[matches, ]
                n.matches <- nrow(current.label.data)
                if (all(current.label.data$n.numbers == 2)) {
                    first <- strsplit(current.label.data[1, "original.labels"], 
                                     current.label.data[1, "delimiter"], 
                                     fixed = TRUE)[[1]][1]
                    last <- strsplit(current.label.data[n.matches, "original.labels"], 
                                    current.label.data[n.matches, "delimiter"], 
                                    fixed = TRUE)[[1]][2]
                    new.label <- paste0(first, delimiter, last)
                } else if (current.label.data[1, "n.numbers"] == 1 && current.label.data[n.matches, "n.numbers"] == 2) {
                    last <- strsplit(current.label.data[n.matches, "original.labels"], 
                                    current.label.data[n.matches, "delimiter"], 
                                    fixed = TRUE)[[1]][2]
                    new.label <- paste0("Up to ", last)
                } else if (current.label.data[1, "n.numbers"] == 2 && current.label.data[n.matches, "n.numbers"] == 1) {
                    first <- strsplit(current.label.data[1, "original.labels"], 
                                     current.label.data[1, "delimiter"], 
                                     fixed = TRUE)[[1]][1]
                    new.label <- paste0(first, " and over")
                }
            }
            label.data[matches, "new.labels"] <- new.label
        }
            
    }

    if (is.data.frame(input.data)) {
        new.factor <- lapply(input.data, 
                            FUN = mapvalues, 
                            from = label.data$original.labels, 
                            to = label.data$new.labels)
        new.factor <- as.data.frame(new.factor)
    } else {
        new.factor <- mapvalues(input.data, 
                               from = label.data$original.labels, 
                               to = label.data$new.labels)
    }
    
    if (is.data.frame(input.data)) {
        colnames(new.factor) <- colnames(input.data)
    }   

    return(new.factor)
}






#' @importFrom utils combn
allPossibleOrdinalMerges <- function(num.categories, target.categories, IDs) {
    # All combinations of breaks
    my.combs <- t(combn(num.categories-1,target.categories-1))
    get.partition <- function(v, IDs) {
        ends <- c(v, length(IDs))
        starts <- c(1, (v + 1))
        result <- character(0)
        for (j in 1L:length(starts)) {
            result <- c(result, paste0(IDs[starts[j] : ends[j]], collapse = "."))
        }
        return(result)
    }
    merges <- t(apply(my.combs, 1, FUN = get.partition, IDs = IDs))
    return(merges) 
}

mostEvenOrdinalMerge <- function(IDs, values, target.number) {
    merge.data <- data.frame(IDs, values)
    all.merges <- allPossibleOrdinalMerges(nrow(merge.data), 
                                      target.number, 
                                      merge.data$ID)
    distributions <- t(apply(all.merges, 1, FUN = getMergedCounts, data = merge.data))
    stdevs <- apply(distributions, 1, FUN = sd)
    return(all.merges[which.min(stdevs), ])
}

getMergedCounts <- function(merges, data) {
    dist <- numeric(length(merges))
    for (j in 1L:length(merges)) {
        current.ids <- strsplit(merges[j], "\\.")[[1]]
        dist[j] <- sum(data[data$ID %in% current.ids, "values"])
    }
    return(dist)
}


#' @importFrom stringr str_count fixed
#' @importFrom Hmisc escapeRegex
extractRangeInformationFromLabel <- function(x, grouping.mark = ",", decimals.mark = ".") {
        grouping.mark <- escapeRegex(grouping.mark)
        decimals.mark <- escapeRegex(decimals.mark)

        x <- gsub(grouping.mark, "", x)
        x <- gsub(decimals.mark, ".", x)

        numbers <- as.numeric(unlist(regmatches(x, gregexpr("-?[[:digit:]]+\\.?[[:digit:]]*", x, perl=TRUE))))
        non.numbers <- unlist(regmatches(x, gregexpr("-?[[:digit:]]+\\.?[[:digit:]]*", x, perl=TRUE), invert = TRUE))

        # Handle edge case where numbers separated by hyphen 
        # with no space to first digit. For example "18-24"
        # or "18 -24". We are assuming that range labels always
        # describe ranges beginning with a lower number and ending
        # with the larger number. If not, all bets are off.
        if (length(numbers) > 1 && any(numbers < 0) && is.unsorted((numbers))) {
            numbers <- as.numeric(unlist(regmatches(x, gregexpr("[[:digit:]]+\\.?[[:digit:]]*", x, perl=TRUE))))
            non.numbers <- unlist(regmatches(x, gregexpr("[[:digit:]]+\\.?[[:digit:]]*", x, perl=TRUE), invert = TRUE))
            # non.numbers = non.numbers[nzchar(non.numbers)]
        }


        if (length(non.numbers[nzchar(non.numbers)]) == 0) {
            # No non-numeric info found, which will happen
            # if there is a single number only in the label
            return(list(numbers = numbers,
                non.numbers = "", 
                prefix.candidate = "", 
                suffix.candidate = "", 
                delimiter = ""))    
        }

        # At this point there should be N numbers, N + 2 non-numbers
        # and at least one non-number is not empty.

        prefix.candidate <- non.numbers[1]
        suffix.candidate <- non.numbers[length(non.numbers)]

        prefix.matches <- 0
        suffix.matches <- 0

        if (nchar(prefix.candidate) > 0) {        
            prefix.matches <- str_count(x, fixed(prefix.candidate))
        }
        if (nchar(suffix.candidate) > 0) {
            suffix.matches <- str_count(x, fixed(suffix.candidate))
        }

        delimiter.candidates <- non.numbers[-c(1, length(non.numbers))]

        delimiter <- delimiter.candidates[1]

        if (nchar(prefix.candidate) > 0) {
            delimiter <- gsub(prefix.candidate, "", delimiter, fixed = TRUE)    
        }
        if (nchar(suffix.candidate) > 0) {
            delimiter <- gsub(suffix.candidate, "", delimiter, fixed = TRUE)    
        }
        
        chunks <- list(numbers = numbers,
                    non.numbers = non.numbers[nzchar(non.numbers)], 
                    prefix.candidate = prefix.candidate, 
                    suffix.candidate = suffix.candidate, 
                    delimiter = delimiter)
        return(chunks)    
}

identifyClosedOrOpenBoundariesFromText <- function(label) {
    
    global.closed.phrases <- c("or equal")
    
    lower.boundary.open.phrases <- c("less than", "under", "fewer than", "lower than", "below", "<", "smaller than", "younger than")
    lower.boundary.closed.phrases <- c(paste0(c("and ", "or "), rep(c("less", "under", "lower", "below", "smaller", "younger", "fewer"), each = 2)), c( "up to", "<="))

    upper.boundary.open.phrases <- c("greater than", "over", "more than", "above", ">", "larger than", "older than", "bigger than")
    upper.boundary.closed.phrases <- c(paste0(c("and ", "or "), rep(c("above", "greater", "more", "over", "older", "bigger", "larger"), each = 2)), ">=")

    lower.label <- tolower(label)

    any.match <- function(target, check.phrases) {
        matches <- vapply(check.phrases, FUN = grepl, FUN.VALUE = logical(1), x = target, fixed = TRUE)
        return(any(matches))
    }

    contains.global.closed <- any.match(lower.label, global.closed.phrases)
    contains.lower.open <- any.match(lower.label, lower.boundary.open.phrases)
    contains.lower.closed <- any.match(lower.label, lower.boundary.closed.phrases)
    contains.upper.open <- any.match(lower.label, upper.boundary.open.phrases)
    contains.upper.closed <- any.match(lower.label, upper.boundary.closed.phrases)

    if (contains.lower.open && ! (contains.global.closed || contains.lower.closed) ) {
        return("lower boundary open")
    } else if (contains.lower.closed || (contains.lower.open && contains.global.closed )) {
        return("lower boundary closed")
    } else if (contains.upper.open && ! (contains.global.closed || contains.upper.closed)) {
        return("upper boundary open")
    } else if (contains.upper.closed || (contains.upper.open && contains.global.closed )) {
        return("upper boundary closed")
    }

    return("ambiguous") 

}

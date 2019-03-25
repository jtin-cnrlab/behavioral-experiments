# within_3SD.R
# Jessica Tin
# 25 Mar 2019
#
# Defines within_3SD() for identifying which values in a vector are within three
# standard deviations of the vector mean. Values outside of the 3 SDs are replaced
# with NA. By default, the vector is first normalized by taking the log of all values.
# If any values in the vector are NA to begin with, they are returned as NA.
#
# within_3SD(vals, lognorm)
#     args: vals  A numeric vector, the values to check
#           lognorm (optional)  A boolean, whether or not to log-normalize first
#           warn (optional)  A boolean, whether or not to print warnings about NA
#                            and non-positive values
#  returns: A numeric vector, identical to the input, but with NAs replacing any
#           value more than three standard deviations away from the vector mean
#      ex.: within_3SD(df$rt)

within_3SD <- function(vals, lognorm = TRUE, warn = FALSE) {

    # warn about NA values
    if (any(is.na(vals)) && warn) cat("Warning:", sum(is.na(vals)), "NA values\n")

    # remove any non-positive values
    if (any(vals[!is.na(vals)] <= 0)) {
        if (warn) cat("Warning: removing", sum(vals[!is.na(vals)] < 0), "negative
                       value(s) and", sum(vals[!is.na(vals)] == 0), "zero value(s).\n")
        vals <- vals[vals > 0]
    }

    # unless specified to use original values, log-normalize the values first
    norm_vals <- if (lognorm) log(vals) else vals

    return(ifelse(abs(norm_vals - mean(norm_vals, na.rm = TRUE)) <= 3*sd(norm_vals, na.rm = TRUE), vals, NA))
}

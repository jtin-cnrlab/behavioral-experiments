# within_3SD.R
# Jessica Tin
# 3 Jan 2019
#
# Defines within_3SD() for identifying which values in a vector are within three
# standard deviations of the vector mean. Values outside of the 3 SDs are replaced
# with NA. By default, the vector is first normalized by taking the log of all values.
#
# within_3SD()
#     args: A numeric vector, and (optionally) whether or not to log-normalize first
#  returns: A numeric vector, the same as the input, except with NAs for each
#           value more than three standard deviations away from the vector mean
#      ex.: within_3SD(df$rt)

within_3SD <- function(vals, lognorm = TRUE) {
    # remove any non-positive values
    if (any(vals <= 0)) {
        cat("Warning: removing", sum(vals < 0), "negative value(s) and", sum(vals == 0),
            "zero value(s).\n")
        vals <- vals[vals > 0]
    }

    # unless specified to use original values, log-normalize the values first
    norm_vals <- if (lognorm) log(vals) else vals

    return(ifelse(abs(norm_vals - mean(norm_vals)) <= 3*sd(norm_vals), vals, NA))
}

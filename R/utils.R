unnest_df = function(data) {
    # Iterate over cols, check if data frame. If so, unnest. Otherwise receive a list.
    lists = lapply(data, function(x) {
        if (is.data.frame(x)) {
            list_vals = unnest_df(x)
            return(as.data.frame(list_vals))
        } else {
            return(x)
        }
    })
    return(as.data.frame(lists))
}


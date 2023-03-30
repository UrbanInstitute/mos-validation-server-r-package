#' Specify a tabular analysis to submit to the validation server
#'
#' @param data Data frame object
#' @param table_name Arbitrary name for the analysis
#' @param stat Summary statistic(s) to calculate (e.g. mean, median, sum, sd, var, min, max, n, n_distinct)
#' @param var Variable(s) to summarize
#' @param by Optional variable(s) to group by
#' @param na.rm Optional NA behavior (defaults to FALSE)
#'
#' @return A data frame formatted for the validation server
#'
#' @importFrom magrittr %>%
#' @export
get_table_output <- function(data = NULL,
                             table_name = NULL,
                             stat = NULL,
                             var = NULL,
                             by = NULL,
                             na.rm = FALSE,
                             validate = TRUE) {

    if (validate) {
        validate_table_args(data, table_name, stat, var, by)
    }

    stat_fn <- setNames(vector("list", length(stat)), stat)
    for (s in stat) {
        if (s == "n") {
            stat_fn[[s]] <- rlang::as_closure(~ dplyr::n())
        } else {
            stat_fn[[s]] <- rlang::as_closure(s)
        }
    }
    output_ <- data %>%
        dplyr::group_by(across(all_of(by))) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = all_of(var),
                .fns = stat_fn,
                .names = "{.col}||{.fn}",
                na.rm = na.rm
            ),
            n = dplyr::n(),
            .groups = "drop"
        )

    output <- output_ %>%
        tidyr::pivot_longer(!c(by, n), names_to = "varstat", values_to = "value") %>%
        tidyr::separate(varstat, into = c("var", "statistic"), sep = "\\|\\|") %>%
        dplyr::select(by, var, statistic, value, n) %>%
        dplyr::mutate(analysis_type = "table",
                      analysis_name = table_name)

    return(output)
}

#' Validate get_table_output() arguments with basic error handling
validate_table_args <- function(data, table_name, stat, var, by, stat_fn) {
    # Validate `data`
    if (is.null(data)) {
        stop('`data` argument must be specified.\n',
             'To use the full, untransformed confidential dataset, set `data = conf_data`.',
             call. = FALSE)
    }

    # Validate `table_name`
    if (is.null(table_name)) {
        stop('`table_name` argument must be specified.\n',
             'This will be the name for this analysis in the validation server output.',
             call. = FALSE)
    }

    # Validate `stat`
    if (is.null(stat)) {
        stop('`stat` argument must be specified.\n',
             'This will be the summary statistic(s) for this analysis.',
             call. = FALSE)
    }
    valid_stats <- c("mean", "median", "sum", "sd", "var", "min", "max", "n", "n_distinct")
    invalid_stats <- setdiff(stat, valid_stats)
    if (length(invalid_stats) > 0) {
        stop('`stat` argument is invalid.\nInvalid stat(s) specified: ',
             paste(invalid_stats, collapse = ', '),
             '\nValid stats are: ',
             paste(valid_stats, collapse = ', '),
             call. = FALSE)
    }

    # Validate `var`
    if (is.null(var)) {
        stop('`var` argument must be specified.\n',
             call. = FALSE)
    }
    invalid_var_cols <- setdiff(var, names(data))
    if (length(invalid_var_cols) > 0) {
        stop('`var` argument is invalid.\nVariable(s) not in data: ',
             paste(invalid_var_cols, collapse = ', '),
             call. = FALSE)
    }

    # Validate `by`
    invalid_by_cols <- setdiff(by, names(data))
    if (length(invalid_by_cols) > 0) {
        stop('`by` argument is invalid.\nVariable(s) not in data: ',
             paste(invalid_by_cols, collapse = ', '),
             call. = FALSE)
    }
}

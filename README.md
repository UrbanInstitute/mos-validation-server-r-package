
<!-- README.md is generated from README.Rmd. Please edit that file -->

# validationserver

This R package lets users submit analyses to Urban Institute’s
validation server. A valid R script must:

1.  Include a main function called `run_analysis()` that takes a
    `conf_data` argument with the confidential data.
2.  Use the `get_table_output()` and/or `get_model_output()` functions
    to define analyses.
3.  Use the `submit_output()` function to specify all analyses to run.

Otherwise, the R script can include arbitrary code to process and
analyze the underlying data. The full R script will be executed, so you
can define helper functions outside of the main `run_analysis()`
function.

## Basic Example

This is a basic example of a script with a linear regression and summary
table using the `penguins` dataset from the [`palmerpenguins` R
package](https://allisonhorst.github.io/palmerpenguins/):

``` r
library(dplyr)
library(palmerpenguins)

run_analysis <- function(conf_data) {
    # Arbitrary code -----------------------------------------------------------
    transformed_data <- conf_data %>%
        filter(year == 2007) %>%
        mutate(bill_ratio = bill_length_mm / bill_depth_mm)
    
    # Specify analyses -----------------------------------------------------------
    # Example regression 
    example_fit <- lm(body_mass_g ~ bill_ratio, data = transformed_data)
    example_model <- get_model_output(
        fit = example_fit, 
        model_name = "Example Model"
    )
    
    # Example table 
    example_table <- get_table_output(
        data = transformed_data,
        table_name = "Example Table",
        stat = c("mean", "sd"),
        var = "bill_ratio",
        by = "species"
    )
    
    # Submit analyses ------------------------------------------------------------
    submit_output(example_table, example_model)
}
```

## Model Examples

The `get_model_output()` function requires two arguments:

- `model_name` is an arbitrary name for the analysis. This name will
  show up in your output and should be unique to each analysis in an R
  script.
- `fit` is a fit model object. Only `lm` and `glm` objects are currently
  supported.

Here’s an example of a script with a linear model (fit using `lm`) and
binomial model (fit using `glm`):

``` r
library(dplyr)
library(palmerpenguins)

run_analysis <- function(conf_data) {
    # Arbitrary code -----------------------------------------------------------
    transformed_data <- conf_data %>%
        mutate(mass_above_4kg = case_when(body_mass_g > 4000 ~ 1, 
                                          TRUE ~ 0))

    # Specify analyses -----------------------------------------------------------
    # Example linear model 
    lm_fit <- lm(body_mass_g ~ bill_length_mm, data = transformed_data)
    lm_example <- get_model_output(
        fit = lm_fit, 
        model_name = "Example Linear Model"
    )

    # Example binomial model 
    glm_fit <- glm(mass_above_4kg ~ bill_length_mm, family = binomial, data = transformed_data)
    glm_example <- get_model_output(
        fit = glm_fit, 
        model_name = "Example Binomial Model"
    )

    # Submit analyses ------------------------------------------------------------
    submit_output(lm_example, glm_example)
}
```

## Table Examples

The `get_table_output()` function includes the following arguments:

- `data` is the data frame object to create the summary table from. To
  use the full data, specify `data = conf_data`.
- `table_name` is an arbitrary name for the analysis. This name will
  show up in your output and should be unique to each analysis in an R
  script.
- `stat` is the summary statistic (or set of summary statistics) to
  calculate. This can include `mean`, `median`, `sum`, `sd`, `var`,
  `min`, `max`, `n` or `n_distinct`.
- `var` is the variable (or set of variables) to summarize.
- `by` (optional) is the variable (or set of variables) to group by.
- `na.rm` (optional) specifies explicit NA behavior (defaults to
  `TRUE`).

Here’s an example of a script with (1) a single summary statistic, and
(2) a table demonstrating how to pass multiple values into the `stat`,
`var`, and `by` arguments.

``` r
library(dplyr)
library(palmerpenguins)

run_analysis <- function(conf_data) {
    # Arbitrary code -----------------------------------------------------------
    transformed_data <- conf_data %>%
        filter(year == 2007) %>%
        mutate(bill_ratio = bill_length_mm / bill_depth_mm)
    
    # Specify output -----------------------------------------------------------
    # Example summary statistic 
    table1 <- get_table_output(
        data = transformed_data, 
        table_name = "Example Table 1",
        stat = "mean", 
        var = "bill_ratio"
    )
    
    # Example table with multiple stat/var/by values  
    table2 <- get_table_output(
        data = transformed_data, 
        table_name = "Example Table 2",
        stat = c("mean", "sd"),
        var = c("bill_length_mm", "bill_ratio"),
        by = c("species", "island")
    )
    
    # Submit output ------------------------------------------------------------
    submit_output(table1, table2)
}
```

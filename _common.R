### ---- Set knitr options ----

# set knitr options
knitr::opts_chunk$set(
  cache = FALSE, # cache output
  error = TRUE, # continue code evaluation even if there is an error, in which case the error messages will print
  comment = '', # remove ## prefix for printed text output
  message = FALSE, # suppress printing of messages in the output document
  warning = FALSE, # suppress printing of warnings in the output document
  fig.pos = "hold" # hold figure position so it stays in the order it should be in relative to the surrounding code / text
)

### ---- Set output display options ----

# set parameters to show / evaluate demo solutions
# -> so can turn on for reference while prepping and off while making
demo_code <- TRUE
demo_results <- TRUE

# set parameter for assignment solutions
solutions <- FALSE
#solutions <- TRUE

### ---- Load packages and define functions ----

# load packages for common functions
library(kableExtra)

# default function to display dataframes nicely
# options -> vector of column names (gets processed by kable(), so can be latex), number of rows to display, and rounding digits
# -> needed because formatting of raw dataframe is bad when output in markdown
# -> nesting functions instead of piping so doesn't require magrittr
display_nice <- function(df, col.names = NA, nrow = 10, digits = 3) {
  
  # set columns names to the given vector or keep names or original df
  if (identical(col.names, NA)) {
    col.names = colnames(df)
  }
  
  # convert to knitr_kable and style
  # -> always want html format, left aligned with not full width
  # -> table.attr -> have to tell quarto to not process the table (https://github.com/quarto-dev/quarto-cli/issues/5737)
  kable_styling(kable(head(df, n = nrow),
                      col.names = col.names,
                      format = "html",
                      digits = digits,
                      table.attr = 'data-quarto-disable-processing="true"'),
                bootstrap_options = "striped",
                full_width = FALSE,
                position = "left")
}

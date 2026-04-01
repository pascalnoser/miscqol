
# miscqol

<!-- badges: start -->
<!-- badges: end -->

This package adds miscellaneous quality of life Addins to RStudio and utility functions for R.

## Installation

You can install the miscqol from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("pascalnoser/miscqol")
```

## RStudio Addins
- **Navigate to current file:** Open the Files pane and navigate to the folder containing the file currently opened in the editor.
- **Wrap in parentheses:** Wrap the selected text in parentheses and move the cursor to the front.
- **Wrap in if:** Wrap the selected text in an if statement.
- **Wrap in for:** Wrap the selected text in a for loop.
- **Assign loop variable:** Assign the loop variable in the current line to its value in the first iteration of the for loop.

## Functions
- **`get_object_sizes()`:** Get memory usage of objects in environment in numeric or human readable format.
- **`peek()`:** Display the first few rows and columns of a data frame or matrix.

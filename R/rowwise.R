#' Create variables useful for determining whether a row's values are suspicious
#'
#' `rowDistinct()` finds the number of unique values given per row of variables in an array
#' `CrunchVariable`. `straightlineResponse()` returns a `selection` variable that indicates
#' whether the responses are identical. When a row has all columns that are missing of the
#' same type, it will return `Selected`, but will missing if any other number of values is missing
#' (or there are multiple types of missing).
#'
#' @param x A `CrunchVariable`that is an an array, that unique values should be counted across.
#' @param name a character to use as the name of the case variable to create
#' @param ... Optional attributes, like `description`, to set on the new variable (passed to
#'   `VarDef()`)
#' @param na.rm Whether to count missing data as a separate category (all missing categories will
#'   be lumped together)
#' @return A Variable Definition, which can be used to create a new `CrunchVariable`
#' @export
#' @seealso [`rowCount()`] for other row-wise functions
rowDistinct <- function(x, name, ..., na.rm = TRUE) {
    if (!is.Array(x)) halt("x must be an array variable")

    if (na.rm) {
        unique_func <- function(x) length(unique(as.character(x[!is.na(x)])))
    } else {
        unique_func <- function(x) length(unique(as.character(x)))
    }

    VarDef(
        apply(as.vector(x), 1, unique_func),
        name = name,
        ...
    )
}

#' @export
#' @rdname rowDistinct
straightlineResponse <- function(x, name, ...) {
    if (!is.Array(x)) halt("x must be an array variable")

    subvar_aliases <- aliases(subvariables(x))
    if (length(subvar_aliases) == 1) stop("Array must have more than 1 subvariable.")
    VarDef(
        Reduce(
            `&`,
            lapply(subvar_aliases[-1], function(sv) x[[sv]] == x[[subvar_aliases[1]]])
        ),
        name = name,
        ...
    )
}

#' Create variables based on row-wise functions for crunch Multiple Response Variables
#'
#' Quickly generate new variables that are based on row-wise summaries of Multiple Response
#' Variables.
#'
#' @param x A crunch variable or expression
#' @param name a character to use as the name of the case variable to create
#' @param ... description, alias, and other metadata passed to [`VarDef()`]
#' @seealso [`expressions`] for the more flexible expressions that power
#' these functions and [`rowDistinct()`] for other row-wise functions
#'
#' @return A Variable Definition
#' @export
rowCount <- function(x, name, ...) {
    VarDef(selectedDepth(x), name = name, ...)
}

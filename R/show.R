#' Show methods for Crunch objects
#'
#' @param object the object
#' @return invisibly
#' @seealso [`methods::show`]
#' @importFrom methods show
#' @name show
setGeneric("show")


# Boilerplate

.showIt <- function(object) {
    out <- getShowContent(object)
    if (!is.character(out)) {
        ## Catalog show content is a data.frame unless otherwise indicated.
        ## Print it, but capture the output so we can return the character output.
        out <- capture.output(print(out))
    }
    cat(out, sep = "\n")
    invisible(out)
}

#' @rdname show
#' @export
setMethod("show", "ShojiObject", .showIt)

#' @rdname show
#' @export
setMethod("show", "CrunchVariable", .showIt)

#' @rdname show
#' @export
setMethod("show", "Category", .showIt)

#' @rdname show
#' @export
setMethod("show", "Categories", .showIt)


#' @rdname show
#' @export
setMethod("show", "Insertion", .showIt)

#' @rdname show
#' @export
setMethod("show", "Insertions", .showIt)

# Actual show methods

showAbsCategory <- function(x) {
    data.frame(
        id = id(x),
        name = name(x),
        value = value(x),
        missing = is.na(x),
        stringsAsFactors = FALSE
    )
}

showAbsCategories <- function(x) do.call("rbind", lapply(x, showAbsCategory))

showInsertion <- function(x) {
    return(data.frame(
        anchor = anchor(x), name = name(x),
        func = func(x), args = serialPaste(arguments(x)),
        stringsAsFactors = FALSE
    ))
}

showInsertions <- function(x) do.call(
        "rbind",
        c(lapply(x, getShowContent),
            stringsAsFactors = FALSE
        )
    )

showSubtotalHeading <- function(x) {
    # if anchor or args error because a categories object is not available to
    # translate from category names to ids, then show the names without error
    # for the show method only.
    anchor <- tryCatch(anchor(x), error = function(e) {
        return(x$after)
    })
    args <- tryCatch(arguments(x), error = function(e) {
        return(x$categories)
    })
    return(data.frame(
        anchor = anchor, name = name(x),
        func = func(x), args = serialPaste(args),
        stringsAsFactors = FALSE
    ))
}


showCrunchVariableTitle <- function(x) {
    out <- paste(getNameAndType(x), collapse = " ")
    desc <- description(x)
    if (!is.null(desc) && nchar(desc)) out <- c(out, desc)
    return(out)
}

getNameAndType <- function(x) {
    varname <- name(x)
    vartype <- paste0("(", type(x), ")")
    return(c(varname, vartype))
}

#' @importFrom utils capture.output
showCrunchVariable <- function(x) {
    out <- showCrunchVariableTitle(x)
    if (!is.null(activeFilter(x))) {
        out <- c(
            out,
            paste("Filtered by", formatExpression(activeFilter(x)))
        )
    }
    try(out <- c(out, "", capture.output(print(summary(x)))))
    invisible(out)
}

showCrunchDataset <- function(x) {
    out <- paste("Dataset", dQuote(name(x)))
    d <- description(x)
    if (!is.null(d) && nchar(d)) {
        out <- c(out, d)
    }

    out <- c(
        out,
        "",
        paste("Contains", nrow(x), "rows of", ncol(x), "variables:")
    )
    if (!is.null(activeFilter(x))) {
        out <- c(
            out,
            paste("Filtered by", formatExpression(activeFilter(x)))
        )
    }
    out <- c(
        out,
        "",
        describeDatasetVariables(x)
    )
    return(out)
}

describeDatasetVariables <- function(dataset) {
    nk <- namekey(dataset)
    return(vapply(variables(dataset), function(v) {
        paste0("$", v[[nk]], ": ", paste(getNameAndType(v), collapse = " "))
    }, character(1)))
}

showCategoricalArrayVariable <- function(x) {
    c(showCrunchVariableTitle(x), showSubvariables(subvariables(x)))
}

showSubvariables <- function(x) {
    out <- c("Subvariables:", vapply(index(x), function(s) {
        paste0("  $`", s$name, "`")
    }, character(1), USE.NAMES = FALSE))
    return(out)
}

showShojiOrder <- function(x, catalog_url = x@catalog_url, key = "name") {
    if (nchar(catalog_url)) {
        catalog <- index(ShojiCatalog(crGET(catalog_url)))
    } else {
        catalog <- list()
    }
    return(unlist(lapply(x, showOrderGroup, index = catalog, key = key)))
}

showOrderGroup <- function(x, index, key = "name") {
    if (inherits(x, "OrderGroup")) {
        ents <- entities(x)
        if (length(ents)) {
            group <- unlist(lapply(ents, showOrderGroup, index = index, key = key))
        } else {
            group <- "(Empty group)"
        }
        out <- c(paste0("[+] ", name(x)), paste0("    ", group))
    } else {
        tup <- index[[x]] %||% list()
        out <- tup[[key]] %||% "(Hidden variable)"
    }
    return(out)
}

formatVersionCatalog <- function(x, from = Sys.time()) {
    ts <- timestamps(x)
    if (!is.null(from)) {
        ts <- vapply(seq_along(ts), function(a) {
            ## Grab dates by sequence because POSIXt is a list internally
            ## (i.e. lapply does the wrong thing)
            this <- from - ts[a]
            num <- as.integer(this)
            un <- attr(this, "units")
            if (num == 1) {
                ## Make singular
                un <- sub("s$", "", un)
            }
            out <- paste(num, un, "ago")
            return(out)
        }, character(1))
    }
    return(data.frame(Name = names(x), Timestamp = ts, stringsAsFactors = FALSE))
}

.operators <- c("+", "-", "*", "/", "<", ">", ">=", "<=", "==", "!=", "&", "|", "%in%", "%ornm%")
.funcs.z2r <- list(
    and = "&",
    or = "|",
    is_missing = "is.na",
    `in` = "%in%",
    duplicates = "duplicated",
    ornm = "%ornm%"
)

formatExpression <- function(expr) {
    if (is.CrunchExpr(expr)) {
        return(formatExpression(expr@expression))
    } else if ("function" %in% names(expr)) {
        func <- expr[["function"]]
        func <- .funcs.z2r[[func]] %||% func ## Translate func name, if needed
        args <- formatExpressionArgs(expr[["args"]])
        if (func == "not") {
            return(paste0("!", args[1]))
        } else if (func %in% .operators) {
            return(paste(args[1], func, args[2]))
        } else if (func == "selected" && grepl("%in%", args[1])) {
            ## R's %in% is Crunch's selected(in()) wrt missing data handling
            return(args[1])
        } else {
            return(paste0(func, "(", paste(args, collapse = ", "), ")"))
        }
    } else if ("variable" %in% names(expr)) {
        return(crGET(expr[["variable"]])$body$alias)
    } else if (length(intersect(c("column", "value"), names(expr)))) {
        return(deparseAndFlatten(expressionValue(expr)))
    } else {
        ## Dunno what this is
        return("[Complex expression]")
    }
}

expressionValue <- function(expr) {
    ## Could be under either "column" or "value". R doesn't distinguish length-1
    ## from length-N values, but Crunch API does.
    unlist(expr$column %||% expr$value)
}

deparseAndFlatten <- function(x, max_length = NULL, control = NULL, ...) {
    out <- deparse(x, control = control, ...)
    if (length(out) > 1) {
        out <- paste0(out, collapse = "")
    }
    # if max_length is null, do nothing
    # else return 1:max_length of out
    if (!is.null(max_length)) {
        out <- substr(out, 1, max_length)
    }
    return(out)
}

formatExpressionArgs <- function(args) {
    ## This is just to pretty-print category values as "names"
    ## Look for "variables"
    vars <- vapply(
        args,
        function(x) identical(names(x), "variable"), logical(1)
    )
    if (sum(vars) == 1) {
        ## Great, let's see if we have any values to format
        vals <- vapply(args, function(x) {
            any(names(x) %in% c("column", "value"))
        }, logical(1))
        if (any(vals)) {
            ## Get the var, see if it is categorical
            var <- VariableEntity(crGET(args[[which(vars)]]$variable))
            ## Well, we'll identify "categorical" by presence of cats
            args[vals] <- lapply(args[vals], formatExpressionValue,
                cats = categories(var)
            )
            args[!vals] <- lapply(args[!vals], formatExpression)
            return(unlist(args))
        }
    }
    ## Else:
    return(vapply(args, formatExpression, character(1), USE.NAMES = FALSE))
}

formatExpressionValue <- function(val, cats = NULL) {
    val <- expressionValue(val)
    if (length(cats)) {
        val_id_replace <- try(i2n(val, cats), silent = TRUE)
        if (!inherits(val_id_replace, "try-error")) val <- val_id_replace
        val
    } else {
        ## TODO: iterate over, replace {?:-1} with NA
    }
    return(deparseAndFlatten(val))
}

# adhoc expressions don't have variable urls, instead they have variable ids and
# dataset urls. This function recursively goes through the expression replacing
# the variable/dataset pair with the correct URL.
# This should be fixed in https://www.pivotaltracker.com/story/show/157399444
fixAdhocFilterExpression <- function(expr) {
    if (is.CrunchExpr(expr)) {
        return(fixAdhocFilterExpression(expr@expression))
    } else if ("variable" %in% names(expr) && "dataset" %in% names(expr)) {
        dataset <- expr[["dataset"]]
        var <- expr[["variable"]]
        expr[["dataset"]] <- NULL
        path <- paste0("datasets/", dataset, "/variables/", var, "/") # nolint
        expr[["variable"]] <- absoluteURL(path, getOption("crunch.api"))
        return(expr)
    } else if (is.atomic(expr)) {
        return(expr)
    }
    return(lapply(expr, fixAdhocFilterExpression))
}

#' @rdname show
#' @export
setMethod("show", "CrunchExpr", function(object) {
    cat("Crunch expression: ", formatExpression(object), "\n",
        sep = ""
    )
    invisible(object)
})

#' @rdname show
#' @export
setMethod("show", "CrunchLogicalExpr", function(object) {
    cat("Crunch logical expression: ", formatExpression(object), "\n",
        sep = ""
    )
    invisible(object)
})

showMultitable <- function(x) {
    out <- paste("Multitable", dQuote(name(x)))

    # TODO: check variable types to alert users in a more friendly manner
    out <- c(
        out, "Column variables:",
        vapply(x@body$template, function(expr) {
            if ("each" %in% names(expr$query[[1]])) {
                # if the first element of the query is each, then this is
                # an array so take the second argument instead.
                exprToFormat <- expr$query[[2]]

                # if the second arg is a as_selected take the variable from
                # that to display var only
                if ((exprToFormat[["function"]] %||% "") == "as_selected") {
                    exprToFormat <- exprToFormat$args[[1]]
                }

                return(paste0("  ", formatExpression(exprToFormat)))
            }

            return(paste0("  ", formatExpression(expr$query[[1]])))
        }, character(1))
    )

    return(c(out))
}

# More boilerplate

setMethod("getShowContent", "AbstractCategory", showAbsCategory)
setMethod("getShowContent", "AbstractCategories", showAbsCategories)
setMethod("getShowContent", "Insertion", showInsertion)
setMethod("getShowContent", "Insertions", showInsertions)
setMethod("getShowContent", "Subtotal", showSubtotalHeading)
setMethod("getShowContent", "Heading", showSubtotalHeading)
setMethod("getShowContent", "SummaryStat", showSubtotalHeading)
setMethod("getShowContent", "CrunchVariable", showCrunchVariable)
setMethod(
    "getShowContent", "CategoricalArrayVariable",
    showCategoricalArrayVariable
)
setMethod("getShowContent", "CrunchDataset", showCrunchDataset)
setMethod("getShowContent", "Subvariables", showSubvariables)
setMethod("getShowContent", "Multitable", showMultitable)
setMethod("getShowContent", "ShojiOrder", showShojiOrder)
setMethod(
    "getShowContent", "VariableOrder",
    function(x) showShojiOrder(x, key = namekey(x))
)
setMethod("getShowContent", "ShojiCatalog", function(x) as.data.frame(x))
setMethod("getShowContent", "VersionCatalog", formatVersionCatalog)
setMethod(
    "getShowContent", "MemberCatalog",
    function(x) {
        data.frame(
            name = names(x),
            email = emails(x),
            is.editor = is.editor(x),
            stringsAsFactors = FALSE
        )
    }
)
setMethod(
    "getShowContent", "ShojiObject",
    function(x) capture.output(print(x@body))
)
setMethod(
    "getShowContent", "CrunchFilter",
    function(x) {
        return(c(
            paste("Crunch filter", dQuote(name(x))),
            paste("Expression:", formatExpression(expr(x)))
        ))
    }
)
#' @rdname show
#' @export
setMethod("show", "CrunchCube", function(object) showTransforms(object))

#' @rdname show
#' @export
setMethod("show", "OrderGroup", function(object) {
    ind <- structure(lapply(urls(object), function(x) list(name = x)),
        .Names = urls(object)
    )
    cat(showOrderGroup(object, index = ind, key = "name"), sep = "\n")
})

#' @rdname show
#' @export
setMethod("show", "CrunchGeography", function(object) {
    geo_datum <- Geodata(crGET(object$geodatum))
    cat("CrunchGeography metadata for variable \n",
        "geodatum name: \t\t", name(geo_datum), "\n",
        "geodatum description: \t", description(geo_datum), "\n",
        "geodatum url: \t\t", object$geodatum, "\n",
        "feature_key: \t\t", object$feature_key, "\n",
        "match_field: \t\t", object$match_field, "\n",
        sep = ""
    )
    invisible(object)
})

#' @rdname show
#' @export
setMethod("show", "DeckCatalog", function(object) {
    out <- as.data.frame(object)
    if (length(object) > 0) {
        print(out[, c("name", "team", "is_public", "owner_name")])
    } else {
        print(out)
    }
})

#' @rdname show
#' @export
setMethod("show", "CrunchDeck", function(object) {
    print(cubes(object))
})

#' @rdname show
#' @export
setMethod("show", "CrunchSlide", function(object) {
    out <- cubes(object)
    names(out) <- title(object)
    print(out)
})

#' @rdname show
#' @export
setMethod("show", "MultitableResult", function(object) {
    show(do.call("cbind", lapply(object, cubeToArray)))
})

setMethod("getShowContent", "ShojiFolder", function(x) {
    paste0(ifelse(types(x) == "folder", "[+] ", ""), names(x))
})

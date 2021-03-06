#' @rdname hide
#' @export
setMethod("privateFolder", "CrunchDataset", function(x) privateFolder(folders(x)))

#' @rdname hide
#' @export
setMethod("privateFolder", "VariableCatalog", function(x) privateFolder(folders(x)))

#' @rdname hide
#' @export
setMethod("privateFolder", "VariableFolder", function(x) {
  url <- shojiURL(rootFolder(x), "catalogs", "secure", mustWork = FALSE)
  if (is.null(url)) return(url)

  VariableFolder(crGET(url))
})


#' @rdname hide
#' @export
setMethod("privatize", "CrunchVariable", function(x) {
  private_dir <- privateFolder(rootFolder(x))
  if (is.null(private_dir)) {
    halt("Could not access private directory, are you an editor of this dataset?")
  }
  .moveToFolder(private_dir, x)
  # TODO: should these refresh?
  invisible(x)
})
#' @rdname hide
#' @export
setMethod("privatize", "VariableCatalog", function(x) {
  private_dir <- privateFolder(rootFolder(x))
  if (is.null(private_dir)) {
    halt("Could not access private directory, are you an editor of this dataset?")
  }
  .moveToFolder(private_dir, x)
  # TODO: should these refresh?
  invisible(x)
})

#' @rdname hide
#' @export
setMethod("deprivatize", "CrunchVariable", function(x) {
  .moveToFolder(rootFolder(x), x)
  invisible(x)
})
#' @rdname hide
#' @export
setMethod("deprivatize", "VariableCatalog", function(x) {
  .moveToFolder(rootFolder(x), x)
  invisible(x)
})

#' @rdname hide
#' @export
privatise <- function(x) {
  privatize(x)
}

#' @rdname hide
#' @export
deprivatise <- function(x) {
  deprivatize(x)
}

#' @rdname hide
#' @export
privatizeVariables <- function(dataset, variables) {
  dataset <- mv(dataset, variables, privateFolder(dataset))
  return(invisible(refresh(dataset)))
}

#' @rdname hide
#' @export
privatiseVariables <- function(dataset, variables) {
  privatizeVariables(dataset, variables)
}

#' @rdname hide
#' @export
`privateVariables<-` <- function(x, value) privatizeVariables(x, value)

#' @rdname hide
#' @export
deprivatizeVariables <- function(dataset, variables) {
  dataset <- mv(dataset, variables, folders(dataset))
  return(invisible(refresh(dataset)))
}

#' @rdname hide
#' @export
deprivatiseVariables <- function(dataset, variables) {
  deprivatizeVariables(dataset, variables)
}


#' @rdname hide
#' @export
privateVariables <- function(dataset, key = namekey(dataset)) {
  pv <- dataset@privateVariables
  if (length(pv)) {
    return(sort(vapply(index(pv), vget(key), character(1),
                       USE.NAMES = FALSE
    )))
  } else {
    return(c())
  }
}

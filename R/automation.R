#' Run a crunch automation script
#'
#' Crunch automation is a custom scripting syntax that allows you to
#' concisely describe the metadata of your data when importing. The
#' syntax is described [in the crunch API documentation](
#' https://docs.crunch.io/feature-guide/feature-automation.html)
#'
#' @param dataset A crunch dataset
#' @param script A path to a text file with crunch automation syntax
#' or a string the syntax loaded in R.
#' @param is_file The default guesses whether a file or string was
#' used in the `script` argument, but you can override the heuristics
#' by specifying `TRUE` for a file, and `FALSE` for a string.
#'
#' @return For `runCrunchAutomation()`: an updated dataset (invisibly),
#' For `crunchAutomationFailure()`, when run after a failure, a list with two items:
#' `script`: that contains the script string sent to the server and `errors` which is a
#' `data.frame` with details about the errors sent from the server.
#' @export
runCrunchAutomation <- function(dataset, script, is_file = string_is_file_like(script)) {
    stopifnot(is.dataset(dataset))
    stopifnot(is.character(script))
    if (length(script) != 1) halt("Can only run automation on a single script")

    if (is_file) {
        # base R doesn't have a way to read a file as a single string
        script <- paste(readLines(script, encoding = "UTF-8"), collapse = "\n")
    }

    crPOST(
        shojiURL(ds, "catalogs", "scripts"),
        body = toJSON(wrapEntity(body = list(body = automation_script))),
        status.handlers = list(`400` = crunchAutomationErrorHandler)
    )
    invisible(refresh(ds))
}


#' @rdname runCrunchAutomation
#' @export
crunchAutomationFailure <- function() {
    as.list(crunch_automation_error_env)
}

string_is_file_like <- function(x) {
    !grepl("\\n", x) & # no new lines
        grepl("\\.[[:alnum:]]+$", x) # ends with a file extension ('.' + any num of letters/nums)
}

# Where we store error information from crunch automation
crunch_automation_error_env <- new.env(parent = emptyenv())


#' @importFrom jsonlite fromJSON
#' @importFrom httr http_status content
crunchAutomationErrorHandler <- function(response) {
    msg <- http_status(response)$message
    automation_messages <- try(content(response)$resolution, silent = TRUE)

    if (!is.error(automation_messages)) {
        # dig into the response to get the script as we sent it to the server
        request_body <- fromJSON(rawToChar(response$request$options$postfields))
        crunch_automation_error_env$script <- request_body$body$body

        # And convert the full information of the error messages into a data.frame
        errors <- lapply(automation_messages, as.data.frame)
        crunch_automation_error_env$errors <- do.call(rbind, errors)

        automation_messages <- vapply(
            automation_messages,
            function(e_msg) paste0("\n - ", e_msg$message),
            character(1)
        )

        msg <- paste(
            "Crunch Automation Error. Run `crunchAutomationFailure() for more information.",
            paste(automation_messages, collapse = ""),
            sep = ": "
        )
    }
    halt(msg)
}
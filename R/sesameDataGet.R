
## fall back data retrieval in case ExperimentHub is down
.sesameDataGet_fallback <- function(title) {
    eh_id <- df_master$EHID[match(title, df_master$Title)]
    if (eh_id %in% c("TBD", "NA")) { eh_id <- NA; }
    stopifnot(is.na(eh_id) || length(eh_id) == 1)
    
    if (is.na(eh_id)) {
        eh_id <- title
    }
    
    message("ExperimentHub not responding. Using backup.")
    u1 <- sprintf('%s/sesameData/%s.rda', alt_base, title)
    if (valid_url(u1)) {
        sesameDataGet_assignEnv(eh_id, get(load(url(u1))))
        TRUE
    } else {
        warning(sprintf("Resource %s cannot be retrieved.", title))
        FALSE
    }
    TRUE
}

.sesameDataGet <- function(title, use_alternative = FALSE) {
    eh_id <- df_master$EHID[match(title, df_master$Title)]
    if (eh_id %in% c("TBD", "NA")) { eh_id <- NA; }
    stopifnot(is.na(eh_id) || length(eh_id) == 1)
    
    if (is.na(eh_id)) { # missing from lookup table
        stop(sprintf("Data %s not found.\n", title)) }
    
    if (!use_alternative) { # present in lookup table
        ## try ExperimentHub
        if (!exists(eh_id, envir=cacheEnv, inherits=FALSE)) {
            if (!file.exists(getExperimentHubOption("CACHE"))) {
                stopAndCache(title) }
            tryCatch({
                eh <- query(ExperimentHub(localHub=TRUE), 'sesameData')
            }, error = function(cond) { stopAndCache(title); })
            if (!(eh_id %in% names(eh))) {
                stopAndCache(title); }
            sesameDataGet_assignEnv(eh_id, eh[[eh_id]])
        }
    }

    ## try backup
    if (!exists(eh_id, envir=cacheEnv, inherits=FALSE)) {
        if (!.sesameDataGet_fallback(title)) {
            stop(sprintf(
                "%s doesn't exist. Try: sesameDataCacheAll(\"%s\")", title))
        }
    }
    return(get(eh_id, envir=cacheEnv, inherits=FALSE))
}

#' Get SeSAMe data
#'
#' @param title title of the data
#' @param use_alternative to use alternative hosting site
#' @param verbose whether to output ExperimentHub message
#' @return data object
#' @import ExperimentHub
#' @import AnnotationHub
#' @importFrom stringr str_replace
#' @examples
#'
#' sesameDataCacheExample()
#' EPIC.1.SigDF <- sesameDataGet('EPIC.1.SigDF')
#' @export
sesameDataGet <- function(title, use_alternative = FALSE, verbose = FALSE) {

    title <- str_replace(title, "MMB", "MM285") # fix potential code discrepancy
    if (!use_alternative) { # take it from env
        use_alternative <- (
            !is.null(getOption("sesameData_use_alternative")) &&
                getOption("sesameData_use_alternative"))
    }
    
    if (verbose) {
        .sesameDataGet(title, use_alternative = use_alternative)
    } else {
        suppressMessages(
            log <- capture.output(obj <- .sesameDataGet(
                title, use_alternative = use_alternative)));
        obj
    }
}

#' List all SeSAMe data
#'
#' @param filter keyword to filter title, optional
#' @param full whether to display all columns
#' @return all titles from SeSAMe Data
#' @examples
#' sesameDataList("KYCG")
#' @export
sesameDataList <- function(filter = NULL, full = FALSE) {
    df <- df_master
    if (!full) {
        df <- df[,c("EHID","Title")]
    }
    if (!is.null(filter)) {
        df <- df[grep(filter, df$Title),]
    }
    df
}



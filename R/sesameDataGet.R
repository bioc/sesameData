#' Master data frame for all object to cache
#'
#' This is an internal object which will be updated on every new release
#' library(ExperimentHub)
#' eh <- query(ExperimentHub(localHub=FALSE), c("sesameData", "v1.11.7"))
#' data.frame(name=eh$title, eh=names(eh))
#'
#' @name df_master
#' @docType data
NULL

cacheEnv <- new.env()
alt_base <- "https://zhouserver.research.chop.edu"
alt_base2 <- "https://zwdzwd.s3.amazonaws.com"

#' Check whether the title exists in cacheEnv
#'
#' @param title the title to check
#' @return the data associated with the title or NULL if title doesn't exist
sesameDataGet_checkEnv <- function(title) {
    if (exists(title, envir = cacheEnv, inherits = FALSE)) {
        return(get(title, envir = cacheEnv, inherits = FALSE))
    } else {
        return(NULL)
    }
}

sesameDataGet_assignEnv <- function(title, data) {
    assign(title, data, envir = cacheEnv)
}

#' Empty cache environment to free memory
#'
#' When this function is called sesameDataGet will
#' retrieve all data from disk again instead of using the in-memory
#' cache, i.e., sesameData:::cacheEnv.
#'
#' Note this is different from sesameDataClearHub which empties the
#' ExperimentHub on disk.
#'
#' @return gc() output
#' @examples
#' sesameDataGet_resetEnv()
#' @export
sesameDataGet_resetEnv <- function() {
    rm(list=ls(envir=cacheEnv), envir=cacheEnv)
    gc()
}

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
    u2 <- sprintf('%s/sesameData/%s.rda', alt_base2, title)
    if (valid_url(u1)) {
        sesameDataGet_assignEnv(eh_id, get(load(url(u1))))
        TRUE
    } else if (valid_url(u2)) {
        sesameDataGet_assignEnv(eh_id, get(load(url(u2))))
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
    
    if (is.na(eh_id)) {            # missing from lookup table
        eh_id <- title             # use title itself
    } else if (!use_alternative) { # present in lookup table
        ## try ExperimentHub
        if (!exists(eh_id, envir=cacheEnv, inherits=FALSE)) {
            if (!file.exists(getExperimentHubOption("CACHE"))) {
                stopAndCache(title)
            }
            tryCatch({
                eh <- query(ExperimentHub(localHub=TRUE), 'sesameData')
            }, error = function(cond) {
                stopAndCache(title)
            })
            if (!(eh_id %in% names(eh))) {
                stopAndCache(title)
            }
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
#' @import rmarkdown
#' @examples
#'
#' sesameDataCacheExample()
#' EPIC.1.SigDF <- sesameDataGet('EPIC.1.SigDF')
#' @export
sesameDataGet <- function(title, use_alternative = FALSE, verbose = FALSE) {

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
#' @return all titles from SeSAMe Data
#' @examples
#' sesameDataList("KYCG")
#' @export
sesameDataList <- function(filter = NULL) {
    df <- df_master[,c("EHID","Title")]
    if (!is.null(filter)) {
        df <- df[grep(filter, df$Title),]
    }
    df
}


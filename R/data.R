
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

## fall back data retrieval in case ExperimentHub is down
.sesameDataGet2 <- function(title) {
    eh_id <- df_master$EHID[match(title, df_master$Title)]
    stopifnot(is.na(eh_id) || length(eh_id) == 1)
    
    if (is.na(eh_id)) {
        eh_id <- title
    }
    
    message("ExperimentHub not responding. Using backup.")
    u1 <- sprintf('%s/sesameData/%s.rda', alt_base, title)
    u2 <- sprintf('%s/sesameData/%s.rda', alt_base2, title)
    if (valid_url(u1)) {
        assign(eh_id, get(load(url(u1))), envir=cacheEnv)
        TRUE
    } else if (valid_url(u2)) {
        assign(eh_id, get(load(url(u2))), envir=cacheEnv)
        TRUE
    } else {
        warning(sprintf("Resource %s cannot be retrieved.", title))
        FALSE
    }
    TRUE
}

stopAndCache <- function(title) {
    stop(sprintf('
| File %s needs to be cached to be used in sesame.
| Please make sure you have updated ExperimentHub and try
| > sesameDataCacheAll()
| to retrieve and cache needed sesame data.', title))
}

.sesameDataGet <- function(title, use_alternative = FALSE) {
    eh_id <- df_master$EHID[match(title, df_master$Title)]
    stopifnot(is.na(eh_id) || length(eh_id) == 1)
    
    if (is.na(eh_id)) {            # missing from lookup table
        eh_id <- title              # use title itself
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
            assign(eh_id, eh[[eh_id]], envir=cacheEnv)
        }
    }

    ## try backup
    if (!exists(eh_id, envir=cacheEnv, inherits=FALSE)) {
        if (!.sesameDataGet2(title)) {
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
#' sesameDataCache("HM27")
#' HM27.hg19.manifest <- sesameDataGet('HM27.hg19.manifest')
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

#' @import curl
has_internet <- function(){
    !is.null(curl::nslookup("r-project.org", error = FALSE))
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




#' Master data frame for all object to cache
#'
#' This is an internal object which will be updated on every new release
#' eh = query(ExperimentHub(localHub=FALSE), c("sesameData", "v1.9.1"))
#' data.frame(name=eh$title, eh=names(eh))
#'
#' @name df_master
#' @docType data
NULL

#' EH ID lookup table in R/sysdata.rda
#'
#' @name eh_id_lookup
#' @docType data
NULL

#' EH ID lookup table per platform in R/sysdata.rda
#'
#' @name platform2eh_ids
#' @docType data
NULL

cacheEnv <- new.env()
alt_base = "https://zhouserver.research.chop.edu"
alt_base2 = "https://zwdzwd.s3.amazonaws.com"

## fall back data retrieval in case ExperimentHub is down
.sesameDataGet2 <- function(title) {
    eh_id = eh_id_lookup[title]
    if (is.na(eh_id)) {
        eh_id = title
    }
    message("ExperimentHub not responding. Using backup.")
    tryCatch(
        assign(eh_id, get(load(url(sprintf('%s/sesameData/%s.rda',
            alt_base, title)))),
            envir=cacheEnv),
        error = function(cond) {
            message("%s doesn't respond. Try alternative backup")
            tryCatch(
                assign(eh_id, get(load(url(sprintf("%s/sesameData/%s.rda",
                    alt_base2, title)))),
                    envir=cacheEnv),
                error = function(cond) {
                    message("sesameDataGet2 fails:")
                    message(cond)
                })
            return(FALSE)
        },
        warning = function(cond) {
            message("sesameDataGet2 causes an issue:")
            message(cond)
            return(FALSE)
        })
    TRUE
}

stopAndCache <- function(title) {

    platforms = names(which(vapply(
        platform2eh_ids, function(x) title %in% names(x), logical(1))))
    
    stop(sprintf('
| File needs to be cached to be used in sesame.
| Please run
| > sesameDataCache("%s")
| or cache all platforms by
| > sesameDataCacheAll()
| to retrieve and cache needed sesame data.', platforms[[1]]))
}

.sesameDataGet <- function(title, use_alternative = FALSE) {
    eh_id = eh_id_lookup[title]
    if (is.na(eh_id)) {            # missing from lookup table
        eh_id = title              # use title itself
    } else if (!use_alternative) { # present in lookup table
        ## try ExperimentHub
        if (!exists(eh_id, envir=cacheEnv, inherits=FALSE)) {
            if (!file.exists(getExperimentHubOption("CACHE"))) {
                stopAndCache(title)
            }
            tryCatch({
                eh = query(ExperimentHub(localHub=TRUE), 'sesameData')
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
#' @param verbose whether to output ExperimentHub message
#' @return data object
#' @import ExperimentHub
#' @import AnnotationHub
#' @import rmarkdown
#' @examples
#'
#' sesameDataCache("HM27")
#' genomeInfo.hg38 <- sesameDataGet('genomeInfo.hg38')
#' @export
sesameDataGet <- function(title, verbose = FALSE) {

    use_alternative = (
        !is.null(getOption("sesameData_use_alternative")) &&
            getOption("sesameData_use_alternative"))
    
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
#' @return all titles from SeSAMe Data
#' @examples
#' sesameDataList()
#' @export
sesameDataList <- function() {
    eh_id_lookup
}



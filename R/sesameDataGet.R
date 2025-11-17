
## fall back to zenodo in case EHID is not assigned yet.
.sesameDataGet_fallback <- function(title, u1) {
    if (valid_url(u1)) {
        sesameDataGet_assignEnv(title, get(load(url(u1))))
        TRUE
    } else {
        warning(sprintf("Resource %s cannot be retrieved.", title))
        FALSE
    }
    TRUE
}

.sesameDataGet <- function(title) {

    idx <- match(title, df_master$Title)
    eh_id <- df_master$EHID[idx]
    stopifnot(length(eh_id) == 1)
    pfx <- df_master$Location_Prefix[idx]
    rdata <- df_master$RDataPath[idx]
    if (is.na(eh_id) && !is.na(pfx)) { # no EHID and exists zenodo link
        ## cache uses title, not ehid
        if (exists(title, envir=cacheEnv, inherits=FALSE)) {
            return(get(title, envir=cacheEnv, inherits=FALSE))
        }
        
        if (.sesameDataGet_fallback(title, paste0(pfx, "/", rdata))) {
            return(get(title, envir=cacheEnv, inherits=FALSE))
        } else {
            stopAndCache(title)
        }
    } else {
        if (exists(eh_id, envir=cacheEnv, inherits=FALSE)) {
            return(get(eh_id, envir=cacheEnv, inherits=FALSE))
        }
        if (!file.exists(getExperimentHubOption("CACHE"))) {
            stopAndCache(title) }
        tryCatch({
            eh <- query(ExperimentHub(localHub=TRUE), 'sesameData')
        }, error = function(cond) { stopAndCache(title); })
        if (!(eh_id %in% names(eh))) {
            stopAndCache(title); }
        sesameDataGet_assignEnv(eh_id, eh[[eh_id]])
        return(get(eh_id, envir=cacheEnv, inherits=FALSE))
    }
}

#' Get SeSAMe data
#'
#' @param title title of the data
#' @param verbose whether to output ExperimentHub message
#' @return data object
#' @import ExperimentHub
#' @import AnnotationHub
#' @importFrom stringr str_replace
#' @examples
#'
#' sesameDataCache("EPIC.1.SigDF")
#' EPIC.1.SigDF <- sesameDataGet('EPIC.1.SigDF')
#' @export
sesameDataGet <- function(title, verbose = FALSE) {

    title <- str_replace(title, "MMB", "MM285") # fix potential code discrepancy
    
    if (verbose) {
        .sesameDataGet(title)
    } else {
        suppressMessages(
            log <- capture.output(obj <- .sesameDataGet(title)))
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

#' Whether sesameData has
#'
#' @param data_titles data titles to check
#' @return a boolean vector the same length as data_titles
#' @examples
#' sesameDataHas(c("EPIC.address","EPIC.address.Nonexist"))
#' @export
sesameDataHas <- function(data_titles) {
    data_titles %in% sesameDataList()$Title
}


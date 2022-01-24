sesameDataCache0 <- function(eh_ids) {
    ## load meta data
    message(sprintf("Metadata (N=%d):\n", length(eh_ids)))
    suppressMessages(log <- capture.output(
        eh <- query(ExperimentHub(), "sesameData")[eh_ids]))
    
    ## load actual data
    tmp2 <- lapply(seq_along(eh), function(i) {
        message(sprintf(
            "(%d/%d) %s | %s:\n", i, length(eh_ids), eh$title[i], eh_ids[i]))
        suppressMessages(log <- capture.output(cache(eh[i])))
    })
}

#' Cache SeSAMe data for specific platform
#'
#' @return TRUE
#' @import ExperimentHub
#' @import AnnotationHub
#' @examples
#' if(FALSE) { sesameDataCacheExample() }
#' @export
sesameDataCacheExample <- function() {
    
    dir.create(getExperimentHubOption("CACHE"), showWarnings = FALSE)
    setExperimentHubOption(arg="MAX_DOWNLOADS", 100)

    tmp <- df_master
    tmp <- tmp[tmp$Example==1,]
    eh_ids <- tmp$EHID
    
    ## TODO platform is supported but no data added for the snapshot
    try({ # only if it's not cached already
        eh_ids <- eh_ids[!(eh_ids %in% names(ExperimentHub(localHub=TRUE)))]
    }, silent = TRUE)
    if (length(eh_ids) == 0) return(TRUE);

    titles <- tmp$Title[match(eh_ids, tmp$EHID)]
    tryCatch({
        sesameDataCache0(eh_ids)
    }, error = function(cond) {
        message("ExperimentHub Caching fails:")
        message(cond)
        return(FALSE)
    })
    invisible(TRUE)
}

## a convenience function, only works on Mac
sesameDataClearHub <- function() {
    unlink("~/Library/Caches/org.R-project.R/R/ExperimentHub/", recursive=TRUE)
}

#' Clear cache to free memory
#'
#' sesameData:::cacheEnv
#'
#' @return gc() output
#' @examples
#' sesameDataClearCache()
#' @export
sesameDataClearCache <- function() {
    rm(list=ls(envir=cacheEnv), envir=cacheEnv)
    gc()
}

#' Cache all SeSAMe data
#'
#' @return TRUE
#' @import ExperimentHub
#' @import AnnotationHub
#' @examples
#' if(FALSE) { sesameDataCacheAll() }
#' @export
sesameDataCacheAll <- function() {
    setExperimentHubOption(arg="MAX_DOWNLOADS", 100)

    dir.create(getExperimentHubOption("CACHE"), showWarnings = FALSE)
        
    eh_ids <- unique(df_master$EHID)
    eh_ids <- eh_ids[eh_ids != "TBD"]
    
    try({
        eh_ids <- eh_ids[!(eh_ids %in% names(ExperimentHub(localHub=TRUE)))]
    }, silent = TRUE)
    if (length(eh_ids) == 0) return(TRUE);
    tryCatch({
        sesameDataCache0(eh_ids)
    }, error = function(cond) {
        message("ExperimentHub Caching fails:")
        message(cond)
        return(FALSE)
    })
    invisible(TRUE)
}

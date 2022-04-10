
sesameDataRecache <- function(eh_id) {
    stopifnot(startsWith(eh_id, "EH"))
    tryCatch({
        sesameDataCache0(eh_id)
    }, error=function(cond) {
        stopAndCache(eh_id)
    })
    query(ExperimentHub(localHub=TRUE), 'sesameData')
}

stopAndCache <- function(title) {
    stop(sprintf('
| File %s needs to be cached to be used in sesame.
| Please make sure you have updated ExperimentHub and try
| > sesameDataCache()
| to retrieve and cache needed sesame data.', title))
}

sesameDataCache0 <- function(eh_ids) {
    ## load meta data
    message(sprintf("Metadata (N=%d):\n", length(eh_ids)))
    suppressMessages(log <- capture.output(
        eh <- query(ExperimentHub(), "sesameData")[eh_ids]))
    
    ## load actual data
    tmp2 <- lapply(seq_along(eh), function(i) {
        message(sprintf(
            "(%d/%d) %s:\n", i, length(eh_ids), eh_ids[i]))
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
    
    setExperimentHubOption(arg="MAX_DOWNLOADS", 100)

    tmp <- df_master
    tmp <- tmp[tmp$Example==1,]
    eh_ids <- tmp$EHID
    
    ## TODO platform is supported but no data added for the snapshot
    suppressMessages(try({ # only if it's not cached already
        eh_ids <- eh_ids[!(eh_ids %in% names(ExperimentHub(localHub=TRUE)))]
    }, silent = TRUE))
    if (length(eh_ids) == 0) return(invisible(TRUE));

    titles <- tmp$Title[match(eh_ids, tmp$EHID)]
    tryCatch({
        sesameDataCache0(eh_ids)
    }, error = function(cond) {
        message("ExperimentHub Caching fails:")
        message(cond)
        return(invisible(FALSE))
    })
    invisible(TRUE)
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

    eh_ids <- unique(df_master$EHID)
    eh_ids <- eh_ids[eh_ids != "TBD"]
    
    suppressMessages(try({
        eh_ids <- eh_ids[!(eh_ids %in% names(ExperimentHub(localHub=TRUE)))]
    }, silent = TRUE))
    if (length(eh_ids) == 0) return(invisible(TRUE));
    tryCatch({
        sesameDataCache0(eh_ids)
    }, error = function(cond) {
        message("ExperimentHub Caching fails:")
        message(cond)
        return(invisible(FALSE))
    })
    invisible(TRUE)
}

#' Cache all SeSAMe data
#'
#' @return TRUE
#' @import ExperimentHub
#' @import AnnotationHub
#' @examples
#' if(FALSE) { sesameDataCacheAll() }
#' @export
sesameDataCache <- sesameDataCacheAll

sesameDataCache0 = function(eh, eh_ids) {
    ## load meta data
    cat(sprintf("Metadata (N=%d):\n", length(eh_ids)))
    suppressMessages(log <- capture.output(
        eh <- query(ExperimentHub(), "sesameData")[eh_ids]))
    
    ## load actual data
    tmp2 = lapply(seq_along(eh), function(i) {
        cat(sprintf(
            "(%d/%d) %s | %s:\n", i, length(eh_ids), eh$title[i], eh_ids[i]))
        suppressMessages(log <- capture.output(cache(eh[i])))
    })
}

#' Cache SeSAMe data for specific platform
#'
#' @param platform EPIC, HM450, MM285, etc.
#' @param keyword keyword used to filter records
#' @return TRUE
#' @import ExperimentHub
#' @import AnnotationHub
#' @examples
#' if(FALSE) { sesameDataCache("MM285") }
#' @export
sesameDataCache <- function(
    platform=NULL, keyword = NULL) {
    
    dir.create(getExperimentHubOption("CACHE"), showWarnings = FALSE)
    setExperimentHubOption(arg="MAX_DOWNLOADS", 100)

    tmp = df_master
    if (!is.null(platform)) {
        if (paste0("Platform",platform) %in% colnames(tmp)) {
            tmp = tmp[tmp[[paste0("Platform",platform)]] == 1,]
        } else {
            stop(sprintf(
                "%s not supported for this version.", platform))
        }
    }
    
    if (!is.null(keyword)) {
        tmp = tmp[grep(keyword, tmp$Title),]
    }
    eh_ids = tmp$EHID
    
    ## TODO platform is supported but no data added for the snapshot
    try({ # only if it's not cached already
        eh_ids = eh_ids[!(eh_ids %in% names(ExperimentHub(localHub=TRUE)))]
    }, silent = TRUE)
    if (length(eh_ids) == 0) return(TRUE);

    titles = tmp$Title[match(eh_ids, tmp$EHID)]
    tryCatch({
        sesameDataCache0(eh, eh_ids)
    },
    error = function(cond) {
        message("ExperimentHub Caching fails:")
        message(cond)
        return(FALSE)
    })
    TRUE
}

## a convenience function, only works on Mac
sesameDataClearCache = function() {
    unlink("~/Library/Caches/org.R-project.R/R/ExperimentHub/", recursive=TRUE)
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
        
    eh_ids = unique(df_master$EHID)
    eh_ids = eh_ids[eh_ids != "TBD"]
    
    try({
        eh_ids = eh_ids[!(eh_ids %in% names(ExperimentHub(localHub=TRUE)))]
    }, silent = TRUE)
    if (length(eh_ids) == 0) return(TRUE);
    tryCatch({
        sesameDataCache0(eh, eh_ids)
    },
    error = function(cond) {
        message("ExperimentHub Caching fails:")
        message(cond)
        return(FALSE)
    })
    TRUE
}

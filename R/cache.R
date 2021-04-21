#' Cache SeSAMe data for specific platform
#'
#' @param platform EPIC, HM450, MM285, etc.
#' @param showProgress whether to show progress of download
#' @return TRUE
#' @import ExperimentHub
#' @import AnnotationHub
#' @examples
#' if(FALSE) { sesameDataCache("MM285") }
#' @export
sesameDataCache <- function(platform, showProgress = FALSE) {
    dir.create(getExperimentHubOption("CACHE"), showWarnings = FALSE)
    setExperimentHubOption(arg="MAX_DOWNLOADS", 100)
    if (platform %in% names(platform2eh_ids)) {
        eh_ids = platform2eh_ids[[platform]]
    } else {
        stop(sprintf(
            "%s not supported for this version. Nothing to cache.", platform))
    }
    ## platform is supported but no data added for the snapshot
    if (length(eh_ids) == 0) return(TRUE);
    tryCatch(
    {
        ## load meta data
        if (showProgress) {
            eh = query(ExperimentHub(), "sesameData")[eh_ids]
        } else {
            suppressMessages(log <- capture.output(
                eh <- query(ExperimentHub(), "sesameData")[eh_ids]))
        }

        ## load actual data
        if (showProgress) {
            cache(eh)
        } else {
            suppressMessages(log <- capture.output(cache(eh)))
        }
    },
    error = function(cond) {
        message("ExperimentHub Caching fails:")
        message(cond)
        return(FALSE)
    },
    warning = function(cond) {
        message("ExperimentHub Caching causes a warning:")
        message("
Usually it is caused by having both old and new versions
of ExperimentHub on your system. You can consider deleting
the old caching at the location above, as suggested.
")
        message(cond)
        return(FALSE)
    })
    
    TRUE
}

#' Cache all SeSAMe data
#'
#' @param showProgress whether to show progress of download
#' @return TRUE
#' @import ExperimentHub
#' @import AnnotationHub
#' @examples
#' if(FALSE) { sesameDataCacheAll() }
#' @export
sesameDataCacheAll <- function(showProgress = FALSE) {
    setExperimentHubOption(arg="MAX_DOWNLOADS", 100)

    dir.create(getExperimentHubOption("CACHE"), showWarnings = FALSE)
        
    eh_ids = unique(eh_id_lookup)
    tryCatch(
    {
        ## load meta data
        if (showProgress) {
            eh = query(ExperimentHub(), "sesameData")[eh_ids]
        } else {
            suppressMessages(log <- capture.output(
                eh <- query(ExperimentHub(), "sesameData")[eh_ids]))
        }

        ## load actual data
        if (showProgress) {
            cache(eh)
        } else {
            suppressMessages(log <- capture.output(cache(eh)))
        }
    },
    error = function(cond) {
        message("ExperimentHub Caching fails:")
        message(cond)
        return(FALSE)
    },
    warning = function(cond) {
        message("ExperimentHub Caching causes a warning:")
        message(cond)
        return(FALSE)
    })
    
    TRUE
}

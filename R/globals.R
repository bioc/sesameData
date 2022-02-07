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
manifest_base <- "https://github.com/zhou-lab/InfiniumManifestsV"
manifest_base_default_version <- 1
anno_base <- "https://github.com/zhou-lab/InfiniumAnnotationV"
anno_base_default_version <- 1

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

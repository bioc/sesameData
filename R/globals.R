#' Master data frame for all object to cache
#'
#' This is an internal object which will be updated on every new release
#' library(ExperimentHub)
#' eh <- query(ExperimentHub(localHub=FALSE), c("sesameData", "v1.13.1"))
#' data.frame(name=eh$title, eh=names(eh))
#'
#' Cache location is default to
#' /Users/zhouw3/Library/Caches/org.R-project.R/R/ExperimentHub/
#'
#' @format A data frame with 22 columns:
#' \describe{
#'   \item{Comments}{Additional comments}
#'   \item{EHID}{ExperimentHub ID}
#'   \item{VERSION}{sesameData version}
#'   \item{IN_USE}{Logical indicating if the resource is in use}
#'   \item{Title}{Title of the data resource}
#'   \item{Description}{Description of the data resource}
#'   \item{BiocVersion}{Bioconductor version}
#'   \item{Genome}{Genome build (e.g., hg38, mm10)}
#'   \item{SourceType}{Source file type}
#'   \item{SourceUrl}{URL to source}
#'   \item{SourceVersion}{Version of source data}
#'   \item{Species}{Species name}
#'   \item{TaxonomyId}{NCBI Taxonomy ID}
#'   \item{Coordinate_1_based}{Logical indicating if coordinates are 1-based}
#'   \item{DataProvider}{Data provider name}
#'   \item{Maintainer}{Maintainer contact information}
#'   \item{RDataClass}{R data class}
#'   \item{DispatchClass}{Dispatch class for ExperimentHub}
#'   \item{RDataPath}{Path to RData file}
#'   \item{Location_Prefix}{URL prefix for data location}
#'   \item{Tags}{Tags for categorization}
#'   \item{Notes}{Additional notes}
#' }
#' @name df_master
#' @docType data
#' @return master sheet of sesameData objects
NULL

cacheEnv <- new.env()
alt_base <- "https://zhouserver.research.chop.edu"

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

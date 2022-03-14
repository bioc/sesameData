

#' get Infinium manifest GRanges
#'
#' Note that some unaligned probes are not included.
#' For full manifest, please visit
#' \url{http://zwdzwd.github.io/InfiniumAnnotation}
#'
#' @param platform Mammal40, MM285, EPIC, and HM450
#' @param genome hg38, mm10 etc.
#' @return GRanges
#' @examples
#' gr <- sesameData_getManifestGRanges("Mammal40")
#' @export
sesameData_getManifestGRanges <- function(
    platform, genome = NULL) {

    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)

    addr <- sesameDataGet(sprintf("%s.address", platform))
    if (genome %in% names(addr)) { return(addr[[genome]]) }
    stop(sprintf("%s-%s manifest is not found", platform, genome))
}

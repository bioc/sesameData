

#' get Infinium manifest GRanges
#'
#' for building manifest from the original
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

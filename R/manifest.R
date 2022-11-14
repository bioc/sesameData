#' get Infinium manifest GRanges
#'
#' Note that some unaligned probes are not included.
#' For full manifest, please visit
#' \url{http://zwdzwd.github.io/InfiniumAnnotation}
#'
#' @param platform Mammal40, MM285, EPIC, and HM450
#' @param genome hg38, mm10, ... will infer if not given.
#' For additional mapping, download the GRanges object from
#' http://zwdzwd.github.io/InfiniumAnnotation
#' and provide the following argument
#' ..., genome = sesameAnno_buildManifestGRanges("downloaded_file"),...
#' to this function.
#' @return GRanges
#' @examples
#' gr <- sesameData_getManifestGRanges("Mammal40")
#' @export
sesameData_getManifestGRanges <- function(
    platform, genome = NULL) {

    if ("GenomicRanges" %in% class(genome)) { # if is a GRanges object already
        return(genome)
    }

    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)
    ## only one genome is supported natively.
    addr <- sesameDataGet(sprintf("%s.address", platform))
    if (genome %in% names(addr)) {
        return(addr[[genome]])
    } else {
        return(NULL)
    }
}

#' Get genome info files
#' 
#' @param genome hg38, mm10, or GRanges with a metadata(genome)[["genome"]]
#' @return a list of genome info files
#' @examples
#' ginfo <- sesameData_getGenomeInfo("hg38")
#' @export
sesameData_getGenomeInfo <- function(genome) {
    if ("GenomicRanges" %in% class(genome)) {
        genome <- attr(genome, "genome")
    }
    stopifnot(is.character(genome))
    key <- paste0('genomeInfo.', genome)
    if (!sesameDataHas(key)) {
        stop(sprintf("%s genome info is not found in Bioconductor.
Please go to http://zwdzwd.github.io/InfiniumAnnotation
for additional genome info files.
", genome))
    }
    sesameDataGet(key)
}

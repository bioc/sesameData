#' Find genome assembly version(s) supported for a platform
#'
#' @param genome mm10, hg38, ..., or NULL
#' @param platform HM27, HM450, EPIC, EPICv2, MSA, ...
#' @return genome as string
#' @examples
#' sesameData_check_genome(NULL, "Mammal40")
#' @export
sesameData_check_genome <- function(genome, platform) {
    if ("GRanges" %in% class(genome)) { return(genome); }
    platform <- sesameData_check_platform(platform)
    supported_genomes <- c("hg19", "hg38", "mm10")
    default_genome <- c(
        HM27 = "hg38", HM450 = "hg38",
        EPIC = "hg38", EPICv2 = "hg38", MSA = "hg38",
        Mammal40 = "hg38", MM285 = "mm10")
    if (is.null(genome)) { genome <- default_genome[platform] }
    stopifnot(!is.null(genome))
    stopifnot(genome %in% supported_genomes)
    genome
}

#' Get genome info files
#' 
#' @param genome hg38, mm10, or GRanges with a metadata(genome)[["genome"]]
#' @return a list of genome info files
#' @examples
#' sesameDataCache("genomeInfo.hg38")
#' res <- sesameData_getGenomeInfo("hg38")
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

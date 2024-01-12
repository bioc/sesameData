#' get genes next to certain probes
#'
#' @param Probe_IDs probe IDs
#' @param platform EPIC, HM450, ... will infer if not given
#' @param genome hg38, mm10, ... will infer if not given.
#' For additional mapping, download the GRanges object from
#' http://zwdzwd.github.io/InfiniumAnnotation
#' and provide the following argument
#' ..., genome = sesameAnno_buildManifestGRanges("downloaded_file"),...
#' to this function.
#' @param max_distance maximum distance to gene (default: 10000)
#' @return a GRanges object for overlapping genes
#' @importMethodsFrom IRanges subsetByOverlaps
#' @examples
#' sesameData_getGenesByProbes(c("cg14620903","cg22464003"))
#' @export
sesameData_getGenesByProbes <- function(
    Probe_IDs, platform = NULL, genome = NULL, max_distance = 10000) {
    if (is.null(platform)) {
        platform <- inferPlatformFromProbeIDs(Probe_IDs) }
    genes <- sesameData_txnToGeneGRanges(
        sesameData_getTxnGRanges(
            sesameData_check_genome(genome, platform)))
    probes <- sesameData_getManifestGRanges(platform, genome=genome)
    ## not every probes are mappable
    probes <- probes[names(probes) %in% Probe_IDs]
    subsetByOverlaps(genes, probes + max_distance)
}

#' infer platform from Probe_IDs
#'
#' @param Probe_IDs probe IDs
#' @param silent suppress message
#' @return a platform code
#' @examples
#' inferPlatformFromProbeIDs(c("cg14620903","cg22464003"))
#' @export
inferPlatformFromProbeIDs <- function(Probe_IDs, silent = FALSE) {
    sig <- sesameDataGet("probeIDSignature")
    cnts <- vapply(sig, function(x) sum(Probe_IDs %in% x), integer(1))
    if(sum(cnts == max(cnts)) > 1) {
        stop("Ambiguous platform. Please provide platform explicitly.") }

    platform <- names(which.max(cnts))
    if (!silent) {
        message("Platform set to: ", platform)
    }
    platform
}

#' check platform code
#'
#' @param platform input platform
#' @param probes probes by which the platform may be guessed
#' @return platform code
#' @examples
#' sesameData_check_platform("HM450")
#' @export
sesameData_check_platform <- function(platform = NULL, probes = NULL) {
    if (is.null(platform)) {
        if (is.null(probes)) {
            platform <- "EPIC" # random guess
        } else {
            platform <- inferPlatformFromProbeIDs(probes)
        }
    }
    stopifnot(platform %in% c("EPIC", "HM27", "HM450", "MM285", "Mammal40"))
    platform
}

#' check genome supported for a platform
#'
#' @param genome mm10, hg38, ..., or NULL
#' @param platform HM27, HM450, EPIC, ...
#' @return genome as string
#' @examples
#' sesameData_check_genome(NULL, "Mammal40")
#' @export
sesameData_check_genome <- function(genome, platform) {
    if ("GRanges" %in% class(genome)) { return(genome); }
    platform <- sesameData_check_platform(platform)
    supported_genomes <- c("hg19", "hg38", "mm10")
    default_genome <- c(
        HM27 = "hg38", HM450 = "hg38", EPIC = "hg38",
        Mammal40 = "hg38", MM285 = "mm10")
    if (is.null(genome)) { genome <- default_genome[platform] }
    stopifnot(!is.null(genome))
    stopifnot(genome %in% supported_genomes)
    genome
}


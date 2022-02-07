#' infer platform from Probe_IDs
#'
#' @param Probe_IDs probe IDs
#' @return a platform code
#' @examples
#' inferPlatformFromProbeIDs(c("cg14620903","cg22464003"))
#' @export
inferPlatformFromProbeIDs <- function(Probe_IDs) {
    sig <- sesameDataGet("probeIDSignature")
    names(which.max(vapply(
        sig, function(x) sum(Probe_IDs %in% x), integer(1))))
}

sesameData_check_platform <- function(platform) {
    if (is.null(platform)) {
        platform <- "EPIC"
    } else {
        stopifnot(platform %in% c(
            "EPIC", "HM27", "HM450", "MM285", "Mammal40"))
    }
    platform
}

#' check genome supported for a platform
#'
#' @param genome mm10, hg38, ... or NULL
#' @param platform HM27, HM450, EPIC, ...
#' @return genome as string
#' @examples
#' sesameData_check_genome(NULL, "Mammal40")
#' @export
sesameData_check_genome <- function(genome, platform) {
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

## defaultAssembly <- function(platform) {
##     platform2build <- c(
##         "HM27"="hg38",
##         "HM450"="hg38",
##         "EPIC"="hg38",
##         "MM285"="mm10",
##         "Mammal40"="hg38"
##     )
##     if (!(platform %in% names(platform2build))) {
##         stop(sprintf(
##             "Platform %s not supported. Try custom manifest?", platform))
##     }
##     platform2build[platform]
## }

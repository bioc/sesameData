check_genomes <- function(genome, platform) {
    default_genome <- c(
        Mammal40="hg38", MM285="mm10", EPIC="hg38", HM450="hg38")
    if (is.null(genome)) { genome <- default_genome[platform] }
    stopifnot(!is.null(genome))
    genome
}

#' download Infinium manifest from repositories
#'
#' @param platform Mammal40, MM285, EPIC, and HM450
#' @param genome hg38, mm10 etc.
#' @param version release version, default is the latest
#' @return tibble
#' @importFrom readr read_tsv
#' @importFrom readr cols
#' @importFrom readr col_integer
#' @importFrom readr col_character
#' @examples
#' mft <- sesameData_getManifestDF("Mammal40")
#' @export
sesameData_getManifestDF <- function(platform, genome=NULL, version=1) {
    genome <- check_genomes(genome, platform)
    base <- "https://github.com/zhou-lab/InfiniumManifestsV"
    read_tsv(sprintf(
        "%s%d/raw/main/%s/%s.tsv.gz", base, version, platform, genome),
        col_types=cols(CpG_beg=col_integer(), CpG_end=col_integer(),
            address_A=col_integer(), address_B=col_integer(),
            .default=col_character()))
}

#' download Infinium manifest from repositories and return GRanges
#'
#' @param platform Mammal40, MM285, EPIC, and HM450
#' @param genome hg38, mm10 etc.
#' @param decoy whether to include probes mapped to decoy sequence
#' @param version release version, default is the latest
#' @param columns additional columns to add from the manifest to mcols
#' @return GRanges
#' @importFrom GenomeInfoDb Seqinfo
#' @examples
#' gr <- sesameData_getManifestGRanges("Mammal40")
#' @export
sesameData_getManifestGRanges <- function(
    platform, genome = NULL, version = 1, decoy = FALSE,
    columns = c("mapQ_A", "mapAS_A")) {
    genome <- check_genomes(genome, platform)
    df <- sesameData_getManifestDF(platform, genome=genome, version=version)

    chrms <- df$CpG_chrm
    chrms <- chrms[!is.na(chrms)]
    if (genome %in% c("mm10","mm39","hg19","hg38")) {
        if (decoy) {
            chrms <- c(
                guess_chrmorder(chrms[!grepl("_", chrms)]),
                sort(unique(chrms[grepl("_", chrms)])))
        } else {
            chrms <- guess_chrmorder(chrms[!grepl("_", chrms)])
        }
    } else {
        chrms <- sort(unique(chrms))
    }
    df <- df[!is.na(df$CpG_chrm) & !is.na(df$CpG_beg) & !is.na(df$CpG_end),]
    df <- df[df$CpG_chrm %in% chrms,]
    gr <- GRanges(df$CpG_chrm,
        IRanges(df$CpG_beg+1, df$CpG_end),
        strand = ifelse(df$mapFlag_A=="0", "+", "-"),
        seqinfo = Seqinfo(chrms))
    mcols(gr) <- df[,columns]
    names(gr) <- df$Probe_ID
    sort(gr, ignore.strand = TRUE)
}

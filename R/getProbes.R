#' Get probes by genomic region
#'
#' The function takes a genomic coordinate and output the a vector of probes
#' on the specified platform that falls in the given genomic region.
#'
#' @param regs GRanges
#' @param chrm chromosome, when given regs are ignored
#' @param beg begin, 1 if omitted
#' @param end end, chromosome end if omitted
#' @param platform EPICv2, EPIC, HM450, ...
#' @param genome hg38, mm10, ... will infer if not given.
#' For additional mapping, download the GRanges object from
#' http://zwdzwd.github.io/InfiniumAnnotation
#' and provide the following argument
#' ..., genome = sesameAnno_buildManifestGRanges("downloaded_file"),...
#' to this function.
#' @return GRanges of selected probes
#' @importMethodsFrom IRanges subsetByOverlaps
#' @examples
#'
#' ## get probes in a region
#' sesameData_getProbesByRegion(
#'     GRanges('chr5', IRanges(135313937, 135419936)), platform = 'Mammal40')
#'
#' ## get all probes on chromosome 5
#' sesameData_getProbesByRegion("chr5", platform = "Mammal40")
#' @export
sesameData_getProbesByRegion <- function(
    regs, chrm = NULL, beg = 1, end = -1,
    platform = NULL, genome = NULL) {

    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)
    
    probes <- sesameData_getManifestGRanges(platform, genome=genome)

    if (!is.null(chrm)) {
        if (end < 0) {
            seqLength <- sesameDataGet(sprintf(
                "genomeInfo.%s", genome))$seqLength
            stopifnot(chrm %in% names(seqLength))
            end <- seqLength[chrm]
        }
        message(sprintf('Retrieve probes from %s:%d-%d.\n', chrm, beg, end))
        regs <- GenomicRanges::GRanges(chrm, IRanges::IRanges(beg, end))
    }
    subsetByOverlaps(probes, regs)
}

#' Get Probes by Chromosome
#'
#' @param chrms chromosomes to subset
#' @param platform EPIC, HM450, Mouse
#' @param genome hg38, mm10, ... will infer if not given.
#' For additional mapping, download the GRanges object from
#' http://zwdzwd.github.io/InfiniumAnnotation
#' and provide the following argument
#' ..., genome = sesameAnno_buildManifestGRanges("downloaded_file"),...
#' to this function.
#' @return GRanges of selected probes
#' @examples
#' Xprobes <- sesameData_getProbesByChromosome('chrX', "Mammal40")
#' @export
sesameData_getProbesByChromosome <- function(
    chrms, platform = NULL, genome = NULL) {

    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)

    gr <- sesameData_getManifestGRanges(platform, genome=genome)
    gr[as.character(GenomicRanges::seqnames(gr)) %in% chrms]
}

#' Get autosome probes
#'
#' @param platform 'EPIC', 'HM450' etc.
#' @param genome hg38, mm10, ... will infer if not given.
#' For additional mapping, download the GRanges object from
#' http://zwdzwd.github.io/InfiniumAnnotation
#' and provide the following argument
#' ..., genome = sesameAnno_buildManifestGRanges("downloaded_file"),...
#' to this function.
#' @return GRanges of autosome probes
#' @examples
#' auto_probes <- sesameData_getAutosomeProbes('Mammal40')
#' @export
sesameData_getAutosomeProbes <- function(
    platform = NULL, genome = NULL) {

    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)

    gr <- sesameData_getManifestGRanges(platform, genome=genome)
    gr[!(as.character(GenomicRanges::seqnames(gr)) %in% c("chrX","chrY"))]
}

#' Get Probes by Gene
#'
#' Get probes mapped to a gene. All transcripts for the gene are considered.
#' The function takes a gene name as appears in UCSC RefGene database. The
#' platform and reference genome build can be changed with `platform` and
#' `genome` options. The function returns a vector of probes that falls
#' into the given gene.
#'
#' @param gene_name gene name, if NULL return all genes
#' @param platform EPIC or HM450
#' @param upstream number of bases to expand upstream of target gene
#' @param downstream number of bases to expand downstream of target gene
#' @param genome hg38 or hg19
#' @return GRanges containing probes that fall into the given gene
#' @examples
#' probes <- sesameData_getProbesByGene(
#'     'DNMT3A', "Mammal40", upstream=500, downstream=500)
#' @export
sesameData_getProbesByGene <- function(
    gene_name = NULL, platform = NULL,
    upstream = 0, downstream = 0, genome = NULL) {

    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)
    
    txns <- sesameData_getTxnGRanges(genome)
    if (!is.null(gene_name)) {
        txns <- txns[GenomicRanges::mcols(txns)$gene_name == gene_name]
    }
    stopifnot(length(txns) > 0)
    
    up <- ifelse(as.vector(GenomicRanges::strand(txns)) == '-',
        downstream, upstream)
    dw <- ifelse(as.vector(GenomicRanges::strand(txns)) == '-',
        upstream, downstream)
    
    sesameData_getProbesByRegion(GRanges(
        as.vector(GenomicRanges::seqnames(txns)),
        IRanges(
            GenomicRanges::start(txns) - up,
            GenomicRanges::end(txns) + dw)),
        platform = platform, genome = genome)
}

#' Get Probes by Gene Transcription Start Site (TSS)
#'
#' Get probes mapped to a TSS. All transcripts for the gene are considered.
#' The function takes a gene name as appears in UCSC RefGene database. The
#' platform and reference genome build can be changed with `platform` and
#' `genome` options. The function returns a vector of probes that falls
#' into the TSS region of the gene.
#'
#' @param gene_name gene name, if NULL, return all TSS probes
#' @param platform EPIC, HM450, or MM285
#' @param upstream the number of base pairs to expand upstream the TSS
#' @param downstream the number of base pairs to expand downstream the TSS
#' @param genome hg38, hg19 or mm10
#' @return probes that fall into the given gene
#' @examples
#' probes <- sesameData_getProbesByTSS('DNMT3A', "Mammal40")
#' @export
sesameData_getProbesByTSS <- function(
    gene_name = NULL, platform = NULL,
    upstream = 1500, downstream = 1500, genome = NULL) {
    
    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)

    txns <- sesameData_getTxnGRanges(genome)
    if (!is.null(gene_name)) {
        txns <- txns[txns$gene_name == gene_name]
    }
    tss <- promoters(
        txns,
        upstream = upstream,
        downstream = downstream)

    sesameData_getProbesByRegion(tss, platform = platform, genome = genome)
}

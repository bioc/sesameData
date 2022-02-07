#' Get probes by genomic region
#'
#' The function takes a genomic coordinate and output the a vector of probes
#' on the specified platform that falls in the given genomic region.
#'
#' @param regs GRanges
#' @param chrm chromosome, when given regs are ignored
#' @param beg begin, 1 if omitted
#' @param end end, chromosome end if omitted
#' @param platform EPIC, HM450, ...
#' @param genome use default if not given
#' @return GRanges of selected probes
#' @importMethodsFrom IRanges subsetByOverlaps
#' @examples
#' library(GenomicRanges)
#' sesameData_getProbesByRegion(
#'     GRanges('chr5', IRanges(135313937, 135419936)), platform = 'Mammal40')
#' @export
sesameData_getProbesByRegion <- function(
    regs, chrm = NULL, beg = 1, end = -1,
    platform = NULL, genome = NULL) {

    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)
    
    probes <- sesameData_getManifestGRanges(platform, genome)

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
#' @param genome hg19, hg38, or mm10, inference by default
#' @return GRanges of selected probes
#' @examples
#' Xprobes <- sesameData_getProbesByChromosome('chrX', "Mammal40")
#' @export
sesameData_getProbesByChromosome <- function(
    chrms, platform = NULL, genome = NULL) {

    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)

    gr <- sesameData_getManifestGRanges(platform, genome)
    gr[as.character(GenomicRanges::seqnames(gr)) %in% chrms]
}

#' Get autosome probes
#'
#' @param platform 'EPIC', 'HM450' etc.
#' @param genome hg19, hg38, or mm10, inference by default
#' @return GRanges of autosome probes
#' @examples
#' auto_probes <- sesameData_getAutosomeProbes('Mammal40')
#' @export
sesameData_getAutosomeProbes <- function(
    platform = NULL, genome = NULL) {

    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)

    gr <- sesameData_getManifestGRanges(platform, genome)
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
#' @param gene_name gene name
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
    gene_name, platform = NULL,
    upstream = 0, downstream = 0, genome = NULL) {

    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)
    
    requireNamespace("GenomicRanges", quietly = TRUE)
    target.txns <- sesameData_getTranscriptsByGene(gene_name, genome)
    merged.exons <- GenomicRanges::reduce(unlist(target.txns))
    
    up <- ifelse(as.vector(GenomicRanges::strand(
        target.txns[[1]][1])) == '-', downstream, upstream)
    dw <- ifelse(as.vector(GenomicRanges::strand(
        target.txns[[1]][1])) == '-', upstream, downstream)
    
    sesameData_getProbesByRegion(GRanges(
        as.character(GenomicRanges::seqnames(merged.exons[1])),
        IRanges(
            min(GenomicRanges::start(merged.exons)) - up,
            max(GenomicRanges::end(merged.exons)) + dw)),
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
#' @param gene_name gene name
#' @param platform EPIC, HM450, or MM285
#' @param upstream the number of base pairs to expand upstream the TSS
#' @param downstream the number of base pairs to expand downstream the TSS
#' @param genome hg38, hg19 or mm10
#' @return probes that fall into the given gene
#' @examples
#' probes <- sesameData_getProbesByTSS('DNMT3A', "Mammal40")
#' @export
sesameData_getProbesByTSS <- function(
    gene_name, platform = NULL,
    upstream = 1500, downstream = 1500, genome = NULL) {
    
    platform <- sesameData_check_platform(platform)
    genome <- sesameData_check_genome(genome, platform)

    txns <- sesameData_getTxnGRanges(genome)
    tss <- promoters(
        txns[txns$gene_name == gene_name],
        upstream = upstream,
        downstream = downstream)

    sesameData_getProbesByRegion(tss, platform = platform, genome = genome)
    ## target.txns <- sesameData_getTranscriptsByGene(gene_name, genome)
    ## tss <- GenomicRanges::reduce(unlist(GenomicRanges::GRangesList(
    ##     lapply(target.txns, function(txn) {
    ##         tss1 <- ifelse(
    ##             as.vector(GenomicRanges::strand(txn))[1] == '-',
    ##             max(GenomicRanges::end(txn)), min(GenomicRanges::start(txn)))
    ##         up <- ifelse(as.vector(
    ##             GenomicRanges::strand(txn))[1] == '-', dwstream, upstream)
    ##         dw <- ifelse(as.vector(
    ##             GenomicRanges::strand(txn))[1] == '-', upstream, dwstream)
    ##         GenomicRanges::GRanges(
    ##             as.vector(GenomicRanges::seqnames(txn))[1],
    ##             ranges = IRanges::IRanges(start=tss1-up, end=tss1+dw))
    ## }))))

    ## probes1 <- subsetByOverlaps(sesameDataGet(paste0(
    ##     platform, '.probeInfo'))[[paste0('mapped.probes.',genome)]], tss)
    
    ## if (length(probes1)>0) {
    ##     probes1$gene <- gene_name
    ## }
    ## probes1
}
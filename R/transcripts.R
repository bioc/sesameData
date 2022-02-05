
read_GENCODE_gtf <- function(x) {
    ## https://www.gencodegenes.org/pages/data_format.html
    download.file(x, sprintf("%s/gtf.gz", tempdir()), mode="wb")
    gtf <- read_tsv(sprintf("%s/gtf.gz", tempdir()), comment="#",
        col_names = c("chrm", "source", "feature_type", "start", "end",
            "score_not_used", "strand", "cds_phase", "additional"),
        col_types=cols(
            start=col_integer(),
            end=col_integer(),
            .default=col_character()))
    gtf
}

read_GENCODE_gtf_transcript <- function(gtf) {
    g <- gtf[gtf$feature_type == "transcript", ]
    g$transcript_id <- str_match(g$additional, 'transcript_id "([^"]*)"')[,2]
    g$transcript_name <- str_match(
        g$additional, 'transcript_name "([^"]*)"')[,2]
    g$transcript_type <- str_match(
        g$additional, 'transcript_type "([^"]*)"')[,2]
    g$gene_id <- str_match(g$additional, 'gene_id "([^"]*)"')[,2]
    g$gene_name <- str_match(g$additional, 'gene_name "([^"]*)"')[,2]
    g$gene_type <- str_match(g$additional, 'gene_type "([^"]*)"')[,2]
    ## there is also transcript_name, is it useful? not included
    g$level <- str_match(g$additional, 'level ([^;]*)')[,2]
    ## gene_status and transcript_status are obsolete after 25 and M11
    g
}

read_GENCODE_gtf_exon <- function(gtf) {
    g <- gtf[gtf$feature_type == "exon", ]
    g$transcript_id <- str_match(g$additional, 'transcript_id "([^"]*)"')[,2]
    g$exon_id <- str_match(g$additional, 'exon_id "([^"]*)"')[,2]
    g$exon_number <- str_match(g$additional, 'exon_number ([^;]*)')[,2]
    g
}

guess_chrmorder <- function(chrms) {
    chrms1 <- chrms[!(chrms %in% c("chrX","chrY","chrM"))]
    paste0("chr",c(as.character(seq_len(max(as.integer(str_replace(
        sort(unique(chrms1)), "chr", "")), na.rm=TRUE))), c("X","Y","M")))
}

#' build GENCODE gtf
#'
#' @param x GENCODE ftp url
#' @return GRangesList
#' @importFrom readr read_tsv
#' @importFrom readr cols
#' @importFrom readr col_integer
#' @importFrom readr col_character
#' @importFrom GenomeInfoDb Seqinfo
#' @importFrom IRanges IRanges
#' @import stringr
#' @import GenomicRanges
build_GENCODE_gtf <- function(x) {

    gtf <- read_GENCODE_gtf(x)

    ## transcript
    g1 <- read_GENCODE_gtf_transcript(x)
    stopifnot(length(g1$transcript_id) == length(unique(g1$transcript_id)))

    ## exon
    g2 <- read_GENCODE_gtf_exon(x)
    chrms <- guess_chrmorder(g2$chrm)
    gr <- GRanges(seqnames = g2$chrm, ranges=IRanges(g2$start, g2$end),
        strand = g2$strand, seqinfo=Seqinfo(chrms))
    mcols(gr)$exon_number <- as.integer(g2$exon_number)
    names(gr) <- g2$exon_id
    grl <- GRangesList(split(gr, g2$transcript_id)) # slow
    stopifnot(length(grl) == length(g1$transcript_id))
    stopifnot(all(sort(names(grl)) == sort(g1$transcript_id)))

    ## CDS
    g3 <- gtf[gtf$feature_type == "CDS", ]
    g3$transcript_id <- str_match(g3$additional, 'transcript_id "([^"]*)"')[,2]
    g1$cdsStart <- g3$start[match(g1$transcript_id, g3$transcript_id)]
    g1$cdsEnd <- g3$end[match(g1$transcript_id, g3$transcript_id)]

    ## put together
    g1 <- g1[order(factor(g1$chrm, levels=chrms), g1$start),]
    grl <- grl[g1$transcript_id]
    mcl <- g1[match(names(grl), g1$transcript_id), c(
        "chrm", "start", "end", "strand",
        "transcript_id", "transcript_type", "transcript_name",
        "gene_name", "gene_id", "gene_type", "source", "level",
        "cdsStart", "cdsEnd")]
    colnames(mcl)[2] <- "transcript_start"
    colnames(mcl)[3] <- "transcript_end"
    colnames(mcl)[4] <- "transcript_strand"
    mcols(grl) <- mcl
    grl
}

#' convert GRangesList to transcript GRanges
#'
#' @param grl GRangesList object
#' @return a GRanges object
#' @examples
#' full_model <- sesameDataGet("genomeInfo.mm10")$txns
#' txns <- sesameData_getTxnGRanges(full_model)
#' ## get verified protein-coding
#' txns <- txns[(txns$transcript_type == "protein_coding" & txns$level <= 2)]
#'
#' @export
sesameData_getTxnGRanges <- function(grl) {
    mcl <- mcols(grl)
    gr <- GRanges(
        seqnames = mcl$chrm, ranges = IRanges(
            mcl$transcript_start, mcl$transcript_end),
        strand = mcl$transcript_strand, seqinfo = seqinfo(grl))
    names(gr) <- mcl$transcript_id
    mcols(gr) <- mcl[,colnames(mcl)[!(colnames(mcl) %in% c(
        "chrm","transcript_id", "transcript_start",
        "transcript_end","transcript_strand"))]]
    gr <- sort(gr, ignore.strand = TRUE)
    gr
}

#' convert transcript GRanges to gene GRanges
#'
#' @param txns GRanges object
#' @return a GRanges object
#' @examples
#' full_model <- sesameDataGet("genomeInfo.mm10")$txns
#' txns <- sesameData_getTxnGRanges(full_model)
#' genes <- sesameData_getGeneGRanges(txns)
#' 
#' @export
sesameData_getGeneGRanges <- function(txns) {
    gene_ids <- unique(txns$gene_id)
    gene2starts <- split(start(txns), txns$gene_id)[gene_ids]
    gene2ends <- split(end(txns), txns$gene_id)[gene_ids]
    genes <- GRanges(seqnames = seqnames(txns)[match(gene_ids, txns$gene_id)],
        IRanges(
            vapply(gene2starts, min, integer(1)),
            vapply(gene2ends, max, integer(1))),
        strand = strand(txns)[match(gene_ids, txns$gene_id)])

    names(genes) <- gene_ids
    mcols(genes)$gene_name <- txns$gene_name[match(names(genes), txns$gene_id)]
    mcols(genes)$gene_type <- txns$gene_type[match(names(genes), txns$gene_id)]
    sort(genes, ignore.strand=TRUE)
}




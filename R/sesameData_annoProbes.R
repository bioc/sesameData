## Helper function to extract labels from overlaps
## Extracts annotation labels from overlapping regions, either using region
## names or a specific metadata column. Returns both the labels and the
## output column name to use.
.getLabelsFromOverlaps <- function(regs, hits, column, out_name) {
    if (is.null(column)) {
        # Use region names as labels when no column specified
        label <- names(regs[subjectHits(hits)])
        if (is.null(out_name)) { out_name <- "anno" }
    } else {
        # Use specified metadata column for labels
        stopifnot(column %in% colnames(mcols(regs)))
        label <- mcols(regs[subjectHits(hits)])[[column]]
        if (is.null(out_name)) { out_name <- column }
    }
    list(label = label, out_name = out_name)
}

## Helper function to collapse annotations
## When a probe overlaps multiple regions, this function either picks the
## first annotation (chooseOne=TRUE) or concatenates all unique annotations
## with a separator. Probes without overlaps get NA.
.collapseAnnotations <- function(
    probes, hits, label, out_name, chooseOne, sep) {
    if (chooseOne) {
        # Take only the first annotation for each probe
        pid2label <- vapply(split(label, queryHits(hits)),
            function(x) x[1], character(1))
    } else {
        # Concatenate all unique annotations with separator
        pid2label <- vapply(split(label, queryHits(hits)),
            function(x) paste0(unique(x), collapse=sep), character(1))
    }
    # Initialize all probes with NA, then fill in the annotations
    mcols(probes)[[out_name]] <- NA
    mcols(probes[as.integer(names(pid2label))])[[out_name]] <- pid2label
    probes
}

#' Annotate Probes by Probe ID
#'
#' This function annotates probes based on genomic coordinate overlaps with
#' provided genomic regions (GRanges). Columns in the manifests will be added
#' to the annotation. Please note that if unfound, the annotation will be NA.
#' The probe will always be kept in the output.
#'
#' For annotation by probe ID using KYCG databases (rather than by genomic
#' coordinates), see \code{knowYourCG::annoProbes()}
#'
#' @param Probe_IDs a character vector of probe IDs
#' @param regs a GenomicRanges::GRanges object against which
#' probes will be annotated, default to genes if not given
#' @param collapse whether to collapse multiple regs into one
#' @param chooseOne choose an arbitrary annotation if multiple exist
#' default to FALSE. which concatenates all with ","
#' @param sep the delimiter for collapsing
#' @param column which column in regs to annotate, if not given
#' return all overlapping probes
#' @param return_ov_probes if TRUE will return overlapping
#' probes in a GRanges object.
#' @param return_ov_features if TRUE will return overlapping
#' features in a GRanges object.
#' @param out_name column header of the annotation, use column if not given
#' @param platform EPIC, MM285 etc. will infer from Probe_IDs if not given
#' @param genome hg38, mm10, ... will infer if not given.
#' For additional mapping, download the GRanges object from
#' http://zwdzwd.github.io/InfiniumAnnotation
#' and provide the following argument
#' ..., genome = sesameAnno_buildManifestGRanges("downloaded_file"),...
#' to this function.
#' @param silent suppress messages
#' @importFrom GenomicRanges findOverlaps
#' @importFrom GenomicRanges mcols
#' @importFrom S4Vectors subjectHits
#' @importFrom S4Vectors queryHits
#' @return a GRanges with annotated column
#' If a probe has no overlap with regs, it will be included in the results
#' with NA. But if a probe is not included in the manifest (due to
#' mappability), it won't be included in the results.
#' 
#' @examples
#' library(GenomicRanges)
#' sesameDataCache(c(
#'     "genomeInfo.mm10", "MM285.address",
#'     "genomeInfo.hg38", "Mammal40.address"))
#'
#' ## Example 1: Basic usage - annotate with gene names (default)
#' ## When regs=NULL, function defaults to gene annotation
#' probes <- c("cg14620903", "cg22464003")
#' anno <- sesameData_annoProbes(probes)
#' ## Returns GRanges with gene_name column
#'
#' ## Example 2: Annotate mouse probes with promoter regions
#' regs <- sesameData_getTxnGRanges("mm10")
#' Probe_IDs <- names(sesameData_getManifestGRanges("MM285"))[1:100]
#' anno <- sesameData_annoProbes(Probe_IDs, promoters(regs), column="gene_name")
#' ## Probes overlapping promoters are annotated with gene names
#'
#' ## Example 3: Get features that overlap with probes
#' ## return_ov_features=TRUE returns the overlapping genomic features
#' genes <- sesameData_getTxnGRanges("hg38", merge2gene = TRUE)
#' ov_genes <- sesameData_annoProbes(
#'     c("cg14620903","cg22464003"), genes, return_ov_features=TRUE)
#' ## Returns GRanges of genes overlapping the probes
#'
#' ## Example 4: Get probes that overlap with features
#' ## return_ov_probes=TRUE returns only overlapping probes
#' ov_probes <- sesameData_annoProbes(
#'     c("cg14620903","cg22464003"), genes, return_ov_probes=TRUE)
#' ## Returns GRanges of probes that overlap genes
#'
#' ## Example 5: Control annotation collapsing behavior
#' ## collapse=TRUE (default): multiple annotations concatenated with separator
#' anno_collapsed <- sesameData_annoProbes(
#'     c("cg14620903","cg22464003"), genes, column="gene_name",
#'     collapse=TRUE, sep=";")
#' ## Multiple overlapping genes listed as "GENE1;GENE2;GENE3"
#'
#' ## collapse=FALSE: each probe-feature overlap gets separate entry
#' anno_expanded <- sesameData_annoProbes(
#'     c("cg14620903","cg22464003"), genes, column="gene_name",
#'     collapse=FALSE)
#' ## Probes with multiple overlaps appear multiple times
#'
#' ## Example 6: Choose only first annotation when multiple exist
#' anno_one <- sesameData_annoProbes(
#'     c("cg14620903","cg22464003"), genes, column="gene_name",
#'     chooseOne=TRUE)
#' ## Each probe gets only the first overlapping gene
#'
#' ## Example 7: Annotate with custom genomic regions
#' custom_regs <- GRanges(
#'     seqnames = c("chr5", "chr5"),
#'     ranges = IRanges(start = c(10000, 135350870),
#'                      end = c(135350866, 145369531)),
#'     feature_type = c("enhancer", "silencer"))
#' anno_custom <- sesameData_annoProbes(
#'     c("cg14620903","cg22464003"), custom_regs,
#'     column="feature_type", genome="hg38")
#'
#' ## Note: For annotation by probe ID using KYCG databases
#' ## (rather than genomic coordinates), see knowYourCG::annoProbes()
#' @export
sesameData_annoProbes <- function(Probe_IDs, regs = NULL,
    collapse = TRUE, chooseOne = FALSE, column = NULL, sep=",",
    return_ov_probes = FALSE, return_ov_features = FALSE,
    out_name = NULL, platform = NULL, genome = NULL, silent = FALSE) {

    # Validate input
    stopifnot(is.character(Probe_IDs))

    # Infer platform from probe IDs if not provided
    if(is.null(platform)) {
        platform <- inferPlatformFromProbeIDs(Probe_IDs, silent = silent)
    }

    # Set up default genomic regions (genes) if not provided
    if (is.null(regs)) {
        if (is.null(genome)) {
            genome <- sesameData_check_genome(NULL, platform)
        }
        regs <- sesameData_getTxnGRanges(genome)
        if (is.null(column)) column <- "gene_name"
    }

    # Get probe coordinates from manifest
    gr <- sesameData_getManifestGRanges(platform, genome = genome)
    in_mft <- Probe_IDs %in% names(gr)
    if (sum(!in_mft) > 0) {
        warning(sprintf(
            "%d probes out of manifest were excluded.", sum(!in_mft)))
    }

    # Subset to requested probes and find overlaps with regions
    probes <- gr[Probe_IDs[in_mft]]
    if (length(probes) == 0) { return(probes); }
    hits <- findOverlaps(probes, regs, ignore.strand = TRUE)

    # Return early if only requesting overlapping probes or features
    if (return_ov_probes) {
        return(probes[unique(queryHits(hits))])
    } else if (return_ov_features) {
        return(regs[unique(subjectHits(hits))])
    }

    # Extract labels from overlapping regions
    label_info <- .getLabelsFromOverlaps(regs, hits, column, out_name)
    label <- label_info$label
    out_name <- label_info$out_name

    # Annotate probes with labels
    if (collapse) {
        # Collapse multiple annotations per probe into single entries
        probes <- .collapseAnnotations(
            probes, hits, label, out_name, chooseOne, sep)
    } else {
        # Keep all probe-region pairs separate (probes may be duplicated)
        unfound <- probes[!(seq_along(probes) %in% queryHits(hits))]
        if (length(unfound) > 0) { mcols(unfound)[[out_name]] <- NA }
        found <- probes[queryHits(hits)]
        if (length(found) > 0) { mcols(found)[[out_name]] <- label }
        probes <- sort(c(unfound, found))
    }
    probes
}

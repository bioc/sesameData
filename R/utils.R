
#' Extend a GRanges
#'
#' source: https://support.bioconductor.org/p/78652/
#' @param gr a GenomicRanges::GRanges
#' @param upstream distance to expand upstream
#' @param downstream distance to expand downstream
#' @importFrom GenomicRanges trim
#' @importFrom GenomicRanges start
#' @importFrom GenomicRanges end
#' @return a GenomicRanges::GRanges
#' @examples
#' library(GenomicRanges)
#' extend(GRanges("chr1",IRanges(10,20), "-"), upstream = 5, downstream = 3)
#' @export
extend <- function(gr, upstream=0, downstream=0) {
    if (any(strand(gr) == "*")) {
        warning("'*' ranges were treated as '+'") }

    on_plus <- strand(gr) == "+" | strand(gr) == "*"
    ranges(gr) <- IRanges(
        start(gr) - ifelse(on_plus, upstream, downstream),
        end(gr) + ifelse(on_plus, downstream, upstream))
    trim(gr)
}

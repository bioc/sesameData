
valid_url <- function(url_in,t=2){
    con <- url(url_in)
    check <- suppressWarnings(try(open.connection(
        con,open="rt",timeout=t),silent=TRUE)[1])
    suppressWarnings(try(close.connection(con),silent=TRUE))
    ifelse(is.null(check),TRUE,FALSE)
}

download_file <- function(title, version, dest_dir) {
    dest_file <- sprintf("%s/%s", dest_dir, title)
    dir.create(sprintf("%s/%s", dest_dir, dirname(title)),
        recursive = TRUE, showWarnings = FALSE)
    url <- sprintf("%s%d/raw/main/%s", anno_base, version, title)
    stopifnot(valid_url(url))
    download.file(url, dest_file, mode="wb")
    stopifnot(file.exists(dest_file) && file.info(dest_file)$size > 0)
    
    invisible(list(
        url = url,
        dest_dir = dest_dir,
        dest_file = dest_file,
        file_name = title))
}

#' Retrieve additional annotation files
#'
#' From the Infinium annotation website associated github repo
#' e.g., 
#' https://github.com/zhou-lab/InfiniumAnnotationV1
#'
#' The default version number should always work. One need to
#' refer to the actual repo to see which one of the other versions
#' also work.
#' 
#' See also
#' http://zwdzwd.github.io/InfiniumAnnotation
#'
#' @param title title of the annotation file
#' @param version version number
#' @param dest_dir if not NULL, download to this directory
#' @return annotation file
#' @examples
#'
#' ## avoided testing these function as they use external resources
#' if (FALSE) {
#' mft <- sesameData_getAnno("HM27/HM27.hg19.manifest.tsv.gz")
#' annoI <- sesameData_getAnno("EPIC/EPIC.hg19.typeI_overlap_b151.rds")
#' annoS <- sesameData_getAnno("EPIC/EPIC.hg19.snp_overlap_b151.rds")
#' sesameData_getAnno("test/3999492009_R01C01_Grn.idat", dest_dir = tempdir())
#' }
#' 
#' @export
sesameData_getAnno <- function(
    title, version = anno_base_default_version, dest_dir = NULL) {

    if (!is.null(dest_dir)) {
        return(download_file(title, version, dest_dir))
    }
    
    download_path <- sprintf("%s%d/raw/main/%s", anno_base, version, title)
    if (!valid_url(download_path)) {
        message(sprintf("File not available %s.", download_path))
        return(NULL)
    }

    if (endsWith(title, ".tsv.gz")) {
        z <- gzcon(url(download_path))
        raw <- textConnection(readLines(z))
        close(z)
        message("Retrieving annotation from ",download_path,
            "... ", appendLF = FALSE)
        anno <- read.table(raw, header=TRUE, sep="\t")
        close(raw)
        message("Done.")
    } else if (endsWith(title, ".rds")) {
        message("Retrieving annotation from ",download_path,
            "... ", appendLF = FALSE)
        anno <- readRDS(url(download_path))
        message("Done.")
    }
    anno
}


sesameDataDownloadFile <- function(file_name, dest_file, base = alt_base) {
    url <- sprintf(
        "%s/sesameData/raw/%s", base, file_name)
    if (!valid_url(url)) {
        message(sprintf("Resource not available %s.", url))
        return(NULL)
    }

    download.file(url, dest_file, mode="wb")
    url
}


#' Download auxiliary data for sesame function and documentation
#' 
#' @param file_name name of file to download
#' @param dest_dir directory to hold downloaded file.
#' use the temporary directory if not given
#' @return a list with url, dest_dir, dest_file and file_name
#' @examples
#' 
#' ## We avoided testing this function to ensure version consistency.
#' if(FALSE) { sesameDataDownload("3999492009_R01C01_Grn.idat") }
#' @export
sesameDataDownload <- function(file_name, dest_dir=NULL) {
    if (is.null(dest_dir)) {
        dest_dir <- tempdir()
    }
    dest_file <- sprintf("%s/%s", dest_dir, file_name)

    url <- sesameDataDownloadFile(file_name, dest_file, base = alt_base)
    if (!file.exists(dest_file) || file.info(dest_file)$size == 0) { # backup
        url <- sesameDataDownloadFile(file_name, dest_file, base=alt_base2)
    }
    
    list(
        url = url,
        dest_dir = dest_dir,
        dest_file = dest_file,
        file_name = file_name)
}

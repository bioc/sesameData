
## update this everytime new data is uploaded
## use sesameDataListDates to check
## checking multiple dates are not very efficient,
## should consider a lookup table
latest_date = c("2020-08-24", "2020-10-27")
cacheEnv <- new.env()
alt_base = 'https://zwdzwd.s3.amazonaws.com/sesameData'

## fall back data retrieval in case ExperimentHub is down
.sesameDataGet2 <- function(title) {
    tryCatch(
        load(url(sprintf('%s/%s.rda', alt_base, title)), envir=cacheEnv),
        error = function(cond) {
            message("sesameDataGet2 fails:")
            message(cond, appendLF = TRUE)
            return(FALSE)
        },
        warning = function(cond) {
            message("sesameDataGet2 causes a warning:")
            message(cond, appendLF = TRUE)
            return(FALSE)
        })
    TRUE
}

.sesameDataGet <- function(title, dateAdded = latest_date) {
    for (dateAdded1 in dateAdded) {
        key <- paste0(title,'|',dateAdded1)
        ## only reload if not in existing environment
        if (!exists(title, envir=cacheEnv, inherits=FALSE)) {
            eh <- query(ExperimentHub(localHub=TRUE), 'sesameData')
            obj_id <- which(eh$title == title & eh$rdatadateadded == dateAdded1)
            if (length(obj_id)==1) {
                assign(title, eh[[obj_id]], envir=cacheEnv)
            } else {
                ## maybe it's older version, try cache that file
                ## it doesn't work properly under parallel
                ## one can sesameDataCacheAll(prev_date) to avoid issue
                eh <- query(ExperimentHub(localHub=FALSE), 'sesameData')
                obj_id <- which(
                    eh$title == title & eh$rdatadateadded == dateAdded1)
                if (length(obj_id)==1) {
                    cache(eh[obj_id])
                    assign(title, eh[[obj_id]], envir=cacheEnv)
                }
            }
        }
    }
    if (!exists(title, envir=cacheEnv, inherits=FALSE) &&
            !.sesameDataGet2(title)) {
        stop(sprintf(
            "%s doesn't exist. Try: sesameDataCacheAll(\"%s\")",
            title, dateAdded1))
    }
    return(get(title, envir=cacheEnv, inherits=FALSE))
}

#' Get SeSAMe data
#'
#' @param title title of the data
#' @param dateAdded version of the data by date added
#' @param verbose whether to output ExperimentHub message
#' @return data object
#' @import ExperimentHub
#' @import AnnotationHub
#' @examples
#' 
#' result <- sesameDataGet('genomeInfo.hg38')
#' @export
sesameDataGet <- function(title, verbose=FALSE, dateAdded = latest_date) {
    if (verbose) {
        .sesameDataGet(title, dateAdded = dateAdded)
    } else {
        suppressMessages(
            log <- capture.output(
                obj <- .sesameDataGet(title, dateAdded = dateAdded)));
        obj
    }
}

#' @import curl
has_internet <- function(){
    !is.null(curl::nslookup("r-project.org", error = FALSE))
}

#' List all SeSAMe data
#'
#' @param dateAdded version of the data by date added, if "all", show all dates
#' @return all titles from SeSAMe Data
#' @examples
#' sesameDataList()
#' @export
sesameDataList <- function(dateAdded = latest_date) {
    if (has_internet()) {
        eh <- query(ExperimentHub(), 'sesameData')
    } else {
        eh <- query(ExperimentHub(localHub = TRUE), 'sesameData')
    }
    if (dateAdded[1] == "all") {
        eh$title
    } else {
        eh$title[eh$rdatadateadded %in% dateAdded]
    }
}

#' List all versions of SeSAMe data
#' 
#' @return sorted unique dates of SeSAMe Data
#' @examples
#' sesameDataListDates()
#' @export
sesameDataListDates <- function() {
    if (has_internet()) {
        eh <- query(ExperimentHub(), 'sesameData')
    } else {
        eh <- query(ExperimentHub(localHub = TRUE), 'sesameData')
    }
    sort(unique(eh$rdatadateadded))
}

#' Cache all SeSAMe data
#'
#' @param dateAdded version of the data by date added, if "all", cache all dates
#' @param showProgress whether to show progress of download
#' @return TRUE
#' @import ExperimentHub
#' @import AnnotationHub
#' @examples
#' sesameDataCacheAll()
#' @export
sesameDataCacheAll <- function(dateAdded = latest_date, showProgress = FALSE) {
    setExperimentHubOption(arg="MAX_DOWNLOADS", 100)
    tryCatch(
        {
            ## load meta data
            if (showProgress) {
                eh <- query(ExperimentHub(), 'sesameData')
            } else {
                suppressMessages(log <- capture.output(
                    eh <- query(ExperimentHub(), 'sesameData')))
            }
            
            ## restrict to specified date
            if (dateAdded[1] != "all") {
                eh <- eh[eh$rdatadateadded %in% dateAdded]
            }
            
            ## load actual data
            if (showProgress) {
                cache(eh)
            } else {
                suppressMessages(log <- capture.output(cache(eh)))
            }
        },
        error = function(cond) {
            message("ExperimentHub Caching fails:")
            message(cond, appendLF = TRUE)
            return(FALSE)
        },
        warning = function(cond) {
            message("ExperimentHub Caching causes a warning:")
            message(cond, appendLF = TRUE)
            return(FALSE)
        })

    TRUE
}

#' Retrieve manifest file from the supporting website
#' at http://zwdzwd.github.io/InfiniumAnnotation
#' and https://github.com/zhou-lab/InfiniumAnnotation
#'
#' @param platform Infinium platform
#' @param refversion human reference version, irrelevant for mouse array
#' @param version manifest version, default to the latest/current.
#' @param probeType cg, ch or rs, default to all probes
#' @param designType I (Infinium-I) or II (Infinium-II), default to both
#' @return manifest file of requested probes
#' @examples
#'
#' mft <- sesameDataPullManifest('HM27', 'hg38')
#' 
#' @export
sesameDataPullManifest <- function(
    platform=c('EPIC','HM450','HM27','MM285'),
    refversion=c('hg19','hg38','mm10'),
    version="current",
    probeType=c('all','cg','ch','rs'),
    designType=c('all','I','II')) {

    platform <- match.arg(platform)
    refversion <- match.arg(refversion)
    probeType <- match.arg(probeType)
    designType <- match.arg(designType)

    if (platform == 'EPIC') { stopifnot(refversion %in% c('hg19','hg38')); }
    if (platform == 'MM285') { stopifnot(refversion == 'mm10'); }
    
    download_path <-
        sprintf(
            paste0(
                'https://zwdzwd.s3.amazonaws.com/InfiniumAnnotation/',
                '%s/%s/%s.%s.manifest.rds'),
            version, platform, platform, refversion)
    
    cat("Retrieving manifest from ",download_path, "... ")
    mft <- readRDS(url(download_path))
    cat("Done.\n")
    if (probeType[1] != 'all')
        mft <- mft[mft$probeType == probeType]

    if (designType[1] != 'all')
        mft <- mft[mft$designType == designType]
    
    mft
}

#' Retrieve variant annotation file for explicit rs probes
#' from the supporting website
#' at http://zwdzwd.github.io/InfiniumAnnotation
#'
#' @param platform Infinium platform
#' @param refversion human reference version, irrelevant for mouse array
#' @param version manifest version, default to the latest/current.
#' @return variant annotation file of explicit rs probes
#' @examples
#'
#' annoS <- sesameDataPullVariantAnno_SNP('EPIC', 'hg38')
#' 
#' @export
sesameDataPullVariantAnno_SNP <- function(
    platform = c('EPIC'),
    refversion = c('hg19','hg38'),
    version = '20200704') {

    platform <- match.arg(platform)
    refversion <- match.arg(refversion)

    download_path <-
        sprintf(
            paste0(
                'https://zwdzwd.s3.amazonaws.com/InfiniumAnnotation/',
                '%s/%s/%s.%s.snp_overlap_b151.rds'),
            version, platform, platform, refversion)

    cat("Retrieving SNP annotation from ",download_path, "... ")
    anno <- readRDS(url(download_path))
    cat("Done.\n")
    
    anno
}

#' Retrieve variant annotation file for Infinium-I probes
#' from the supporting website
#' at http://zwdzwd.github.io/InfiniumAnnotation
#'
#' @param platform Infinium platform
#' @param refversion human reference version, irrelevant for mouse array
#' @param version manifest version, default to the latest/current.
#' @return variant annotation file of infinium I probes
#' @examples
#'
#' annoI <- sesameDataPullVariantAnno_InfiniumI('EPIC', 'hg38')
#' 
#' @export
sesameDataPullVariantAnno_InfiniumI <- function(
    platform = c('EPIC'),
    refversion = c('hg19','hg38'),
    version = '20200704') {

    platform <- match.arg(platform)
    refversion <- match.arg(refversion)

    download_path <-
        sprintf(
            paste0(
                'https://zwdzwd.s3.amazonaws.com/InfiniumAnnotation/',
                '%s/%s/%s.%s.typeI_overlap_b151.rds'),
            version, platform, platform, refversion)

    cat("Retrieving SNP annotation from ",download_path, "... ")
    anno <- readRDS(url(download_path))
    cat("Done.\n")
    
    anno
}

#' Download auxiliary data for sesame function and documentation
#'
#' @param file_name name of file to download
#' @param dest_dir directory to hold downloaded file.
#' use the temporary directory if not given
#' @return a list with url, dest_dir, dest_file and file_name
#' @examples
#' \dontrun{
#' sesameDataDownload("3999492009_R01C01_Grn.idat")
#' }
sesameDataDownload = function(file_name, dest_dir=NULL) {
    if (is.null(dest_dir)) {
        dest_dir = tempdir()
    }
    dest_file = sprintf("%s/%s", tempdir(), file_name)
    url = sprintf("https://zwdzwd.s3.amazonaws.com/sesameData/raw/%s", file_name)
    download.file(url, dest_file)
    list(
        url = url,
        dest_dir = dest_dir,
        dest_file = dest_file,
        file_name = file_name)
}


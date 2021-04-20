
## the following lookup table will be updated on every new release
eh_id_lookup = c(
    ## eh = query(ExperimentHub(localHub=TRUE), c("sesameData", "v1.7.2"))
    ## data.frame(name=eh$title, eh=names(eh))
    "EPIC.1.LNCaP"="EH3659",
    "EPIC.5.normal"="EH3660",
    ## "HM450.1.TCGA.PAAD"="EH3661",
    "HM450.10.TCGA.PAAD.normal"="EH3662",
    "HM450.10.TCGA.BLCA.normal"="EH3663",
    "HM450.76.TCGA.matched"="EH3664",
    "genomeInfo.hg19"="EH3665",
    "genomeInfo.hg38"="EH3666",
    ## "EPIC.address"="EH3667",
    ## "HM450.address"="EH3668",
    ## "HM27.address"="EH3669",
    "EPIC.hg19.manifest"="EH3670",
    "EPIC.hg38.manifest"="EH3671",
    "HM27.hg19.manifest"="EH3672",
    "HM27.hg38.manifest"="EH3673",
    "HM450.hg19.manifest"="EH3674",
    "HM450.hg38.manifest"="EH3675",
    "EPIC.probeInfo"="EH3676",
    "HM450.probeInfo"="EH3677",
    "HM27.probeInfo"="EH3678",
    "leukocyte.betas"="EH3679",
    "ref.methylation"="EH3680",
    "age.inference"="EH3681",
    "ethnicity.inference"="EH3682",
    "sex.inference"="EH3683",
    "detection.stats"="EH3684",
    ## eh = query(ExperimentHub(localHub=TRUE), c("sesameData", "v1.9.1"))
    ## data.frame(name=eh$title, eh=names(eh))
    ## "MM285.address"="EH4678",
    ## "Mammal40.address"="EH4679",
    "MM285.mm10.manifest"="EH4680",
    "EPIC.address"="EH5963",
    "HM27.address"="EH5964",
    "HM450.1.TCGA.PAAD"="EH5965",
    "HM450.address"="EH5966",
    "Mammal40.address"="EH5967",
    "MM285.1.NOD.FrontalLobe"="EH5968",
    "MM285.10.tissue"="EH5969",
    "MM285.address"="EH5970",
    "MM285.clock347"="EH5971",
    "MM285.mm10.manifest"="EH5972",
    "MM285.strain.snp.table"="EH5973",
    "MM285.tissueSignature"="EH5974"
)

cacheEnv <- new.env()

## fall back data retrieval in case ExperimentHub is down
.sesameDataGet2 <- function(title) {
    eh_id = eh_id_lookup[title]
    if (is.na(eh_id)) {
        eh_id = title
    }
    message("ExperimentHub not responding. Using backup.")
    alt_base = 'https://zwdzwd.s3.amazonaws.com/sesameData'
    tryCatch(
        assign(eh_id, get(load(url(sprintf('%s/%s.rda', alt_base, title)))),
            envir=cacheEnv),
        error = function(cond) {
            message("sesameDataGet2 fails:")
            message(cond)
            return(FALSE)
        },
        warning = function(cond) {
            message("sesameDataGet2 causes a warning:")
            message(cond)
            return(FALSE)
        })
    TRUE
}

.sesameDataGet <- function(title) {
    eh_id = eh_id_lookup[title]
    if (is.na(eh_id)) { # missing from lookup table
        eh_id = title   # use title itself
    } else {            # present in lookup table
        ## try ExperimentHub
        if (!exists(eh_id, envir=cacheEnv, inherits=FALSE)) {
            eh = query(ExperimentHub(localHub=TRUE), 'sesameData')
            assign(eh_id, eh[[eh_id]], envir=cacheEnv)
        }
    }

    ## try backup
    if (!exists(eh_id, envir=cacheEnv, inherits=FALSE)) {
        if (!.sesameDataGet2(title)) {
            stop(sprintf(
                "%s doesn't exist. Try: sesameDataCacheAll(\"%s\")", title))
        }
    }
    return(get(eh_id, envir=cacheEnv, inherits=FALSE))
}

#' Get SeSAMe data
#'
#' @param title title of the data
#' @param verbose whether to output ExperimentHub message
#' @return data object
#' @import ExperimentHub
#' @import AnnotationHub
#' @examples
#' 
#' result <- sesameDataGet('genomeInfo.hg38')
#' @export
sesameDataGet <- function(title, verbose=FALSE) {
    
    if (verbose) {
        .sesameDataGet(title)
    } else {
        suppressMessages(
            log <- capture.output(
                obj <- .sesameDataGet(title)));
        obj
    }
}

#' @import curl
has_internet <- function(){
    !is.null(curl::nslookup("r-project.org", error = FALSE))
}

#' List all SeSAMe data
#'
#' @return all titles from SeSAMe Data
#' @examples
#' sesameDataList()
#' @export
sesameDataList <- function() {
    eh_id_lookup
}

#' Cache all SeSAMe data
#'
#' @param showProgress whether to show progress of download
#' @return TRUE
#' @import ExperimentHub
#' @import AnnotationHub
#' @examples
#' if(FALSE) { sesameDataCacheAll() }
#' @export
sesameDataCacheAll <- function(showProgress = FALSE) {
    setExperimentHubOption(arg="MAX_DOWNLOADS", 100)
    tryCatch(
    {
        ## load meta data
        if (showProgress) {
            eh = query(ExperimentHub(), "sesameData")[eh_id_lookup]
        } else {
            suppressMessages(log <- capture.output(
                eh <- query(ExperimentHub(), "sesameData")[eh_id_lookup]))
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
        message(cond)
        return(FALSE)
    },
    warning = function(cond) {
        message("ExperimentHub Caching causes a warning:")
        message(cond)
        return(FALSE)
    })
    
    TRUE
}

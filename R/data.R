
## the following lookup table will be updated on every new release
## eh = query(ExperimentHub(localHub=FALSE), c("sesameData", "v1.9.1"))
## data.frame(name=eh$title, eh=names(eh))
platform2eh_ids = list(
    "EPIC" = c(
        ## "EPIC.address"="EH3667",           # 1.7.2
        "EPIC.address"="EH5963",              # 1.9.1
        "EPIC.1.LNCaP"="EH3659",              # 1.7.2
        "EPIC.5.normal"="EH3660",             # 1.7.2
        "EPIC.hg19.manifest"="EH3670",        # 1.7.2
        "EPIC.hg38.manifest"="EH3671",        # 1.7.2
        "EPIC.probeInfo"="EH3676",            # 1.7.2
        "leukocyte.betas"="EH3679",           # 1.7.2
        "ref.methylation"="EH3680",           # 1.7.2
        "age.inference"="EH3681",             # 1.7.2
        "ethnicity.inference"="EH3682",       # 1.7.2
        "sex.inference"="EH3683",             # 1.7.2
        "detection.stats"="EH3684",           # 1.7.2
        "genomeInfo.hg19"="EH3665",           # 1.7.2
        "genomeInfo.hg38"="EH3666",           # 1.7.2
        "idatSignature"="EH6018",             # 1.9.1
        "probeIDSignature"="EH6019",          # 1.9.1
        NULL),
    "HM450" = c(
        ## "HM450.address"="EH3668",          # 1.7.2
        "HM450.address"="EH5966",             # 1.9.1
        ## "HM450.1.TCGA.PAAD"="EH3661",      # 1.7.2
        "HM450.1.TCGA.PAAD"="EH5965",         # 1.9.1
        "HM450.10.TCGA.PAAD.normal"="EH3662", # 1.7.2
        "HM450.10.TCGA.BLCA.normal"="EH3663", # 1.7.2
        "HM450.76.TCGA.matched"="EH3664",     # 1.7.2
        "HM450.hg19.manifest"="EH3674",       # 1.7.2
        "HM450.hg38.manifest"="EH3675",       # 1.7.2
        "HM450.probeInfo"="EH3677",           # 1.7.2
        "leukocyte.betas"="EH3679",           # 1.7.2
        "ref.methylation"="EH3680",           # 1.7.2
        "age.inference"="EH3681",             # 1.7.2
        "ethnicity.inference"="EH3682",       # 1.7.2
        "sex.inference"="EH3683",             # 1.7.2
        "detection.stats"="EH3684",           # 1.7.2
        "genomeInfo.hg19"="EH3665",           # 1.7.2
        "genomeInfo.hg38"="EH3666",           # 1.7.2
        "idatSignature"="EH6018",             # 1.9.1
        "probeIDSignature"="EH6019",          # 1.9.1
        NULL),
    "HM27" = c(
        ## "HM27.address"="EH3669",           # 1.7.2
        "HM27.address"="EH5964",              # 1.9.1
        "HM27.hg19.manifest"="EH3672",        # 1.7.2
        "HM27.hg38.manifest"="EH3673",        # 1.7.2
        "HM27.probeInfo"="EH3678",            # 1.7.2
        "genomeInfo.hg19"="EH3665",           # 1.7.2
        "genomeInfo.hg38"="EH3666",           # 1.7.2
        "idatSignature"="EH6018",             # 1.9.1
        "probeIDSignature"="EH6019",          # 1.9.1
        NULL),
    "MM285" = c(
        ## "MM285.address"="EH4678",          # 1.9.1 obsolete
        "MM285.address"="EH5970",             # 1.9.1
        "MM285.1.NOD.FrontalLobe"="EH5968",   # 1.9.1
        "MM285.10.tissue"="EH5969",           # 1.9.1
        "MM285.clock347"="EH5971",            # 1.9.1
        ## "MM285.mm10.manifest"="EH4680",    # 1.9.1 obsolete
        "MM285.mm10.manifest"="EH5972",       # 1.9.1
        "MM285.strain.snp.table"="EH5973",    # 1.9.1
        "MM285.tissueSignature"="EH5974",     # 1.9.1
        "MM285.bloodSignature"="EH6016",      # 1.9.1
        "idatSignature"="EH6018",             # 1.9.1
        "probeIDSignature"="EH6019",          # 1.9.1
        NULL),
    "Mammal40" = c(
        ## "Mammal40.address"="EH5967",       # 1.9.1 obsolete
        "Mammal40.address"="EH4679",          # 1.9.1
        "Mammal40.alignmentScore"="EH6017",   # 1.9.1
        "idatSignature"="EH6018",             # 1.9.1
        "probeIDSignature"="EH6019",          # 1.9.1
        NULL)
)

eh_id_lookup = do.call(c, unname(platform2eh_ids))
cacheEnv <- new.env()
alt_base = "https://zhouserver.research.chop.edu"
alt_base2 = "https://zwdzwd.s3.amazonaws.com"

## fall back data retrieval in case ExperimentHub is down
.sesameDataGet2 <- function(title) {
    eh_id = eh_id_lookup[title]
    if (is.na(eh_id)) {
        eh_id = title
    }
    message("ExperimentHub not responding. Using backup.")
    u1 = sprintf('%s/sesameData/%s.rda', alt_base, title)
    u2 = sprintf('%s/sesameData/%s.rda', alt_base2, title)
    if(valid_url(u1)) {
        assign(eh_id, get(load(url(u1))), envir=cacheEnv)
        TRUE
    } else if (valid_url(u2)) {
        assign(eh_id, get(load(url(u2))), envir=cacheEnv)
        TRUE
    } else {
        warning(sprintf("Resource %s cannot be retrieved.", title))
        FALSE
    }
}

stopAndCache <- function(title) {

    platforms = names(which(vapply(
        platform2eh_ids, function(x) title %in% names(x), logical(1))))
    
    stop(sprintf('
| File needs to be cached to be used in sesame.
| Please run
| > sesameDataCache("%s")
| or cache all platforms by
| > sesameDataCacheAll()
| to retrieve and cache needed sesame data.', platforms[[1]]))
}

.sesameDataGet <- function(title) {
    eh_id = eh_id_lookup[title]
    if (is.na(eh_id)) { # missing from lookup table
        eh_id = title   # use title itself
    } else {            # present in lookup table
        ## try ExperimentHub
        if (!exists(eh_id, envir=cacheEnv, inherits=FALSE)) {
            if (!file.exists(getExperimentHubOption("CACHE"))) {
                stopAndCache(title)
            }
            tryCatch({
                eh = query(ExperimentHub(localHub=TRUE), 'sesameData')
            }, error = function(cond) {
                stopAndCache(title)
            })
            if (!(eh_id %in% names(eh))) {
                stopAndCache(title)
            }
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
#' @import rmarkdown
#' @examples
#'
#' sesameDataCache("HM27")
#' genomeInfo.hg38 <- sesameDataGet('genomeInfo.hg38')
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



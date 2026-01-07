# SeSAMe Supporting Data Package

[![last commit](https://img.shields.io/github/last-commit/zwdzwd/sesameData.svg?style=flat-square)](https://github.com/zwdzwd/sesameData/commits/master)
                  
## Bugs
    
Bug reports are appreciated. Register issues at the SeSAMe [issue tracker](http://github.com/zwdzwd/sesame/issues)
    
    
## About

See [SeSAMe: reducing artifactual detection of DNA methylation by Infinium BeadChips in genomic deletions](https://doi.org/10.1093/nar/gky691) for more details.

## Exported functions (user-facing)

Data access and caching:
- `sesameDataList()` list available data titles
- `sesameDataHas()` check if titles exist
- `sesameDataGet()` load a data object by title
- `sesameDataCache()` / `sesameDataCacheAll()` cache data locally
- `sesameDataGet_resetEnv()` clear the in-memory cache

Platform and genome helpers:
- `inferPlatformFromProbeIDs()` infer platform from probe IDs
- `sesameData_check_platform()` validate or infer platform
- `sesameData_check_genome()` validate or infer genome build
- `sesameData_getGenomeInfo()` load genome metadata

Manifests, probes, and annotation:
- `sesameData_getManifestGRanges()` load manifest GRanges
- `sesameData_getProbesByRegion()` select probes by genomic range
- `sesameData_getProbesByGene()` select probes by gene/promoter
- `sesameData_annoProbes()` annotate probes by features

Transcript utilities:
- `sesameData_getTxnGRanges()` get transcripts or merged genes
- `sesameData_txnToGeneGRanges()` merge transcripts to genes

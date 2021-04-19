context('data')

test_that("test='EPIC.1.LNCaP' gives correct data", {
    dt <- sesameDataGet('EPIC.1.LNCaP')
    expect_equal(length(dt), 2)
    expect_is(dt$seg, "CNSegment")
    expect_equal(class(dt$sset)[1], 'SigSet')
})

test_that("test='EPIC.5.normal' gives correct data", {
    dt <- sesameDataGet('EPIC.5.normal')
    expect_equal(length(dt), 5)
    for (i in seq_len(5)) {
        expect_equal(class(dt[[i]])[1],  "SigSet")
    }
})

test_that("test='HM450.1.TCGA.PAAD' gives correct data", {
    dt <- sesameDataGet('HM450.1.TCGA.PAAD')
    
    expect_equal(length(dt), 2)
    expect_is(dt$betas, "numeric")
    expect_equal(class(dt$sset)[1], "SigSet")
})

test_that("test='HM450.10.TCGA.PAAD.normal' gives correct data", {
    dt <- sesameDataGet('HM450.10.TCGA.PAAD.normal')
    
    expect_equal(dim(dt), c(485577,10))
    expect_is(dt, "matrix")
})

test_that("test='HM450.10.TCGA.BLCA.normal' gives correct data", {
    dt <- sesameDataGet('HM450.10.TCGA.BLCA.normal')
    
    expect_equal(length(dt), 10)
    for (i in seq_len(10)) {
        expect_equal(class(dt[[i]])[1], 'SigSet')
    }
})

test_that("test='HM450.76.TCGA.matched' gives correct data", {
    dt <- sesameDataGet('HM450.76.TCGA.matched')
    
    expect_equal(length(dt), 2)
    expect_is(dt$betas, "matrix")
    expect_is(dt$sampleInfo, "data.frame")
})

test_that("test='genomeInfo.hg19' gives correct data", {
    dt <- sesameDataGet('genomeInfo.hg19')
    
    expect_equal(length(dt), 6)
    expect_is(dt$seqInfo, "Seqinfo")
    expect_is(dt$gapInfo, "GRanges")
    expect_is(dt$cytoBand, 'data.frame')
    expect_is(dt$gene2txn, 'list')
    expect_is(dt$txn2gene, 'list')
    expect_is(dt$txns, 'CompressedGRangesList')
})

test_that("test='genomeInfo.hg38' gives correct data", {
    dt <- sesameDataGet('genomeInfo.hg38')
    
    expect_equal(length(dt), 6)
    expect_is(dt$seqInfo, "Seqinfo")
    expect_is(dt$gapInfo, "GRanges")
    expect_is(dt$cytoBand, 'data.frame')
    expect_is(dt$gene2txn, 'list')
    expect_is(dt$txn2gene, 'list')
    expect_is(dt$txns, 'CompressedGRangesList')
})

test_that("test='EPIC.address' gives correct data", {
    dt <- sesameDataGet('EPIC.address')
    
    expect_equal(length(dt), 2)
    expect_is(dt$ordering, "data.frame")
    expect_is(dt$controls, "data.frame")
})

test_that("test='HM450.address' gives correct data", {
    dt <- sesameDataGet('HM450.address')
    
    expect_equal(length(dt), 2)
    expect_is(dt$ordering, "data.frame")
    expect_is(dt$controls, "data.frame")
})

test_that("test='HM27.address' gives correct data", {
    dt <- sesameDataGet('HM27.address')
    
    expect_equal(length(dt), 2)
    expect_is(dt$ordering, "data.frame")
    expect_is(dt$controls, "data.frame")
})

test_that("test='EPIC.hg19.manifest' gives correct data", {
    dt <- sesameDataGet('EPIC.hg19.manifest')
    expect_is(dt, "GRanges")
})

test_that("test='EPIC.hg38.manifest' gives correct data", {
    dt <- sesameDataGet('EPIC.hg38.manifest')
    expect_is(dt, "GRanges")
})

test_that("test='HM450.hg19.manifest' gives correct data", {
    dt <- sesameDataGet('HM450.hg19.manifest')
    expect_is(dt, "GRanges")
})

test_that("test='HM450.hg38.manifest' gives correct data", {
    dt <- sesameDataGet('HM450.hg38.manifest')
    expect_is(dt, "GRanges")
})

test_that("test='HM27.hg19.manifest' gives correct data", {
    dt <- sesameDataGet('HM27.hg19.manifest')
    expect_is(dt, "GRanges")
})

test_that("test='HM27.hg38.manifest' gives correct data", {
    dt <- sesameDataGet('HM27.hg38.manifest')
    expect_is(dt, "GRanges")
})

test_that("test='EPIC.1.LNCaP' gives correct data", {
    dt <- sesameDataGet('EPIC.1.LNCaP')
    
    expect_equal(length(dt), 2)
    expect_is(dt$seg, "CNSegment")
    expect_equal(class(dt$sset)[1], "SigSet")
})

test_that("test='EPIC.probeInfo' gives correct data", {
    dt <- sesameDataGet('EPIC.probeInfo')
    
    expect_equal(length(dt), 9)
    expect_is(dt$probe2chr.hg19, "character")
    expect_is(dt$mapped.probes.hg19, "GRanges")
    expect_is(dt$mapped.probes.hg38, "GRanges")
    expect_is(dt$typeI.extC, "character")
    expect_is(dt$typeI.extT, "character")
    expect_is(dt$mask, "character")
    expect_is(dt$mask.hg19, "character")
    expect_is(dt$chrY.clean, "character")
    expect_is(dt$chrX.xlinked, "character")
})

test_that("test='HM450.probeInfo' gives correct data", {
    dt <- sesameDataGet('HM450.probeInfo')
    
    expect_equal(length(dt), 10)
    expect_is(dt$probe2chr.hg19, "character")
    expect_is(dt$mapped.probes.hg19, "GRanges")
    expect_is(dt$mapped.probes.hg38, "GRanges")
    expect_is(dt$typeI.extC, "character")
    expect_is(dt$typeI.extT, "character")
    expect_is(dt$mask, "character")
    expect_is(dt$mask.hg19, "character")
    expect_is(dt$mask.tcga, "character")
    expect_is(dt$chrY.clean, "character")
    expect_is(dt$chrX.xlinked, "character")
})

test_that("test='HM27.probeInfo' gives correct data", {
    dt <- sesameDataGet('HM27.probeInfo')
    
    expect_equal(length(dt), 3)
    expect_is(dt$probe2chr.hg19, "character")
    expect_is(dt$mask, "character")
    expect_is(dt$mask.hg19, "character")
})

test_that("test='leukocyte.betas' gives correct data", {
    dt <- sesameDataGet('leukocyte.betas')
    
    expect_equal(length(dt), 3)
    expect_is(dt$EPIC, "data.frame")
    expect_is(dt$HM450, "data.frame")
    expect_is(dt$HM27, "data.frame")
})

test_that("test='ref.methylation' gives correct data", {
    dt <- sesameDataGet('ref.methylation')
    
    expect_equal(length(dt), 17)
    for (i in seq_len(17)){
        expect_is(dt[[i]], "numeric")
    }
})

test_that("test='age.inference' gives correct data", {
    dt <- sesameDataGet('age.inference')
    expect_equal(length(dt), 3)
    expect_is(dt, "list")
    expect_is(dt$Horvath353, "data.frame")
    expect_is(dt$SkinBlood, "data.frame")
    expect_is(dt$PhenoAge, "data.frame")
})

test_that("test='ethnicity.inference' gives correct data", {
    dt <- sesameDataGet('ethnicity.inference')
    
    expect_equal(length(dt), 3)
    expect_is(dt$ccs.probes, "character")
    expect_is(dt$rs.probes, "character")
    expect_is(dt$model, "randomForest")
})

test_that("test='sex.inference' gives correct data", {
    dt <- sesameDataGet('sex.inference')
    expect_is(dt, "randomForest")
})

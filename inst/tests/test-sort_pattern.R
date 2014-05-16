context("sort_pattern test suite")

test_that("sort_pattern works", {
	a <- c("simple-AAA-100", "simple-AAA-200", "simple-AAA-300", "xyz-BBB-100", "xyz-BBB-200", "xyz-BBB-300")
	res <- sort_pattern(a, c("300", "200", "100"))
	expected.res <- c("simple-AAA-300", "xyz-BBB-300", "simple-AAA-200", "xyz-BBB-200", 
	"simple-AAA-100", "xyz-BBB-100")
	expect_equal(res, expected.res)

	res <- sort_pattern(a, c("xyz", "simple"))
	expected.res <- c("xyz-BBB-100", "xyz-BBB-200", "xyz-BBB-300", "simple-AAA-100", 
	"simple-AAA-200", "simple-AAA-300")
	expect_equal(res, expected.res)

	res <- sort_pattern(a, c("xyz", "AAA"))
	expected.res <- c("xyz-BBB-100", "xyz-BBB-200", "xyz-BBB-300", "simple-AAA-100", 
	"simple-AAA-200", "simple-AAA-300")
	expect_equal(res, expected.res)

	res <- sort_pattern(a, c("xyz", "AAA", "300")) # the last term has no effect
	expected.res <- c("xyz-BBB-100", "xyz-BBB-200", "xyz-BBB-300", "simple-AAA-100", 
	"simple-AAA-200", "simple-AAA-300")
	expect_equal(res, expected.res)
	
})



test_that("sort_pattern2 works", {
	a <- c("CCDS_CDS_hg19_uniq", "CCDS_exons_hg19_uniq", "CDSgencodeV16_uniq", 
	"knownGene_CDS_hg19_uniq", "knownGene_exons_hg19_uniq", "knownGene_UTR_hg19_uniq", 
	"LncRNAgencodeV16_uniq", "miRNAgencodeV16pluslength_uniq", "PsuedoGenesgencodeV16_uniq", 
	"refGene_CDS_hg19_uniq", "refGene_exons_hg19_uniq", "refGene_UTR_hg19_uniq", 
	"SnoRNAgencodeV16plusLength_uniq", "SnRNAgencodeV16plusLength_uniq", 
	"start_codengencodeV16_uniq", "stop_codengencodeV16_uniq", "UnprocessedPsudoGenegencodeV16_uniq", 
	"UTRgencodeV16_uniq")
	res <- sort_pattern(a, c("gencode", "exons", "CDS", "UTR"))
	expected.res <- c("CDSgencodeV16_uniq", "LncRNAgencodeV16_uniq", "miRNAgencodeV16pluslength_uniq", 
	"PsuedoGenesgencodeV16_uniq", "SnoRNAgencodeV16plusLength_uniq", 
	"SnRNAgencodeV16plusLength_uniq", "start_codengencodeV16_uniq", 
	"stop_codengencodeV16_uniq", "UnprocessedPsudoGenegencodeV16_uniq", 
	"UTRgencodeV16_uniq", "CCDS_exons_hg19_uniq", "knownGene_exons_hg19_uniq", 
	"refGene_exons_hg19_uniq", "CCDS_CDS_hg19_uniq", "knownGene_CDS_hg19_uniq", 
	"refGene_CDS_hg19_uniq", "knownGene_UTR_hg19_uniq", "refGene_UTR_hg19_uniq"
	)
	expect_equal(res, expected.res)
})

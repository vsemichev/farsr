test_that("can read FARS report CSV files",{
    setwd(file.path(find.package("farsr"),"extdata"))
    smry <- fars_summarize_years(list(2013,2014))
    expect_is(smry,"data.frame")
    expect_equal(nrow(smry),12)
})


test_that("can read FARS report CSV files",{
    df <- fars_read(system.file("extdata","accident_2014.csv.bz2",package="farsr"))
    expect_is(df,"data.frame")
    expect_equal(nrow(df),30056)
})

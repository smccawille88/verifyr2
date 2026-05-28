test_that("XlsxFileComparator detects no differences for identical files", {
  file1 <- system.file("extdata/base_files/file1.xlsx", package = "verifyr2")
  file2 <- system.file("extdata/base_files/file1.xlsx", package = "verifyr2")

  skip_if(!file.exists(file1), "Test xlsx file not available")

  config     <- verifyr2::Config$new()
  comparator <- XlsxFileComparator$new(file1 = file1, file2 = file2)
  result     <- comparator$vrf_summary(config = config)

  expect_true(grepl("No differences", result))
})

test_that("XlsxFileComparator detects differences between files", {
  tmp1 <- tempfile(fileext = ".xlsx")
  tmp2 <- tempfile(fileext = ".xlsx")

  wb1 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb1, "Sheet1")
  openxlsx::writeData(wb1, "Sheet1", data.frame(
    Name  = c("Alice", "Bob"),
    Value = c(1, 2),
    stringsAsFactors = FALSE
  ))
  openxlsx::saveWorkbook(wb1, tmp1, overwrite = TRUE)

  wb2 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb2, "Sheet1")
  openxlsx::writeData(wb2, "Sheet1", data.frame(
    Name  = c("Alice", "Bob"),
    Value = c(1, 99),
    stringsAsFactors = FALSE
  ))
  openxlsx::saveWorkbook(wb2, tmp2, overwrite = TRUE)

  config     <- verifyr2::Config$new()
  comparator <- XlsxFileComparator$new(file1 = tmp1, file2 = tmp2)
  result     <- comparator$vrf_summary(config = config)

  expect_false(grepl("No differences", result))
})

test_that("XlsxFileComparator vrf_details returns text type", {
  tmp1 <- tempfile(fileext = ".xlsx")
  tmp2 <- tempfile(fileext = ".xlsx")

  wb1 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb1, "Sheet1")
  openxlsx::writeData(wb1, "Sheet1", data.frame(
    Name  = c("Alice"),
    Value = c(1),
    stringsAsFactors = FALSE
  ))
  openxlsx::saveWorkbook(wb1, tmp1, overwrite = TRUE)

  wb2 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb2, "Sheet1")
  openxlsx::writeData(wb2, "Sheet1", data.frame(
    Name  = c("Alice"),
    Value = c(99),
    stringsAsFactors = FALSE
  ))
  openxlsx::saveWorkbook(wb2, tmp2, overwrite = TRUE)

  config     <- verifyr2::Config$new()
  comparator <- XlsxFileComparator$new(file1 = tmp1, file2 = tmp2)
  details    <- comparator$vrf_details(config = config)

  expect_true(length(details) > 0)
  expect_equal(details[[1]]$type, "text")
})

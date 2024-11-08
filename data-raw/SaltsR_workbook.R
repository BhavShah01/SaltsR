library(readxl)

usethis::use_data(SaltsR_workbook, overwrite = TRUE)

SaltsR_entryform <-
  read_excel("data-raw/SaltsR_workbook.xlsx", sheet = "EntryForm", skip = 3)

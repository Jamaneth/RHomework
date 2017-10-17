#' save_as_csv
#'
#' @param dataset A dataset, basically
#' @param filename The path of the file
#'
#' @return The full path of the csv where this dataset has been saved
#' @import tidyverse
#' @import assertthat
#' @import readxl
#'
#' @examples
#' save_as_csv(iris, "iris.csv")
#' @export

save_as_csv <- function(dataset, filename) {

  assertthat::assert_that(grepl(".csv", filename))
  assertthat::assert_that(is.data.frame(iris))
  assertthat::assert_that(dir.exists(dirname(filename)))

  dataset %>% write.csv2(filename)
  return(normalizePath(filename))
}


#' My csv reader
#'
#' @param folderpath The path of the folder
#'
#' @return A list of all the csv files contained in the folder
#' @import readxl
#' @import assertthat
#' @import magrittr
#' @export
#'
#' @examples
#' my_csv_reader("~/Documents/")
my_csv_reader <- function(folderpath) {

  assert_that(dir.exists(folderpath))

  csv_list = list()
  for(item in folderpath %>% list.files(pattern = ".*csv$", full.names = TRUE)) {
    csv_list[[length(csv_list)+1]] <- read.csv2(item)
  }
  return(csv_list)
}




#' Read Excel multi
#'
#' @param file Excel file
#'
#' @return
#' @import tidyverse
#' @import magrittr
#' @export
#'
#' @examples


read_excel_multi <- function(file) {

  assert_that(grepl(c(".xls*"), file))

  all_sheets <- readxl::excel_sheets(file)
  result <- lapply(all_sheets, function(sheet){
    readxl::read_excel(file, sheet = sheet)
  }
  )
  return(result)
}



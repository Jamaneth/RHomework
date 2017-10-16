#' save_as_csv
#'
#' @param dataset A dataset, basically
#'
#' @return The full path of the csv where this dataset has been saved
#' @import tidyverse
#' @import readxl
#' @import assertthat
#' @export
#'
#' @examples
save_as_csv <- function(dataset, filename) {

  assert_that(grepl(".csv", filename))
  assert_that(is.data.frame(iris))
  assert_that(dir.exists(dirname(filename)))

  dataset %>% write.csv2(filename)
  return(normalizePath(filename))
}


#' My csv reader
#'
#' @param folderpath The path of the folder
#'
#' @return A list of all the csv files contained in the folder
#' @import tidyr
#' @import readxl
#' @import assertthat
#' @export
#'
#' @examples
my_csv_reader <- function(folderpath) {

  assert_that(dir.exists(folderpath))

  csv_list = list()
  for(item in folderpath %>% list.files(pattern = ".*csv$", full.names = TRUE)) {
    csv_list[[length(csv_list)+1]] <- read.csv2(item)
  }
  return(csv_list)
}



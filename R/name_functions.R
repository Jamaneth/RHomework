#' Draw a name
#'
#' @param the_name Name searched
#' @param the_sex Sex searched
#'
#' @return Returns a graph with the number of times the name was given every year since 1900
#' @import tidyverse
#' @import assertthat
#' @import prenoms
#' @export
#'
#' @examples
#' draw_a_name("Vincent", "M")

draw_a_name <- function(the_name, the_sex) {

  assert_that(is.character(the_name), the_sex %in% c("M", "F"))
  assert_that(the_name %in% (prenoms %>% filter(sex == the_sex))[["name"]])

  return(prenoms %>% filter(name == the_name, sex == the_sex) %>%
           group_by(year) %>%
           summarise(n = sum(n)) %>%
           ggplot(aes(x = year, y = n)) +
           geom_line())
}

#' Draw names
#'
#' @param the_names Names searched (vector form)
#'
#' @return Returns a graph with the number of times each name was given every year since 1900
#' @import tidyverse
#' @import assertthat
#' @import prenoms
#' @export
#'
#' @examples
#' draw_names(c("Vincent", "Diane"))

draw_names <- function(the_names) {

  for(name in the_names) {
    assert_that(name %in% prenoms$name)
  }

  return(prenoms %>% filter (name %in% the_names) %>%
           group_by(year, name) %>%
           summarise(n = sum(n)) %>%
           ggplot(aes(x = year, y = n, color = name)) +
           geom_line())
}

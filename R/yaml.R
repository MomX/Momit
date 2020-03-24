
# utils ----
#' @export
print.yaml <- function(x, ...){
  cat(x)
}

# yaml ----------------------------------------------------
#' yaml wrappers
#'
#' @param x any object
#'
#' @name yaml
#' @examples
#' chivas %>% to_yaml()
#'
NULL

#' @rdname yaml
#' @export
to_yaml <- function(x){
  x %>%
    yaml::as.yaml() %>%
    # add yaml class to benefit print.yaml
    `class<-`(c("yaml", class(.)))
}

#' @rdname yaml
#' @export
from_yaml <- function(x){
  x %>%
    yaml::yaml.load() %>%
    # turn into a tibble
    tibble::as_tibble() %>%
    # no idea why rownmaes in as_tibble doesnt work
    `rownames<-`(NULL)
}


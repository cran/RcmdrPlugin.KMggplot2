#' A Wrapper Theme for ggthemes's theme_wsj
#'
#' @param base_size base font size
#' @param base_family font family name
#' @seealso \code{\link[ggplot2:ggplot]{ggplot}} \code{\link[ggthemes:theme_wsj]{theme_wsj}}
#'
#' @rdname theme-wsj2
#' @keywords color
#' @export
theme_wsj2 <- function(base_size = 16, base_family = "") {

  ggthemes::theme_wsj(base_size = base_size, base_family = base_family, title_family = base_family)

}

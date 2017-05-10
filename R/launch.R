

#' Launch PIVOT Data Basic Analysis Module
#' @examples
#' pivot_analysis()
#' @import shiny
#' @export
pivot_main <- function(r_module = NULL) {
    shiny::runApp(system.file("app", package='PIVOT.analysis'),launch.browser = T)
}

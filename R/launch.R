

#' Launch PIVOT Data Basic Analysis Module
#' @examples
#' pivot_analysis()
#' @import shiny
#' @export
pivot_main <- function(r_module = NULL) {
    Sys.setenv("R_MAX_NUM_DLLS"=180)
    shiny::runApp(system.file("app", package='PIVOT.analysis'),launch.browser = T)
}

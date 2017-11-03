

### Script for Control the package loading and detaching

# Function from Henrik Bengtsson
unloadNamespaces <- function(ns, ...) {
    while (length(ns) > 0) {
        ns0 <- loadedNamespaces()
        for (name in ns) {
            try(unloadNamespace(name), silent=TRUE)
        }
        ns1 <- loadedNamespaces()
        ## No namespace was unloaded?
        if (identical(ns1, ns0)) break
        ns <- intersect(ns, ns1)
    }
    if (length(ns) > 0) stop("Remaining namespaces: ",
                             paste(sQuote(ns), collapse=", "))
} # unloadNamespaces()

# Big issure for this function: quote from R help: detach
# Unloading some namespaces has undesirable side effects: e.g. unloading grid closes all graphics devices, and on some systems tcltk cannot be reloaded once it has been unloaded and may crash R if this is attempted.
#' Function used for clean the current R session
#'
#' This function calls unloadNamespaces by Henrik Bengtsson to unload all attached namespaces except the base ones.
#' @source \url{http://r.789695.n4.nabble.com/Is-it-possible-to-increase-MAX-NUM-DLLS-in-future-R-releases-td4720352.html}
#' @examples
#' detachAllPackages()
#' @export
detachAllPackages <- function() {
    # The base packages required by the launcher (and base R), anything in Depends should be here
    basic.namespaces <- c('shiny', 'R6', 'graphics', 'htmltools', 'tools', 'PIVOT', 'rstudioapi', # Base dependence
                          'utils', 'grDevices', 'Rcpp', 'stats', 'datasets', 'methods', 'digest', # Base dependence
                          'xtable', 'httpuv', 'mime', 'miniUI', 'packrat', 'base', # Base dependence
                          'shinydashboard', 'shinyBS',
                          'PIVOT')

    # First unload all packages (and try unloading associated namespcaces)
    if(FALSE){
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        print(paste("Before package unloading: ", length(package.list)))
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, unload = TRUE, character.only=TRUE)
        print(paste("After package unloading: ", length(search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)])))
    }
    print("Processing...")
    # For those with only namespaces, try unload them using Henrik's brutal force function
    print(paste("#Namespaces before namespace unloading: ", length(loadedNamespaces())))
    namespace.list <- setdiff(loadedNamespaces(),basic.namespaces)
    if (length(namespace.list)>0) {
        tryCatch({
            unloadNamespaces(namespace.list)
        }, error = function(e){
            # Do nothing
        })
    }
    print(paste("#Namespaces after namespace unloading: ", length(loadedNamespaces())))

    # After previous step, still a few extra DLLs loaded
    # Quote Henrik: If we look for .onUnload() in packages that load DLLs, we find that
    # the following does not have an .onUnload() and therefore probably does
    # neither call dyn.unload() when the package is unloaded
    # R.utils::gcDLLs()?
    # Not run
    if(FALSE){
        dlls <- getLoadedDLLs()
        print(paste("Before DLL unloading: ", length(dlls)))
        dlls <- dlls[which(!names(dlls) %in% basic.dlls)]
        if(length(dlls)) lapply(dlls, function(x) {try(dyn.unload(x[["path"]]), silent = T)})
        print(paste("After DLL unloading: ", length(getLoadedDLLs())))
        print("DLL cleanup done!")
    }
}

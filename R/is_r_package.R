#' Is R package
#'
#' Check whether a GitHub repository is for an R package or not.
#' @param pkg Package name.
#' @param owner Name of the owner of the R packages'
#' GitHub repository (optional).
#' @inheritParams echogithub::r_repos_data
#' @returns boolean
#'
#' @importFrom utils installed.packages
#' @importFrom echogithub r_repos_data description_extract
#' @export
#' @examples
#' is_pkg <- is_r_package(pkg="stats")
is_r_package <- function(pkg,
                         owner=NULL,
                         verbose=TRUE){

    pkgs_installed <- utils::installed.packages()
    if(pkg %in% rownames(pkgs_installed)){
        return(TRUE)
    } else {
        pkgs <- echogithub::r_repos_data(add_downloads = FALSE,
                                         verbose = verbose)
        if(pkg %in% unique(pkgs$package)){
            return(TRUE)
        } else if (is.null(owner)){
            stopper("Please provide owner argument to infer R package status.")
        } else {
            d <- echogithub::description_extract(
                ref = paste(owner,pkg,sep="/"),
                fields = "Package",
                verbose = verbose
            )
            if(!is.null(d)) return(TRUE)
        }
    }
    return(FALSE)
}

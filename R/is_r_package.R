#' Is R package
#'
#' Check whether a GitHub repository is for an R package or not.
#' @param pkg Package name.
#' @inheritParams echogithub::description_find
#' @returns boolean
#'
#' @importFrom utils installed.packages
#' @importFrom echogithub r_repos_data description_find
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
        if(pkg %in% pkgs$pkgs$r_repo){
            return(TRUE)
        } else if (is.null(owner)){
            stopper("Please provide owner argument to infer R package status.")
        } else {
            d <- echogithub::description_find(owner = owner,
                                              repo = pkg,
                                              verbose = verbose)
            if(!is.null(d)) return(TRUE)
        }
    }
    return(FALSE)
}

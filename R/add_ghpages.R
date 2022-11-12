#' Add GitHub Pages metadata
#'
#' For each package, list the links to the
#' dedicated GitHub Pages website(s).
#' If there are vignettes, add those as well.
#' If the GitHub Pages website for a package does not exist,
#'  \code{NULL} will be returned for that package.
#' @param meta A metadata \link[data.table]{data.table} produced
#' by \link[echodeps]{package_metadata}.
#' Alternatively, can be a character vector of packages to gather metadata for,
#' passed to \link[echodeps]{package_metadata}.
#' @param add_vignettes Search for any vignettes on the GitHub Pages websites,
#'  and add them to a new column named "ghpages_vignettes".
#' @inheritParams package_metadata
#' @returns \link[data.table]{data.table} of metadata.
#'
#' @export
#' @importFrom stats setNames
#' @importFrom methods is
#' @importFrom data.table data.table := tstrsplit merge.data.table
#' @examples
#' \dontrun{
#' meta <- add_ghpages()
#' }
add_ghpages <- function(meta = "echoverse",
                        add_vignettes = TRUE,
                        verbose = TRUE){
    # echoverseTemplate:::source_all()
    # echoverseTemplate:::args2vars(list_ghpages)

    requireNamespace("echogithub")
    URL <- link_ghpages_index <- Package <- NULL;
    #### Gather package metadata ####
    if(methods::is(meta,"character")){
        meta <- package_metadata(pkgs = meta,
                                 verbose = verbose)
    }
    meta <- data.table::data.table(meta)
    #### List vignettes amd Pages ####
    if(isTRUE(add_vignettes)){
        vdt <- echogithub::github_pages_vignettes(
            owner = lapply(meta$URL,function(x){strsplit(x,"/")[[1]][4]}),
            repo = lapply(meta$URL,function(x){strsplit(x,"/")[[1]][5]}),
            branch = c("gh-pages"),
            as_toc = FALSE,
            verbose = verbose)
    }
    #### If there's no vignettees, try to at least get the GHP index ####
    if(nrow(vdt)==0){
        pages <- lapply(stats::setNames(meta$URL,
                                        meta$Package), function(u){
            echogithub::github_pages(owner = strsplit(u,"/")[[1]][4],
                                     repo = strsplit(u,"/")[[1]][5],
                                     error = FALSE,
                                     verbose = verbose)
        })
        meta[,link_ghpages_index:=pages[Package]]
    }
    #### Add to metadata ####
    meta[,c("owner","repo"):=(data.table::tstrsplit(URL,"/",keep = c(4,5)))]
    meta <- data.table::merge.data.table(meta, vdt,
                                         by = c("owner","repo"))
    return(meta)
}

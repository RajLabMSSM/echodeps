#' Gather package metadata
#'
#' Gather metadata from the DESCRIPTION file of each R package and construct a
#' \link[data.table]{data.table}.
#'  By default, returns metadata for all
#'  \href{https://github.com/topics/echoverse}{\code{echoverse}} packages.
#'
#' @param pkgs Packages to gather metadata for.
#' @param fields DESCRIPTION file fields.
#' If a field is missing for a given package, will fill with \code{NULL}.
#' @param verbose Print messages.
#' @returns \link[data.table]{data.table} of metadata.
#'
#' @export
#' @importFrom echogithub description_extract
#' @importFrom data.table rbindlist setnames setkey
#' @examples
#' meta <- echodeps::package_metadata()
package_metadata <- function(pkgs = "echoverse",
                             fields = c("Package",
                                         "Title",
                                         "Description",
                                         "URL",
                                         "Version",
                                         "Depends",
                                         "Imports",
                                         "Suggests",
                                         "Remotes",
                                         "SystemRequirements"),
                              verbose = TRUE){
    # devoptera::args2vars(package_metadata)

  requireNamespace("echogithub")

  #### Handle "echoverse" ####
  pkgs <- handle_echoverse(pkgs = pkgs,
                           verbose = verbose)
  messager("Collecting metadata for",length(pkgs),"packages",v=verbose)
  #### Iterate ####
  meta <- lapply(pkgs, function(pkg){
    tryCatch({
      echogithub::description_extract(refs = pkg,
                                      fields = NULL,
                                      as_datatable = TRUE,
                                      verbose = FALSE)
    }, error = function(e){warning(e); NULL})
  }) |> data.table::rbindlist(fill = TRUE)

  if(nrow(meta)==0){
      stop("No metadata retrieved.")
  } else {
      meta <- meta |>
          data.table::setnames("Package","package") |>
          data.table::setkey("package")
  }
  meta <- echogithub::add_owner_repo(dt = meta)
  #### Return ####
  return(meta)
}

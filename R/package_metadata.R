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
    # echoverseTemplate:::source_all()
    # echoverseTemplate:::args2vars(package_metadata)

  requireNamespace("echogithub")

  #### echoverse modules ####
  echoverse <- echoverse_modules()
  #### Handle "echoverse" ####
  if("echoverse" %in% tolower(pkgs)){
    pkgs <- c(pkgs[pkgs!="echoverse"],echoverse)
    messager("Adding all echoverse modules to metadata search.",v=verbose)
    pkgs <- echoverse
  }
  messager("Collecting metadata for",length(pkgs),"packages",v=verbose)
  #### Split func ####
  parse_deps <- function(d,
                         field,
                         split=","){
    gsub("\n","",strsplit(d[[field]],split=split)[[1]])
  }
  #### Iterate ####
  meta <- lapply(pkgs, function(pkg){
    tryCatch({
      messager("-",pkg, v=verbose)
      d <- echogithub::description_find(repo = pkg,
                                        verbose = verbose)
      if(is.null(d)) return(NULL)
      # d <- utils::packageDescription(pkg)
      # if(!methods::is(d,"data.frame") && is.na(d)) {
      #   return(NULL)
      # }
      data.table::data.table(
        t(
          lapply(fields,function(x){
            if(is.null(d[[x]]))  return(NA)
            parse_deps(
              d = d,
              field = x,
              split = if(x %in% c("Title","Description")) "______" else ",")
          }) |> `names<-`(fields)
        )
      )
    }, error = function(e){warning(e); NULL})
  }) |> data.table::rbindlist()
  meta$Package <- unlist(meta$Package)
  #### Set index ####
  ## ----------------------##
  #### data.table issues ####
  ## https://github.com/Rdatatable/data.table/issues/5330
  ## Something weird is happening with data.table indexing lately.
  ## When I use keys in R console, they work fine.
  ## But when i set them within a function, it's as if they were never set
  ## at all. Even when i set them again within the same function.
  ## Using the slower (but more reliable) data.frame method.
  ##
  ### NONE of these methods work!
  # data.table::setkey(meta,Package)
  # meta <- data.table::as.data.table(meta, key="Package")
  # data.table::setDT(meta, key = "Package")
  # meta <- data.table::setDT(meta, key = "Package")
  ##
  ### Only this works
  meta <- data.frame(meta)
  rownames(meta) <- meta$Package
  #### Return ####
  return(meta)
}

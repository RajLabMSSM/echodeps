subset_deps <- function(pkg_name,
                        deps,
                        report){
    if(tolower(pkg_name)=="echolocator" && is.null(deps)){
        deps <- "echoverse"
    } else if(is.null(deps)){
        deps_all <- report$DependencyReporter$nodes$node
        deps <- deps_all[deps_all!=pkg_name]
    } else {
        deps_all <- report$DependencyReporter$nodes$node
        deps <- deps_all[(deps_all!=pkg_name) & (deps_all %in% deps)]
    }
    return(deps)
}

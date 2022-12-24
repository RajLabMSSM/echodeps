subset_deps <- function(pkg,
                        include=NULL,
                        exclude=NULL,
                        report){

    deps <- report$DependencyReporter$nodes$node
    #### Include ####
    if(is.null(include)){
        deps <- deps[deps!=pkg]
    } else {
        deps <- deps[(deps!=pkg) & (deps %in% include)]
    }
    #### Exclude ####
    if(!is.null(exclude)){
        deps <- deps[!deps %in% exclude]
    }
    return(deps)
}

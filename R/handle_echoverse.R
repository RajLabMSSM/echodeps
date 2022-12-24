handle_echoverse <- function(pkgs,
                             verbose=TRUE){
    if("echoverse" %in% tolower(pkgs)){
        messager("Adding all echoverse modules to metadata search.",v=verbose)
        echoverse <- echoverse_modules()
        pkgs <- c(pkgs[pkgs!="echoverse"],echoverse)
    }
    return(pkgs)
}

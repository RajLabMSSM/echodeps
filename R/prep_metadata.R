prep_metadata <- function(pkg,
                          meta,
                          owner=NULL,
                          repo=NULL,
                          use_basename,
                          sep){
    repo <- NULL;

    ##### Add repo metadata ####
    if(isTRUE(use_basename)) {
        pkg_name2 <- if(!is.null(pkg)) basename(pkg) else NULL
        key_var <- c("package","repo")
        key_var <- key_var[key_var %in% names(meta)][1]
        data.table::setkeyv(meta,key_var)
    } else {
        if(!is.null(owner) &&
           !is.null(repo)){
            pkg_name2 <- paste(owner,repo,sep=sep)
        } else {
            pkg_name2 <- meta[repo==pkg,]$owner_repo
        }
        key_var <- c("owner_repo","package")
        key_var <- key_var[key_var %in% names(meta)][1]
        data.table::setkeyv(meta,key_var)
    }
    return(list(pkg_name2=pkg_name2,
                meta=meta))
}

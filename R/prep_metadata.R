prep_metadata <- function(meta=NULL,
                          g=NULL,
                          verbose=TRUE){
    package <- NULL;

    include <- unique(igraph::V(g)$ref)
    include <- include[!duplicated(basename(include))]
    if(is.null(meta)){
        if(is.null(g)) stop("g is required when meta is NULL.")
        meta <- echogithub::r_repos_data(include = include,
                                         add_downloads = TRUE,
                                         add_descriptions = TRUE,
                                         add_github = TRUE,
                                         add_hex = TRUE,
                                         cast = TRUE,
                                         verbose = verbose)
    }
    meta <- echogithub::add_owner_repo(dt = meta)
    ##### Add repo metadata ####
    key_var <- c("ref","owner_repo","package","repo")
    key_var <- key_var[key_var %in% names(meta)][1]
    # sum(is.na(meta[[key_var[1]]]))
    #### Remove duplicates ####
    meta <- meta[!duplicated(package),]
    #### Set key ####
    data.table::setkeyv(meta,key_var)
    return(meta)
}

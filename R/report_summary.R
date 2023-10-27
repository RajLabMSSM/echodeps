report_summary <- function(meta,
                           verbose=TRUE){

    # meta <- data.table::as.data.table(g)
    cols <- c(
        "stargazers_count","watchers_count","forks_count","subscribers_count",
        "clones_uniques","views_uniques",
        "open_issues_count","network_count","size",
        echogithub::r_repos_opts(),
        "total_downloads")
    cols <- cols[cols %in% names(meta)]
    if(length(cols)==0) return(NULL)
    messager("Creating report summary.",v=verbose)
    meta[,(cols):= lapply(.SD, as.integer), .SDcols = cols]
    sums <- colSums(meta[,cols, with=FALSE], na.rm = TRUE)
    report <- rbind(
        data.table::data.table(
            metric="package_count",
            value=length(unique(meta$Package))
        ),
        data.table::data.table(
            metric="repo_count",
            value=length(unique(meta$repo))
        ),
        data.table::data.table(
            metric="owner_count",
            value=length(unique(meta$owner))
        ),
        data.table::data.table(
            metric=names(sums),
            value=unname(sums)
        )
    )
    return(report)
}

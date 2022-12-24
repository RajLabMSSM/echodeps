report_summary <- function(metadata,
                           verbose=TRUE){

    messager("Creating report summary.",v=verbose)
    cols <- c(
        "stargazers_count","watchers_count","forks_count","subscribers_count",
        "clones_uniques","views_uniques",
        "open_issues_count","network_count","size",
        echogithub::r_repos_opts(),
        "total_downloads")
    cols <- cols[cols %in% names(metadata)]
    metadata[,(cols):= lapply(.SD, as.integer), .SDcols = cols]
    report <- cbind(
        package_count=length(unique(metadata$repo)),
        colSums(metadata[,cols, with=FALSE])
    )
    return(report)
}

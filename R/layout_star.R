#' Star layout
#'
#' Default layout function that arranges nodes in a star pattern with
#' one node (\code{pkg_name}) at its center.
#'
#' @param pkg_name Name of the node to place at the center of the plot
#'  (e.g. "echolocatoR").
#' @inheritParams visNetwork::visIgraphLayout
#' @param ... Additional arguments passed to
#' \link[visNetwork]{visIgraphLayout}.
#' @export
layout_star <- function(graph,
                        pkg_name,
                        ...){
    visNetwork::visIgraphLayout(
        graph = graph,
        layout = "layout_as_star",
        center=pkg_name,
        randomSeed = 11,
        ...)
}

#' Star layout
#'
#' Default layout function that arranges nodes in a star pattern with
#' one node (\code{pkg}) at its center.
#'
#' @param pkg Name of the node to place at the center of the plot
#'  (e.g. "echolocatoR").
#' @inheritParams visNetwork::visIgraphLayout
#' @param ... Additional arguments passed to
#' \link[visNetwork]{visIgraphLayout}.
#' @export
layout_star <- function(graph,
                        pkg,
                        layout = "layout_as_star",
                        ...){
    visNetwork::visIgraphLayout(
        graph = graph,
        layout = layout,
        center=pkg,
        randomSeed = 11,
        ...)
}

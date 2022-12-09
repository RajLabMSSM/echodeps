#' Construct colors
#'
#' Construct a color mapping for \link[echodeps]{dep_graph_plot}.
#' @param node_background node_background
#' @param node_border node_border
#' @param node_highlight node_highlight
#' @param node_hover_background node_hover_background
#' @param node_hover_border node_hover_border
#' @param node_font node_font
#' @param node_font_stroke node_font_stroke
#' @param node_shadow node_shadow
#' @param edge_color edge_color
#' @param edge_highlight edge_highlight
#' @param edge_shadow edge_shadow
#' @param tooltip_font tooltip_font
#' @param tooltip_background interaction_background
#' @param tooltip_box_shadow interaction_box_shadow
#' @param background background
#' @param save_background save_background
#' @returns Named list.
#'
#' @export
#' @examples
#' colors <- construct_colors()
construct_colors <- function(
    ## Nodes
    node_background = "#25355c",
    node_border = "#41c6c8",
    node_highlight = "#56ffff",
    node_hover_background = "blue",
    node_hover_border = node_border,
    node_font = "white",
    node_font_stroke = "rgba(103,115,141,.5)",
    node_shadow = "#537bcb",
    ## Edges
    edge_color = node_highlight,
    edge_highlight = "#686ea6",
    edge_shadow = edge_highlight,
    ## Tooltips
    tooltip_font = "#fff",
    tooltip_background = "rgba(0,0,0,.5)",
    tooltip_box_shadow = "rgba(247,247,247,0.5)",
    ## Plot background
    background = node_background,
    save_background = node_background){
    list(
        ## Nodes
        node_background = node_background,
        node_border = node_border,
        node_highlight = node_highlight,
        node_hover_background=node_hover_background,
        node_hover_border=node_hover_border,
        node_font=node_font,
        node_font_stroke = node_font_stroke,
        node_shadow = node_shadow,
        ## Edges
        edge_color = edge_color,
        edge_highlight = edge_highlight,
        edge_shadow = edge_shadow,
        ## Interactions
        tooltip_font = tooltip_font,
        tooltip_background = tooltip_background,
        tooltip_box_shadow = tooltip_box_shadow,
        ## Plot background
        background = background,
        save_background = save_background)
}

# Construct colors

Construct a color mapping for
[dep_graph_plot](https://rajlabmssm.github.io/echodeps/reference/dep_graph_plot.md).

## Usage

``` r
construct_colors(
  node_background = "#25355c",
  node_border = "#41c6c8",
  node_highlight = "#56ffff",
  node_hover_background = "blue",
  node_hover_border = node_border,
  node_font = "white",
  node_font_stroke = "rgba(103,115,141,.5)",
  node_shadow = "#537bcb",
  edge_color = node_highlight,
  edge_highlight = "#686ea6",
  edge_shadow = edge_highlight,
  tooltip_font = "#fff",
  tooltip_background = "rgba(0,0,0,.5)",
  tooltip_box_shadow = "rgba(247,247,247,0.5)",
  background = "transparent",
  save_background = background
)
```

## Arguments

- node_background:

  node_background

- node_border:

  node_border

- node_highlight:

  node_highlight

- node_hover_background:

  node_hover_background

- node_hover_border:

  node_hover_border

- node_font:

  node_font

- node_font_stroke:

  node_font_stroke

- node_shadow:

  node_shadow

- edge_color:

  edge_color

- edge_highlight:

  edge_highlight

- edge_shadow:

  edge_shadow

- tooltip_font:

  tooltip_font

- tooltip_background:

  interaction_background

- tooltip_box_shadow:

  interaction_box_shadow

- background:

  background

- save_background:

  save_background

## Value

Named list.

## Examples

``` r
if (FALSE) { # \dontrun{
colors <- construct_colors()
} # }
```

# echodeps 0.99.4

## New features

* Change `deps=` arg to exclude.
* New features for finding *reverse* dependencies:
    - `dep_graph(reverse=TRUE)`
    - `revdep_*` functions
* Change all args: `pkg_name` --> `pkg` (more terse)
* New function:
    - `merge_graphs`

## Bug fixes

* Elevate `echogithub` to an *Import*.

# echodeps 0.99.3

## New features

* Use `rworkflows` GHA.
    - Add as dep.
* Break `dep_graph_create` into subfunctions:
    - `dep_graph_create_pkgnet`
    - `dep_graph_create_github`
* Select from the two `dep_graph_create` options with new `method=` arg.
* New function `construct_colors`: helps construct colors for `dep_graph_plot`.
* New function `dep_graph_create` to create just the graph without the plot.
* New function `subset_graph` to easily subset graphs by node names.

## Bug fixes

* Fixed `package_metadata` example by using `echogithub::description_find` 
to more robustly get *DESCRIPTION* file.

# echodeps 0.99.2

## New features

* Added `rworkflows`
* New function: `add_ghpages`
* New Suggest: `echogithub`

# echodeps 0.99.1

## New features

* Removed unnecessary functions:
    - `is_32bit`
    - `source_all`

# echodeps 0.99.0

## New features

* Added a `NEWS.md` file to track changes to the package.

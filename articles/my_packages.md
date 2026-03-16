# My packages

``` r

library(echodeps)
```

This vignette shows how to use `echodeps` to visualise reverse
dependencies of one of your own packages. The examples below use
`rworkflows` as the seed package and then filter the resulting graph to
packages published by specific GitHub organisations.

## Create graph

``` r

res <- dep_graph(pkg = "rworkflows",
                 method_seed = "github",
                 reverse = TRUE)
```

## Subset graph

Once you have the full reverse-dependency graph you can subset it to
only the packages you maintain. Here we keep nodes whose names contain
known GitHub organisation prefixes.

``` r

mypkgs <- grep("neurogenomics/|bschilder/|RajLabMSSM/|NathanSkene/",
               names(igraph::V(res$graph)),
               value = TRUE)
g <- res$graph |>
    tidygraph::filter(name %in% mypkgs) |>
    dplyr::mutate(size = 60)
```

## Plot network

``` r

plt <- echodeps::dep_graph_plot(g = g,
                                pkg = "rworkflows",
                                use_basename = TRUE)
```

## Session Info

``` r

utils::sessionInfo()
```

    ## R Under development (unstable) (2026-03-12 r89607)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.4 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] echodeps_1.0.0   BiocStyle_2.39.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.6        xfun_0.56           bslib_0.10.0       
    ##  [4] ggplot2_4.0.2       htmlwidgets_1.6.4   visNetwork_2.1.4   
    ##  [7] gh_1.5.0            vctrs_0.7.1         tools_4.6.0        
    ## [10] bitops_1.0-9        generics_0.1.4      yulab.utils_0.2.4  
    ## [13] parallel_4.6.0      tibble_3.3.1        pkgconfig_2.0.3    
    ## [16] data.table_1.18.2.1 RColorBrewer_1.1-3  S7_0.2.1           
    ## [19] desc_1.4.3          pals_1.10           lifecycle_1.0.5    
    ## [22] compiler_4.6.0      farver_2.1.2        stringr_1.6.0      
    ## [25] dlstats_0.1.7       textshaping_1.0.5   mapproj_1.2.12     
    ## [28] htmltools_0.5.9     maps_3.4.3          sass_0.4.10        
    ## [31] RCurl_1.98-1.17     yaml_2.3.12         pkgdown_2.2.0      
    ## [34] pillar_1.11.1       jquerylib_0.1.4     tidyr_1.3.2        
    ## [37] cachem_1.1.0        tidyselect_1.2.1    digest_0.6.39      
    ## [40] stringi_1.8.7       dplyr_1.2.0         purrr_1.2.1        
    ## [43] bookdown_0.46       rprojroot_2.1.1     fastmap_1.2.0      
    ## [46] grid_4.6.0          here_1.0.2          colorspace_2.1-2   
    ## [49] cli_3.6.5           magrittr_2.0.4      dichromat_2.0-0.1  
    ## [52] tidygraph_1.3.1     scales_1.4.0        rappdirs_0.3.4     
    ## [55] rmarkdown_2.30      rvcheck_0.2.1       igraph_2.2.2       
    ## [58] otel_0.2.0          ragg_1.5.1          evaluate_1.0.5     
    ## [61] echogithub_1.0.0    knitr_1.51          rworkflows_1.0.8   
    ## [64] rlang_1.1.7         glue_1.8.0          BiocManager_1.30.27
    ## [67] renv_1.1.8          jsonlite_2.0.0      R6_2.6.1           
    ## [70] badger_0.2.5        systemfonts_1.3.2   fs_1.6.7

\

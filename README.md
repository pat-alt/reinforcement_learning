
## Multi-Armed Bandit problems

[Problem Set 2](ps2_answers.html) replicates an empirical evaluation of
Thompson Sampling (Chapelle and Li 2011). Code for this problem has been
implemented in R and performance-enhanced through `Rcpp` (`C++`).

To run the simulation from the command line, simply execute the command
below. This will clone the git repo to your device and run the
simulation with parameters specified in `run_simulation`.

``` bash
git clone https://github.com/pat-alt/reinforcement_learning.git
cd reinforcement_learning
Rscript -e 'source("requirements.R"); run_simulation(n_sim=5,horizon=1e6,update_every = 100)'  
open 'www/user_sim.png'
```

With the given set of parameters the computations should only take a few
minutes and the resulting chart should looks something like this:

![](www/user_sim.png)

The results from the full simulation are shown below. All details and
documentation can be found in the [HTML document](ps2_answers.html).

![](www/ps1_sim.png)

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-chapelle2011empirical" class="csl-entry">

Chapelle, Olivier, and Lihong Li. 2011. “An Empirical Evaluation of
Thompson Sampling.” *Advances in Neural Information Processing Systems*
24: 2249–57.

</div>

</div>

## Session Info

``` r
utils::sessionInfo()
```

    ## R version 4.0.3 (2020-10-10)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS High Sierra 10.13.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_4.0.3    magrittr_2.0.1    tools_4.0.3       htmltools_0.5.1.1
    ##  [5] yaml_2.2.1        stringi_1.5.3     rmarkdown_2.6     knitr_1.30       
    ##  [9] stringr_1.4.0     xfun_0.20         digest_0.6.27     rlang_0.4.10     
    ## [13] evaluate_0.14

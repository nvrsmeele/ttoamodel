# Taboo trade-off aversion in choice behaviors: a discrete choice model and application to health-related decisions

[[Paper]](https://www.sciencedirect.com/science/article/pii/S0277953625009372)
[[Data]](#) *soon available*

This repository contains the codebase for the Taboo Trade-off Aversion (TTOA) model to replicate the results from the paper *"Taboo trade-off aversion in choice behaviors: a discrete choice model and application to health-related decisions"*.

## Setup

We used R 4.3.2 and [Apollo](http://www.apollochoicemodelling.com/index.html) 0.3.4 to estimate all our models and compute post-estimation measures (Likelihood Ratio Statistic and Willingness-To-Pay estimates). The codebase is expected to be compatible with R 4.0.0 or higher.

You can download and install Apollo 0.3.4 with the following command:

``` r
install.packages("remotes")
remotes::install_version("apollo", version = "0.3.4")
```

## Usage

The code for modelling taboo trade-off aversion in choice behaviors is provided for two experiments:

* Experiment 1 (health insurance) &rarr; `src/experiment1`
* Experiment 2 (organ transplantation) &rarr; `src/experiment2`

Each experiment folder contains:

* `mnl_benchmark.R` - Benchmark Multinomial Logit model
* `mnl_ttoa.R` - Multinomial Logit model accounting for TTOA behavior
* `lc_rum.R` - Benchmark Latent Class model
* `lc_ttoa.R` - Latent Class model accounting for TTOA behavior
* `post_estimation.R` - Likelihood Ratio test and WTP estimation

## Replication steps

In case you want to replicate the results from the paper, you should follow the next steps:

1. Download the two stated preference datasets [here](#) *soon available*.
2. To replicate the *health insurance* results, run:
* `mnl_benchmark.R`, `mnl_ttoa.R`, and `lc_ttoa.R` inside `src/experiment1`
3. To replicate the *organ transplantation* results, run:
* `mnl_benchmark.R`, `mnl_ttoa.R`, and `lc_ttoa.R` inside `src/experiment2`
4. In each experiment folder, run `post_estimation.R` to compute:
* Likelihood Ratio test between benchmark and TTOA models
* Willingness-To-Pay (WTP) estimates with standard errors (Delta method)

## Experimental design

Each experiment contains a `design.ngs` file (Ngene syntax) that specifies the experimental design of the discrete choice experiment.

These can be regenerated with Ngene (see [Ngene software](https://www.choice-metrics.com/))

## Citation

If you use this code, please cite the manuscript:

```
@article{smeele2025ttoa,
  title={Taboo trade-off aversion in choice behaviors: a discrete choice model and application to health-related decisions},
  author={Smeele, N.V.R. and van Cranenburgh, S. and Donkers, B. and Schermer, M.H.N. and de Bekker-Grob, E.W.},
  journal={Social Science \& Medicine},
  year={2025},
  note={In press}
}
```

## License

This repository is released under the MIT License. See [LICENSE](https://github.com/nvrsmeele/ttoamodel/blob/main/LICENSE) for further details.

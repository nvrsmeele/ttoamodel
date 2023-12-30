# Taboo trade-off aversion in choice behaviours: a discrete choice model and application to health-related decisions

[[Paper]](#)
[[Data]](#)

This repository contains the codebase for the Taboo Trade-off Aversion (TTOA) model to replicate the results from the paper *"Taboo trade-off aversion in choice behaviours: a discrete choice model and application to health-related decisions"*.

## Setup

We used R 4.3.2 and [Apollo](http://www.apollochoicemodelling.com/index.html) 4.3.1 to estimate all our models and compute post-estimation measures, such as the Likelihood Ratio Statistic and Willingness-To-Pay estimates. The codebase is expected to be compatible with R 4.0.0 or higher.

You can download and install Apollo 4.3.1 with the following command:

``` r
install.packages("apollo", version="4.3.1")
```

## Usage

The codebase for modelling taboo trade-off aversion in choice behaviours using Multinomial Logit (MNL) models and Latent Class (LC) models with classes parameterised by linear-additive MNL kernels can be found in the `\src\experiment1` and `\src\experiment2` folders. The TTOA-MNL  and TTOA-LC models can be found under `mnl_ttoa.R` and `lc_ttoa.R` in each respective folder.

In case you want to replicate the results from our paper, you should follow the next steps:

1. Download the two stated preference datasets [here](#).
2. Run the `\src\utils\preprocess_exp1.R` and `\src\utils\preprocess_exp2.R` scripts to preprocess the *health insurance* and *organ transplantation* datasets, respectively.
3. To replicate the results of the *health insurance* experiment, run the `mnl_benchmark.R`, `mnl_ttoa.R`, and `lc_ttoa.R` scripts to estimate all models in the `\src\experiment1` folder.
4. To replicate the results of the *organ transplantation* experiment, run the `mnl_benchmark.R`, `mnl_ttoa.R`, and `lc_ttoa.R` scripts to estimate all models in the `\src\experiment2` folder.
5. Run the `post_estimation.R` scripts to compute the Likelihood Ratio Statistic between `mnl_benchmark.R` and `mnl_ttoa.R` and Willingness-To-Pay estimates with standard errors using the Delta method for each respective experiment.

## Citation

For citations, please use:

```
TBD
```

## License

The TTOA model's codebase is released under the MIT License. See [LICENSE](https://github.com/nvrsmeele/ttoamodel/blob/main/LICENSE) for further details.

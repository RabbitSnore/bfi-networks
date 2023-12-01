# Network Analysis of Open Data Sets of Five-Factor Model Personality Inventories

This repository contains the code for statistical analyses of open data from
personality inventories based on the Five-Factor Model. This repository is a
companion to this one: https://github.com/RabbitSnore/ipip-networks/

We reanalyze data from the following four articles:

Gallardo-Pujol, D., Rouco, V., Cortijos-Bernabeu, A., Oceja, L., Soto, C. J., &
John, O. P. (2022). Factor Structure, Gender Invariance, Measurement Properties,
and Short Forms of the Spanish Adaptation of the Big Five Inventory-2.
Psychological Test Adaptation and Development, 3(1), 44–69.
https://doi.org/10.1027/2698-1866/a000020

Soto, C. J., & John, O. P. (2017). The next Big Five Inventory (BFI-2):
Developing and assessing a hierarchical model with 15 facets to enhance
bandwidth, fidelity, and predictive power. Journal of Personality and Social
Psychology, 113(1), 117–143. https://doi.org/10.1037/pspp0000096

Zhang, B., Li, Y. M., Li, J., Luo, J., Ye, Y., Yin, L., Chen, Z., Soto, C. J., &
John, O. P. (2022). The Big Five Inventory–2 in China: A Comprehensive
Psychometric Evaluation in Four Diverse Samples. Assessment, 29(6), 1262–1284.
https://doi.org/10.1177/10731911211008245

Zhang, X., Wang, M.-C., He, L., Jie, L., & Deng, J. (2019). The development and
psychometric evaluation of the Chinese Big Five Personality Inventory-15. PLOS
ONE, 14(8), e0221621. https://doi.org/10.1371/journal.pone.0221621

Additionally, we reanalyze data from the Open-Source Psychometrics Project.
These data are available here: https://openpsychometrics.org/_rawdata/

With the exception of B. Zhang et al (2022), the code is designed to
automatically download all necessary data.

## Repository structure

- `data` will be created automatically by the scripts as a place to store the raw and cleaned data
- `R` contains the scripts for data cleaning and analyses
- `output` contains the stored results of the analyses
- `figures` contains data visualizations

## Reproduce Analyses

The easiest method of reproducing the analyses is to run
`source("R/bfi-networks_build-projects.R")`, which will run all the code to
build the project content, in the necessary order. By default, the code is set
up to load the stored output. This approach is useful if you want to recreate
the analytic environment as we created it. However, if you want to reproduce all
the analyses from scratch, delete the `output` folder before running the build
script. Note that the analyses are computationally intensive. Even with parallel
processing, you should expect computation times in the order of hours, on a
typical personal computer.

If you are reproducing the analyses from scratch, be sure to adjust the parallel
processing set up to suit your specific computing environment!

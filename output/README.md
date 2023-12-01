# Output Data Files

The files in this directory are produced by the R code in this repository. If
reproducing the analyses from scratch, it is recommended that you delete your
copies of these output files, since much of the code is set up to skip processes
if the output files already exist. It is set up this way to avoid running very
long processes when they are not necessary.

You will notice that the cross country data and the model comparison data have
both RDS and CSV files. The RDS files contain a list column with the omega
matrix from the network models (i.e., the network structure), and the CSV files
omit this column. If you just want to glance through things, the CSV files are
more convenient. But the RDS files are more complete.

## Manifest

```
"bfi_cross-country-test-data_open-psychometrics.csv" Fit statistics for cross-country network models
"bfi_cross-country-test-data_open-psychometrics.rds" Fit statistics for cross-country network models
"bfi_model-comparison-data_cbf.csv"                  Fit statistics for factor and network models in test data, X. Zhang et al (2019)
"bfi_model-comparison-data_cbf.rds"                  Fit statistics for factor and network models in test data, X. Zhang et al (2019)
"bfi_model-comparison-data_chinese-adaptation.csv"   Fit statistics for factor and network models in test data, B. Zhang et al (2022)
"bfi_model-comparison-data_chinese-adaptation.rds"   Fit statistics for factor and network models in test data, B. Zhang et al (2022)
"bfi_model-comparison-data_open-psychometrics.csv"   Fit statistics for factor and network models in test data, Open Psychometrics
"bfi_model-comparison-data_open-psychometrics.rds"   Fit statistics for factor and network models in test data, Open Psychometrics
"bfi_model-comparison-data_spanish-adaptation.csv"   Fit statistics for factor and network models in test data, Gallardo-Pujol et al (2022)
"bfi_model-comparison-data_spanish-adaptation.rds"   Fit statistics for factor and network models in test data, Gallardo-Pujol et al (2022) 
"bfi_model-comparison-data_us-validation.csv"        Fit statistics for factor and network models in test data, Soto & John (2017) 
"bfi_model-comparison-data_us-validation.rds"        Fit statistics for factor and network models in test data, Soto & John (2017)    
"bfi_op_confirmatory-networks.rds"                   Network models fit to full country data, Open Psychometrics             
"bfi_op_ffm-esem-model-fit.csv"                      Fit statistics for ESEMs, Open Psychometrics         
"bfi_op_ffm-esem-model-fit.rds"                      Fit statistics for ESEMs, Open Psychometrics
"bfi_op_matrix-bic-test.csv"                         Matrix of cross-country models' BICs, Open Psychometrics
"bfi_op_matrix-cfi-test-rounded.csv"                 Matrix of cross-country models' CFIs, rounded, Open Psychometrics
"bfi_op_matrix-cfi-test.csv"                         Matrix of cross-country models' CFIs, Open Psychometrics          
"bfi_op_matrix-rmsea-test-rounded.csv"               Matrix of cross-country models' RMSEAs, rounded, Open Psychometrics
"bfi_op_matrix-rmsea-test.csv"                       Matrix of cross-country models' RMSEAs, Open Psychometrics
"bfi_op_matrix-tli-test-rounded.csv"                 Matrix of cross-country models' TLIs, rounded, Open Psychometrics
"bfi_op_matrix-tli-test.csv"                         Matrix of cross-country models' TLIs, rounded, Open Psychometrics
"bfi_op_origin-model-performance.csv"                Model comparison statistics of country's own network vs other countries', Open Psychometrics
"bfi_op_uc-esem-model-fit.csv"                       Fit statistics for ESEMs (parallel analysis), Open Psychometrics
"bfi_op_uc-esem-model-fit.rds"                       Fit statistics for ESEMs (parallel analysis), Open Psychometrics
"ffm-esem"                                           Folder containing ESEM output and syntax, Open Psychometrics
```
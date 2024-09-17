# Generating Synthetic Datasets with Synthpop

## Introduction

In this project, we utilize the Synthpop package in R to generate synthetic datasets from original microdata containing confidential information. Synthetic data provides a secure alternative to real-world data, ensuring privacy while maintaining statistical properties. Our aim is to evaluate the effectiveness of synthetic data in various scenarios, including:

- Checking the distribution of a continuous variable across different categories of a categorical variable.
- Comparing logistic and linear regression models before and after synthesizing the dataset.
- Analyzing the timing and performance of generating datasets of varying sizes.
- Verifying if the statistical distribution of synthetic datasets aligns with the original data.

By exploring these aspects, we can determine the robustness of synthetic data generation methods and their practical applications in data analysis.

## Table of Contents

- [Part A: Generate a Dataset of Normal Distribution with 3 Categories for Synthesizing](#part-a-generate-a-dataset-of-normal-distribution-with-3-categories-for-synthesizing)
- [Part B: Apply Synthpop for the Health Insurance Dataset](#part-b-apply-synthpop-for-the-health-insurance-dataset)
- [Part C: Apply Synthpop for the Headcheck Dataset](#part-c-apply-synthpop-for-the-headcheck-dataset)
- [Part D: Timing of Generating Data with Different Sizes](#part-d-timing-of-generating-data-with-different-sizes)
- [Conclusion](#conclusion)

## Conclusion

The synthetic dataset generated using the Synthpop package exhibits a similar statistical distribution to the original dataset. Through various tests and comparisons, we found that:

- The synthetic dataset retains the distribution characteristics for both categorical and numerical variables, as validated by the KS test.
- Logistic and linear regression models fitted on synthetic data provide results comparable to those obtained from the original data.
- The time required to generate larger synthetic datasets increases significantly, particularly with datasets exceeding 1,000,000 observations and numerous variables.
- The synthetic data effectively covers missing values and outliers, maintaining the integrity of the analysis.

Overall, synthetic data proves to be a valuable tool for preserving data confidentiality while ensuring that statistical analyses remain accurate and reliable. However, addressing potential issues may require additional rules and access to the original dataset and its variable descriptions.

For more details, refer to the project’s GitHub repository [here](https://github.com/jessicaphamca/Generating-Synthetic-Datasets-with-Synthpop/blob/main/Synthetic_Synthpop_Presentation.docx.pdf).

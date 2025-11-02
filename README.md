# Anchor House GPA Analysis

A statistical and machine learning study exploring how dorm orientation (inside- vs. outside-facing rooms) relates to student GPA outcomes at UC Berkeley’s Anchor House residence.

## Overview
This analysis investigates environmental and academic factors influencing GPA among undergraduate residents. Using regression, classification, and simulation methods, the project quantifies the relationship between dorm orientation and academic performance while controlling for prior GPA, major discipline, and housing variables.

## Methods
- **OLS and Logistic Regression** to estimate academic performance effects  
- **Covariate Balance Table** for quasi-experimental comparison  
- **Interaction Modeling** to test heterogeneous effects by prior GPA  
- **Decision Tree Classifier** for predictive exploration of GPA outcomes  
- **Synthetic Data Replication** to test robustness across 100 randomized datasets  

## Technical Stack
| Component | Description |
|------------|-------------|
| Language | R (v4.3+) |
| Libraries | `dplyr`, `ggplot2`, `rpart`, `modelsummary`, `broom`, `tableone`, `margins` |
| Output | Regression tables, diagnostic plots, decision trees |

## Reproduction
1. Clone the repository:  
   ```bash
   git clone https://github.com/ZachUsesG/anchor-house-gpa-analysis.git
   cd anchor-house-gpa-analysis
Open AnchorHouse_GPA_Analysis_ZMakari.R in RStudio

Place Anchor_GPA.csv in a data/ subdirectory

Run the script to generate figures and model outputs

Credits
Created by Zacharia Makari
Developed as part of the Anchor House Behavioral Research Project (UC Berkeley, 2025)

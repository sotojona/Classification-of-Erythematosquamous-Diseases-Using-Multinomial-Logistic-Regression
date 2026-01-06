# Classification of Erythematosquamous Diseases

## Overview
This project presents a data science approach to the classification of erythematosquamous diseases,  including psoriasis, seborrheic dermatitis, lichen planus, pityriasis rosea, chronic dermatitis, and pityriasis rubra pilaris. The objective is to explore statistical methods for early and accurate disease classification using clinical data.


The project was developed as part of my Bachelor’s Thesis in Applied Statistics.
## Dataset
- Dermatology Dataset from the UCI Machine Learning Repository
- Multiclass classification problem
- Clinical and histopathological variables

## Methodology
- Data cleaning and preprocessing in R
- Exploratory Data Analysis (EDA)
  - Descriptive statistics
  - Correlation analysis
- Feature selection and variable exploration
- Application of classification models, with a focus on Multinomial Logistic Regression
- Model evaluation using appropriate performance metrics

## Results
The Multinomial Logistic Regression model did not achieve the expected classification performance, reflecting the complexity and similarity among the diseases. The results suggest that more advanced or alternative classification techniques may be required.

## Libraries Used
This project was developed in R using several libraries commonly employed in data science and statistical analysis:

- **Data exploration and descriptive analysis**:  
  `psych`, `car`

- **Correlation analysis and visualization**:  
  `corrplot`, `polycor`

- **Data visualization**:  
  `ggplot2`, `rpart.plot`

- **Data import/export**:  
  `openxlsx`

- **Statistical modeling and machine learning**:  
  `caret`, `nnet`, `naivebayes`, `rattle`

- **Model evaluation and performance metrics**:  
  `pROC`

- **Parallel computing**:  
  `doParallel`

- **Results presentation**:  
  `stargazer`


## Conclusion
This project highlights the importance of exploratory data analysis and model selection in multiclass medical classification problems. It also serves as a foundation for further experimentation with machine learning techniques in R.

## Author
Jonathan Andrés Soto Botina 
Bachelor’s Degree in Applied Statistics  
Complutense University of Madrid


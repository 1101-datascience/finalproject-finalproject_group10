# [GroupID] Title of your final project

### Groups
* 劉柏毅, 106305022
* 林藝潔, 108703005
* 郭沛澐, 108703006
* 鄭宛薰, 108703003
* 江宏繹, 108304016
* ...

### Goal
The goal of our project is to predict the loyalty of customers towards the payment company Elo, one of the largest payment brands in Brazil. The company has built partnerships with merchants in order to offer promotions or discounts to cardholders, and this data science project will help them to assess their business model and know more about the customers' experience.

### Demo 
You should provide an example commend to reproduce your result
```R
Rscript modeling.R --train train_0109_tg.csv --test test_0106.csv --report performance.csv --predict predict.csv
```


#### on-line visualization
* https://anita56yu.shinyapps.io/data_analysis/
* https://anita56yu.shinyapps.io/model_analysis/

## Folder organization and its related information

### docs
* Your presentation, 1101_datascience_FP_<yourID|groupName>.ppt/pptx/pdf, by **Jan. 13**
* Any related document for the final project
  * papers
  * software user guide

### data

* Kaggle: https://www.kaggle.com/c/elo-merchant-category-recommendation
* Input format
* There are four datasets. One for customer profile, one for past transaction records, one for new transaction records and one for merchant details.
* We first clean the merchant details. the main issue here include duplicated merchants with different properties, missing values in some of the merchant features. Next, we process transaction records by grouping up data from the same card_id and examine there mean, variance and more. Lastly, we try to construct some new features that measures the ratio or the difference between transaction behaviors in two time period.

### code

* Which method do you use?
  * random forest
* What is a null model for comparison?
  * kaggle提供的kaggle值為3.87852
* How do your perform evaluation? ie. cross-validation, or addtional indepedent data set
  * 82分為train、test

### results

* Which metric do you use 
  * MSE、RMSE、R-squared
* Is your improvement significant?
* What is the challenge part of your project?

## References
* Some of our feature engineering is inspired by this notebook: https://www.kaggle.com/samaujs/elo-eda-ml
* One of challenge in the dataset is that lots of features are anonymized, discussion could be found: https://www.kaggle.com/sreeedevi/merchant-rating-variables-revealed
* modeling's code: https://rpubs.com/jiankaiwang/rf
* Code/implementation which you include/reference (__You should indicate in your presentation if you use code for others. Otherwise, cheating will result in 0 score for final project.__)
* Packages you use
 * randomForest
* Related publications

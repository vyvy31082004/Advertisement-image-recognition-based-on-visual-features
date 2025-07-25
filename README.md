# Advertisement-image-recognition-based-on-visual-features
Team member : 3
Build a binary classification model to automatically identify whether an image on a webpage is an advertisement or not, using both visual and text-based features.
Dataset
Source: UCI Machine Learning Repository – Internet Advertisements Data Set

Samples: 3,279 images

Features:
  3 continuous features: height, width, ratio (aspect ratio)
  1,555 binary features: presence of words in image URL, alt text, anchor text, and surrounding content
  1 target: ad (1) vs. non-ad (0)

Data Processing
  Categorical encoding for target (ad = 1, nonad = 0)
  Removed irrelevant columns and handled missing values
  Replaced outliers using the IQR method
  Descriptive statistics & visualizations (histogram, boxplot) revealed:
  Ads tend to have larger width and height
  Non-ads often have larger aspect ratio (ratio)
  Data is right-skewed and contains outliers
  
Modeling Approaches
  Logistic Regression
                      Used to model the probability of an image being an ad
                      Assessed feature significance via coefficients and hypothesis testing

  Decision Tree
                      Applied Gini impurity to split nodes
                      Visual and interpretable, though sensitive to overfitting

Random Forest
                      Used bootstrap sampling and feature bagging
                      Provided better generalization and accuracy
                      Reduced overfitting and handled missing values robustly

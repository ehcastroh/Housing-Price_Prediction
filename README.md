# Price Prediction (Retail Housing) 
## Effects of Macroeconomic Indicators in Ames Dataset Price Predictions


#### Introduction 
A collaborative project for predicting Housing Prices in Ames dataset. 

Project expands upon the commonly used Ames Housing dataset.  Instead of utilizing “micro” predictors, such as square footage, number of bathrooms, etc., we introduce “macro” predictors to predict the Sale Price of homes in Ames.  “Macro” predictors consist of predictors that measure "general economic strength" of the Ames region in Iowa.  Using the acquired macro predictors, the project's goal is (1) to improve upon the existing Ames dataset, and (2) to create a better prediction model of housing prices in the Ames dataset.

#### Motivation
Being able to predict the housing prices in a region such as Ames could be beneficial to several different groups of people.  For numerous financial corporations that specialize in the real estate industry, a better prediction model would allow them to decide whether or not they would like to invest in a region such as Ames.  Another example would be service related businesses.  An increase in housing prices would mean a more populated area and wealthier individuals.  Analysts at these service companies would notice this and decide if their company should open a store in that location.  Ultimately a better predictive model will allow several different groups to make more informed decisions

#### Methodology
For this project we decided to follow two routes in terms of implementing methods for analysis.  First, since we are predicting Sale Price of homes in Ames, based on our “macro” predictors, we first carried out a complete analysis on the generic Ames datset -- the purpose of which was to establish a baseline for comparison.   

The methods for this first part of analysis were:
 
1) Naive Linear Regression
2) “Common Sense” Linear Regression
3) Ridge Regression
4) LASSO
5) Forward Stepwise
6) Random Forest
7) Deep Learning

For the second part of analysis we decided to implement different Time Series methods.  We decided to use Time Series to examine how strong the predictive power of these methods would be in comparison to the previous methods listed above.   The methods utilized for the second part of our analysis were: 

1) Linear Trend 
2) Random Walk
3) Autoregressive
4) ARIMA
5) SARIMA

#### Results

##### First Methodology

1) Naive Linear Regression : This model trained very well on the test set with a R^2 value of 0.521.  Did not use this method for a test set since it is very similar to a common sense linear regression.   

2) Common Sense Linear Regression:  This model gave a very negative R^2 value which indicated that it is a poor model for predicting the Sale Price.

3) Ridge Regression:  Another model which resulted in very poor results which would suggest that it is another poor model for predicting the Sale Price.  

4) LASSO: This model performed the same as Ridge Regression and is a poor model for predicting the Sale Price.  

5) Forward Stepwise:  This model had a much less negative R^2 value but in turn had a much larger error value which leads us to believe that this indicates this is a poor model for predicting the Sale Price. 

6) Random Forest:  The only model we achieved with a positive R^2 value and a minimal amount of error.  While in comparison to our other models this is better, but its predictive ability is still lacking.  

7) Deep Learning: Only able to get the output of the error values for this method, but yielded good error results.  

##### Remark: 
Based on the results above and while working through the assignment we realized that the main source of these lacking results were our predictors.  All of our dataset contained predictors that were of no value towards predicting the Sale Price. 

##### Comparison and Second Methodology

1) Linear model : a simple linear trend model, this model just did a linear regression, fit all the points into a straight line. This model is apparently not good (see appendix figure2).

2) Random walk : the house price is equal the house price of previous time period plus an error term. It looks pretty good, but in fact just a x shift of the origin data set(see appendix figure 3).

3) Autoregressive : the predicted house price for a given month is a weighted average of previous month’s house price(see appendix figure 4).

4) ARIMA (autoregressive integrated moving average) : This model is a generation of an autoregressive moving average model. This model is generally denoted as ARIMA(p,d,q), the performance of this model (see appendix figure 6)
Where  p is the order (number of time lags) of the autoregressive model, d is the degree of differencing (the number of times the data have had past values subtracted), and q is the order of the moving-average model.

5) SARIMA(seasonal autoregressive integrated moving average) ： this model is usually denoted as ARIMA(p,d,q)(P,D,Q)m, where m refers to the number of periods in each season, and the uppercase P,D,Q refer to the autoregressive, differencing, and moving average terms for the seasonal part of the ARIMA model.  For the purpose of good comparison, we assign both of two ARIMA models (1, 1, 7)(see appendix figure 7). In figure 6 and figure 7, we display the image so that it is easily for people to visualize the performance intuitively by put the prediction and testing set in a parallel way.

#### Conclusion and Next Steps
Ideally, our group would have liked to have better results to discuss but this was the reality of our project.  Given more time we would liked to have built a better predictive model that would have allowed us to accurately predict the Sale Prices for the Ames Dataset.  To do this we would have to spent a lot more time in carefully deciding which predictors to use and if those predictors would even be of value towards prediction.  We believe that many of the methods we implemented would have produced more significant results in a working dataset.  Ultimately, we would have wanted our final predictive model to be an ensemble model that included several different methods that we described above.

Any subsequent efforts on the the part of our team to develop a better house-price prediction model would likely need to deviate from the “doll house” nature of the Ames set.  The careful curation of the set makes it a great learning tool, but the limitations can quickly be seen when trully familiar with the data.  We believe that should we encounter the same project formulation in the future, we would depart from our previous set, and instead deeply consider our efforts, our modeling goals, and then create “farm” from which we can pull data from, in our efforts to evolve and improve our modeling formulations..   





## Start Here:
R was used for all data gathering, cleaning, and modeling purposes.

Multiple R scripts were written in order to carry out project. To execute our procedure:

1) Data Cleaning (in order):

    clean_ames.R
    
    varPer_to_qtr.R
    
    stitch.R	
        
2) Explanatory Data Analysis (in no particular order): 

    EDA.Rmd	
    
    EDA_Predicting_House_Prices.R	

3) Modeling and Prediction (in no particular order):

    TS_Predicting_House_Prices.R	
    
    timeDependence.R	
    
    timeseries.Rmd	
    
    142 Ames Analysis.R

### A brief consideration:
##### Majority of labor associated with this project is consumed by data gathering and clean up. Should you choose to expand on our efforts, please reach out to me directly and I can share the raw, preprocessed data sets we used. 




## Linear Regression

R Package for linear regression.

This package calculates -  

### Confidence intervals: The user can choose 

    the significance level α to obtain for the 1−α confidence intervals for β and 
    whether to use the asymptotic or bootstrap approach for this.

### Plots including:

  1. Residuals vs fitted-values (fitted values are y^=Xβ^).
  2. qq-plot of residuals
  3. Histogram (or density) of residuals

### Mean Square Prediction Error (MSPE) computed in matrix form:

    MSPE:=1/n∑i=1n(yi−y^i)2
    where n is the number of observations in the data (i.e. number of rows).

### F-test: The statistic is calculated in matrix form and output the corresponding p-value. 

    With y¯ representing the sample mean of y, let
    SSM:=∑i=1n(y^i−y¯)2,
    SSE:=∑i=1n(yi−y^i)2,and 
    DFM=p−1 and DFE=n−p. 
    Then MSM=SSM/DFM and MSE=SSE/DFE and 
    obtain the F-statistic as follows:
    F∗=MSM/MSE.
Using the appropriate distribution in R, P(F>F∗) which corresponds to the p-value is computed.

## License

The license this source code is released under GNU General Public License

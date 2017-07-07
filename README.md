# ARIMA-Time-Serie-Analysis

Forecasting of S&P House Price Index using Autoregressive integrated moving average (ARIMA) methodology.

Check/Run the scripts in the following order:

1. DataImportFromWeb.R - this imports Housing Price Index data from the webpage

2. DataMunging.R - performs basic data cleaning, reshaping and validation

3. Basic_Overall_Graphs.R - Generates timeseries plot for all the cities.

4. AutoARIMA.R - 
  a. In this script, Stationarity of timeseries have been check using Stationarity test such as shapiro Test; Dickey-Fuller Test; Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test; ACF- PACF Test. This also tells the order of differencing required for each TS.
  b. autoarima function is used, predicitons were generated too.
          

5. ARIMA_Model_Testing_and_Fitting.R - Here, ARIMA model is tested on different values of P-D-Q for each timeseries. Models were fitted on the best performing orders of P-D-Q.

6. ARIMA_Model_Diagnostic_Checks.R - Diagnostic checks (Ljung-Box Test, qqNorm Test) are performed on the fitted models.

7. TimeSeries_Decomposition_Trend_Seasonality.R - Timeseries is decomposed into Seasonality, Trend and Remainder and plotted.

8. ARIMA_Model_Forecasting.R - Predictions for the next 18 months are created and plotted.

9. ARIMA_Prediction_Plots.R - Final prediction plots all together









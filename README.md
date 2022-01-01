# Time-Series

Neo Bank of India (a fictional bank) receives a lot of requests for its various finance offerings that include housing loans, two-wheeler loans, real estate financing, and microloans. The number of applications received is something that varies a lot with the season. Going through these applications is a manual and tedious process.
Accurately forecasting the number of cases received can help with resource and manpower management resulting in quick response on applications and more efficient processing.

The dataset is divided into 2 components, namely train data and test data. The training data consists of dependent variables such as date of application (application_date), business segment (segment 1 or 2), anonymized id for a branch at which application was received (branch_id), State in which application was received (state), zone of state in which application was received (zone). The target variable here is the number of cases/applications received (no_of_applicants).

The test data consists of a unique id for each sample in the test set (id), Date of application (application_date), business segment (segment).

Sample submission consists of id with the number of applicants forecasted for next month for business segments 1 and 2.

Data Preprocessing:
In the given data as a part of pre-processing we divide the data based on the business segments to analyze them individually for the missing values we use Interpolation to fill in the missing values.We combined the dataset with Holiday dataset downloaded from Kaggle through left join. This final dataset is used for further analysis. The Holiday dataset consists of all the Indian holidays for a period of 2017 to 2019.The data type of the Application date is also modified to ‘datetime’ type and the dataframes for indovidual segments are converted to ‘tsibble’ object for time series analysis.

Modeling:
Arima

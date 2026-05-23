# Predicting Tourist Arrivals After COVID-19 Outbreak in Sri Lanka Using Machine Learning

## Overview
- This research project focuses on predicting tourist arrivals in Sri Lanka after the COVID-19 outbreak using Machine Learning and Time Series Forecasting techniques. The system was developed as a web-based tourism demand forecasting platform integrated with interactive Power BI dashboards to support data-driven decision-making for the tourism industry.
- The project compares traditional statistical forecasting methods with machine learning-based forecasting models to identify the most accurate approach for predicting future tourist arrivals.


## Objective
- Analyse the effectiveness of predicting tourist arrivals after COVID-19.
- Develop a web-based tourism demand forecasting system.
- Compare the forecasting accuracy of ARIMA and NNAR models.
- Integrate interactive Power BI dashboards for business insights.
- Enable forecasting report generation and visualisation.


## Technologies & Tools Used
### Programming & Development
- R Programming Language
- RStudio
- R Shiny Framework


### Libraries & Packages
- `forecast`
- `caret`
- `tseries`
- `zoo`

### Database
- MySQL

### Data Visualization
- Power BI

### Methodologies
- Agile Development Methodology
- Time Series Forecasting
- Machine Learning


## Forecasting Models Used

- ARIMA Model: `ARIMA(3,1,1)(0,1,2)[12]`
- NNAR Model: `NNAR(14,1,8)[12]`

The forecasting models were evaluated using:
- RMSE
- MAE
- MAPE
- MASE
- ME
- MPE
- ACF1


## Key Findings
- The NNAR model outperformed the ARIMA model for forecasting tourist arrivals after COVID-19.
- Machine Learning techniques improved forecasting accuracy compared to traditional forecasting methods.
- The developed system supports faster and more efficient tourism demand analysis for decision-makers.

## Features of the System
- User Registration & Authentication
- Tourist Arrival Forecasting
- Multiple Forecasting Model Selection
- Time Range Selection
- Download Forecast Reports (PDF & Excel)
- Interactive Power BI Dashboards
- Dashboard Customization
- Past & Predicted Tourist Arrival Analysis


## Repository Structure
```text
.
├── Excel files/                        # Dataset files
├── rsconnect/shinyapps.io/sltdfs/      # Shiny app hosting credentials
├── www/                                # logo image
└── README.md
```

## Data Source
Tourist arrivals data was collected from:
- [Sri Lanka Tourism Development Authority (SLTDA)](https://sltda.gov.lk/en)
- [Trading Economics](https://tradingeconomics.com/sri-lanka/tourist-arrivals)

Dataset period:
- January 1977 – December 2021

## Web based Tourism Forecasting System
[View](https://sltdfs.shinyapps.io/Sri_Lanka_Tourism_Demand_Forecasting_System/)

## Research Report
[View](PredictingtheTouristArrivalsAfterCovid-19OutbreakinSriLankaUsingMachineLearning.pdf)

## Authors
- **Dr. Sachintha Pitigala (Supervisor)**
- **Nisini Silva**

## License
This project is developed for academic and research purposes.

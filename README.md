# Analysis code for comparison of expert and computational forecasts of Ebola transmission in North Kivu 2019/2020

## Overview
This repo contains the analysis code for the paper "Predicting ongoing transmission and flare-ups in a declining Ebola epidemic: Understanding the relative performance of experts and mathematical models". The breakdown of the codebase is as follows: 

## Additional repositories
The preperation of data from the expert forecasts is found in a seperate repository: https://github.com/rmjlros/Expert-elicitation

Computational forecasts were made using the EpiCastR package: https://github.com/epiforecasts/EpiCastR

## Present repository
All code for evaluating the forecasts can be found in this repository. 


### Running the computational forecasts
The code for making the computational forecasts is found in `run_forecasts_ee.R`

### Combining and cleaning of the forecasts 
The analysis is broken into two components:   
1. Analysis of Healthzones included in the survey
2. Analysis of Healthzones nominated by the experts

Cleaning, combination and scoring of the forecasts made in each of these components are found in: 
`clean_data_surveyed.R`
and
`clean_data_nominated.R`

respectively. 

### Presenting results
The presentation of scores and ranking based on performance is executed in: 
`score_and_rank_combinations_surveyed.R`
and 
`score_and_rank_combinations_nominated.R`

respectively. 

The calculation of hazard rate gap is executed in `calculate_hazard_rates_surveyed.R`






<p align='center'>
<img width="600" alt="image" src="https://github.com/crustyapples/BeatScout/assets/24990448/da5491d1-ebe7-42a2-9ebf-5f934548a7cc">
</p>

<h2 align="center">BeatScout</h2>

<p align="center">
    <a href="https://github.com/crustyapples/BeatScout/blob/main/R%20Scripts/S01_T4_DataExploration.R">Data Exploration</a>
    |
    <a href="https://github.com/crustyapples/BeatScout/blob/main/R%20Scripts/S01_T4_DataPreparation.R">Data Preparation</a>
    |
    <a href="https://github.com/crustyapples/BeatScout/blob/main/R%20Scripts/S01_T4_Models.R">Models</a>
</p>

## What we set out to do
Identifying which artist is going to be the next viral hit is increasingly difficult using by relying on traditional scouting techniques and gut feel. 
We believe scraping and modelling the vast amount of data present on Tiktok can provide an informed decision to help music labels pick their future stars.

## The Results
The performance measures of all 5 models are presented in the table below. After comparing all the metrics, we have chosen the Random Forest (RF) as our best predictive model. 
We notice that despite the model having the best performance scores, the FNR is still at an undesirable level even after using the model’s optimal threshold. 
However, in our context, we prioritise capturing more true positives i.e. having a low FPR. This is because having a high FPR will lead to investing resources in promoting a track that does not have commercial potential, resulting in financial losses. 
This is a more severe consequence to UMG as compared to the consequence of a high FNR - missing out on the signing of an artist with a high profitability potential. 
Hence, it is appropriate for us to compromise on the FNR to ensure low FPR and, subsequently, decide on the RF model as the best predictive model. 



| Classification Model | Accuracy | FPR   | FNR   | F1 Score | F1 Gain |
|----------------------|----------|-------|-------|----------|---------|
| Logistic Regression  | 43.9%    | 69.1% | 12.1% | 0.417    | 0.587   |
| CART                 | 74.0%    | 18.4% | 51.5% | 0.460    | 0.653   |
| Random Forest        | 79.6%    | 11.2% | 51.5% | 0.520    | 0.727   |
| MARS                 | 48.1%    | 57.4% | 33.3% | 0.370    | 0.496   |
| Neural Network       | 59.5%    | 38.1% | 48.5% | 0.368    | 0.491   |

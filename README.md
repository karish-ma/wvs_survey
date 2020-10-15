# wvs_survey
Contains a .do file with code adapted and written for a subset of the data from the original World Values Survey

* Final
*
*
* Specify the version of Stata being used:
version 15.1
*
* Clear all computer memory for Stata to read the new data set:
clear
*
* Define the local directory where I want Stata to find data and save output:
cd "/Users/karis/Desktop/S040 - McIntyre/Final"
*
* Input the dataset:
use "wvs"
*
*
* Reducing the sample to comprise only Denominations 30 = Muslim and 22 = Hindu
keep if denomination == 30 | denomination == 22
*
*
* Generating a dichotomous variable using denomination:
generate Muslim=denomination==30
generate Hindu=denomination==22
tab denomination Muslim
*
*
* Univariate Analysis
*
*
sum equality, detail
sum disbelief, detail
sum education, detail
sum choice, detail
sum defiance, detail
sum denomination, detail
sum Muslim, detail
*
* Displays of the distribution of the variables:
histogram equality, normal kdensity name(Equality_Distribution, replace)
histogram disbelief, normal kdensity name(Disbelief_Distribution, replace)
histogram choice, normal kdensity name(Choice_Distribution, replace)
histogram defiance, normal kdensity name(Defiance_Distribution, replace)
histogram education, normal kdensity name(Education_Distribution, replace)
histogram Muslim, normal kdensity name(Denomination_Distribution, replace)
*
*
* Scatterplots of the outcome over each predictor:
*
* Scatterplot of equality over disbelief:
twoway (scatter equality disbelief), name(equlity_over_belief, replace), ///
(lfit equality disbelief) (lowess equality disbelief)
*
* Scatterplot of equality over choice:
twoway (scatter equality choice), name(equality_over_choice, replace), ///
(lfit equality choice) (lowess equality choice)
*
* Scatterplot of equality over defiance:
twoway (scatter equality defiance), name(equality_over_defiance, replace), ///
GENDER EQUALITY AND RELIGIOSITY: COMPARING ISLAM AND HINDUISM 26
(lfit equality defiance) (lowess equality defiance)
*
* Scatterplot of equality over education:
twoway (scatter equality education), name(equality_over_education, replace), ///
(lfit equality education) (lowess equality education)
*
*
* Generating a visual display of denomination:
graph box Muslim
*
*
* Bivariate Analysis
*
* Correlation Matrix:
pwcorr equality disbelief education choice defiance Muslim, sig
*
*
*
* FITTING MODEL 1 (equality over the key predictors):
eststo drop *
eststo: regress equality disbelief Muslim
*
* CHECKING FOR THE REGRESSION ASSUMPTIONS
* Generating the residuals and standardized residuals:
predict RESM1, residual
predict SRESM1, rstandard
* Histogram of the standard residuals:
histogram SRESM1, bin(20) normal kdensity name(SRESM1_Distribution, replace)
sum SRESM1, detail
* Scatterplot of the standardized residuals vs. the predictor education:
twoway (scatter SRESM1 equality, yline(0)) (lowess SRESM1 equality), ///
name(SRESM1_over_equality, replace)
twoway (scatter SRESM1 disbelief, yline(0)) (lowess SRESM1 disbelief), ///
name(SRESM1_over_disbelief, replace)
twoway (scatter SRESM1 education, yline(0)) (lowess SRESM1 education), ///
name(SRESM1_over_education, replace)
twoway (scatter SRESM1 choice, yline(0)) (lowess SRESM1 choice), ///
name(SRESM1_over_choice, replace)
twoway (scatter SRESM1 defiance, yline(0)) (lowess SRESM1 defiance), ///
name(SRESM1_over_defiance, replace)
*
* Visually representing the association between the outcome and the predictors:
*
* Displaying the model of equality over disbelief
* holding Muslim constant at values 1 (Muslim) and 0 (Hindu):
graph twoway ///
(function y = _b[_cons] + _b[disbelief] * x + _b[Muslim] * 1, range(disbelief)) ///
(function y = _b[_cons] + _b[disbelief] * x + _b[Muslim] * 0, range(disbelief)) ///
(scatter equality disbelief, msymbol(i)), ///
legend(order(1 "Muslim" 2 "Hindu")) ///
xtitle("Disbelief") ytitle("Equality") ///
name(Final_Model_Plot_1, replace)
*
*
*
* FITTING MODEL 2 (equality over all variables):
eststo drop *
eststo: regress equality disbelief Muslim education choice defiance
*
* CHECKING FOR THE REGRESSION ASSUMPTIONS
* Generating the residuals and standardized residuals:
predict RESM2, residual
predict SRESM2, rstandard
GENDER EQUALITY AND RELIGIOSITY: COMPARING ISLAM AND HINDUISM 27
* Histogram of the standard residuals:
histogram SRESM2, bin(20) normal kdensity name(SRESM2_Distribution, replace)
sum SRESM2, detail
* Scatterplot of the standardized residuals vs. the predictor education:
twoway (scatter SRESM2 equality, yline(0)) (lowess SRESM2 equality), ///
name(SRESM2_over_equality, replace)
twoway (scatter SRESM2 disbelief, yline(0)) (lowess SRESM2 disbelief), ///
name(SRESM2_over_disbelief, replace)
twoway (scatter SRESM2 education, yline(0)) (lowess SRESM2 education), ///
name(SRESM2_over_education, replace)
twoway (scatter SRESM2 choice, yline(0)) (lowess SRESM2 choice), ///
name(SRESM2_over_choice, replace)
twoway (scatter SRESM2 defiance, yline(0)) (lowess SRESM2 defiance),///
name(SRESM2_over_defiance, replace)
*
* Visually representing the association between the outcome and the predictors:
*
* Displaying the model of equality over disbelief holding education constant at the mean 5.81,
* choice constant at the mean 0.19, defiance constant at the mean 0.19,
* and Muslim at values 1 (Muslim) and 0 (Hindu):
graph twoway ///
(function y = _b[_cons] + _b[disbelief] * x + _b[education] * 5.81 + ///
_b[choice] * 0.19 + _b[defiance] * 0.19 + _b[Muslim] * 1, range(disbelief)) ///
(function y = _b[_cons] + _b[disbelief] * x + _b[education] * 5.81 + ///
_b[choice] * 0.19 + _b[defiance] * 0.19 + _b[Muslim] * 0, range(disbelief)) ///
(scatter equality disbelief, msymbol(i)), ///
legend(order(1 "Muslim" 2 "Hindu")) ///
xtitle("Disbelief") ytitle("Equality") ///
name(Model2_Plot, replace)
*
*
*
* FITTING MODEL 3 (equality over all variables accounting for an interaction):
*
* Generating a new variable to account for the interaction between disbelief and denomination:
generate disbelief_Muslim = disbelief * Muslim
*
* Fitting Model 3:
eststo drop *
eststo: regress equality disbelief Muslim education choice defiance disbelief_Muslim
*
* Checking for the regression assumptions:
*
* Generating the residuals and standardized residuals:
predict RESM3, residual
predict SRESM3, rstandard
* Histogram of the standard residuals:
histogram SRESM3, bin(20) normal kdensity name(SRESM3_Distribution, replace)
sum SRESM3, detail
* Scatterplot of the standardized residuals vs. the predictor education:
twoway (scatter SRESM3 equality, yline(0)) (lowess SRESM3 equality), ///
name(SRESM3_over_equality, replace)
twoway (scatter SRESM3 disbelief, yline(0)) (lowess SRESM3 disbelief), ///
name(SRESM3_over_disbelief, replace)
twoway (scatter SRESM3 education, yline(0)) (lowess SRESM3 education), ///
name(SRESM3_over_education, replace)
twoway (scatter SRESM3 choice, yline(0)) (lowess SRESM3 choice), ///
name(SRESM3_over_choice, replace)
twoway (scatter SRESM3 defiance, yline(0)) (lowess SRESM3 defiance), ///
name(SRESM3_over_defiance, replace)
*
GENDER EQUALITY AND RELIGIOSITY: COMPARING ISLAM AND HINDUISM 28
* Displaying Model 3 with equality over disbelief
* holding education constant at the mean 5.81,
* choice constant at the mean 0.19, defiance constant at the mean 0.19,
* and Muslim at values 1 (Muslim) and 0 (Hindu):
graph twoway ///
(function y = _b[_cons] + _b[disbelief] * x + _b[education] * 5.81 + ///
_b[choice] * 0.19 + _b[defiance] * 0.19 + _b[Muslim] * 1 + ///
_b[disbelief_Muslim] * x*1, range(disbelief)) ///
(function y = _b[_cons] + _b[disbelief] * x + _b[education] * 5.81 + ///
_b[choice] * 0.19 + _b[defiance] * 0.19 + _b[Muslim] * 0 + ///
_b[disbelief_Muslim] * x*0, range(disbelief)), ///
legend(order(1 "Muslim" 2 "Hindu")) ///
xtitle("Disbelief") ytitle("Equality") ///
name(Model3_Plot, replace)
*
*
* Creating a taxonomy table:
eststo drop *
* Regressing the outcome equality over different predictors:
eststo: regress equality disbelief Muslim
eststo: regress equality disbelief Muslim education choice defiance
eststo: regress equality disbelief Muslim education choice defiance disbelief_Muslim
*
* Taxonomy Table:
esttab using "Final_Taxonomy_Table.rtf", replace ///
cells(b(star fmt(2)) se(par fmt(2)) t(fmt(2))) ///
scalars(r2 F df_m df_r rmse p) legend
*
*clear

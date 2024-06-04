////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Replication and Extension of the Hessels.et.al(2020) paper

// AUTHORS: Jolanda Hessels, Cornelius A. Rietveld, A. Roy Thurik, Peter van der Zwan,
//TITLE: The higher returns to formal education for entrepreneurs versus employees in Australia,
//Journal of Business Venturing Insights, Volume 13, 2020, e00148, ISSN 2352-6734,
//https://doi.org/10.1016/j.jbvi.2019.e00148.

//Abstract: Van Praag et al. (2013) analyze whether the returns to formal education in terms of income differ between entrepreneurs and employees. Using US data (1979–2000), they find that entrepreneurs have higher returns to formal education than employees. They also find evidence that the level of personal control in one's occupation explains these higher returns. In the present study, we aim to replicate these findings using a dataset from a different country (Australia) and time period (2005–2017). Moreover, we extend the study by Van Praag et al. (2013) by distinguishing between entrepreneurs with and without employees. In accordance with Van Praag et al. (2013), we also find higher returns to education for entrepreneurs compared to employees. However, this finding mainly applies to the entrepreneurs without employees. Moreover, we do not find evidence for a mediating role of personal control in this relationship.

//Keywords: Entrepreneurship; Self-employment; Education; Income; Job control; Earnings

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

clear all               // clear memory
capture log close       // close any open log files
set more off            // don't pause when screen fills

// create a new log-file to save code and output
log using Replication_Extension_Hessels_et_al.log, replace  

//stata_additional_packages
*ssc install hildasetup
*ssc install asdoc
*ssc install estout

set maxvar 8000

//HILDA DATA SETUP
hildasetup, data("C:\original_data") saving("C:\new_data")  file(Hilda_Replication) var(xwaveid hhrpid wsfei bifip bifin esempdt jbmo62 jbhruc edhigh edhists jomfw jomls jomfd hgage hgsex mrcurr gh1 fmmsch fmmpsq fmfsch fmfpsq hhstate ancob ctbds ctwps ctsds ghgh ghph ghmh jbmsall losat) rel(20) master(yrenter yrleft) lw(wlea_t)


* IDENTIFIER
// xwaveid - unique person identifier across all waves
// hhrpid - random person identifier

* DEPENDENT VARIABLES
// wsfei - financial year gross wages & salary ($) [imputed] [weighted topcode]
// bifip - financial year business income ($) [imputed] Positive values [weighted topcode]
// bifin - financial year business income ($) [imputed] Negative values

* INDEPENDENT VARIABLES
// esempdt - current employment status (detailed)
// jbmo62 - occupation 2-digit ANZSCO 2006
// jbhruc - hours per week usually worked in all jobs
// edhigh - highest education level achieved
// edhists - highest year of school completed (excludes equivalents obtained post-sc)
// jomfw - "I have a lot of freedom to decide when I do my work, range from 1 to 7"
// jomls - "I have a lot of say about what happens on my job, range from 1 to 7"
// jomfd - "I have a lot of freedom to decide how I do my own work, range from 1 to 7"
//_losat Satisfaction - How satisfied are you with your life 1 to 10
//jbmsall Overall job satisfaction 1 to 10
* CONTROL VARIABLES
// hgage - age (based on each wave)
// hgsex - sex (gender)
// mrcurr - marital status 
// gh1 - self-assessed health
// fmmsch - how much schooling mother completed
// fmmpsq - mother completed an educational qualification after leaving school
// fmfsch - how much schooling father completed
// fmfpsq - father completed an educational qualification after leaving school
// hhstate - geographical location
// ancob - history: country of birth
// ctbds - backwards digits score
// ctbds - word pronunciation score (25-item NART)
// ctsds - symbol-digit modalities score


** DATA CLEANING
//================================================================
* DEPENDENT VARIABLES

// generate dependent variable 'Gross_Labor_Income'
gen Gross_Labor_Income = wsfei + bifip - bifin
drop wsfei bifip bifin

// dropping observations with negative & zero values from 'Gross_Labor_Income'
drop if Gross_Labor_Income <= 0

// logarithmic transformation of 'Gross_Labor_Income'
gen Log_Gross_Labor_Income = log(Gross_Labor_Income)
drop Gross_Labor_Income
//=====================================================================
* INDEPENDENT VARIABLES

// 'Entrepreneur' variable based on employment status
drop if esempdt == -10 | esempdt == -4 | esempdt == -1 | esempdt == 6
gen Entrepreneur = .
replace Entrepreneur = 0 if esempdt == 1
replace Entrepreneur = 2 if esempdt == 2 | esempdt == 4
replace Entrepreneur = 3 if esempdt == 3 | esempdt == 5
label define EntrepreneurLabel 0 "[0] Wageworkers" 2 "[2] Entrepreneurs with employees" 3 "[3] Entrepreneurs without employees"
label values Entrepreneur EntrepreneurLabel
drop esempdt

gen EntrepreneurGroup = (Entrepreneur == 2 | Entrepreneur == 3)
label variable EntrepreneurGroup "Entrepreneur with or without employees"
label define EntrepreneurGroupLabel 0 "[0]Non-Entrepreneur" 1 "[1]Entrepreneur"
label values EntrepreneurGroup EntrepreneurGroupLabel

// Annual hours for all jobs 
gen Annual_Hours_All_Jobs = jbhruc * 52
drop if Annual_Hours_All_Jobs <= 300
drop Annual_Hours_All_Jobs

// Occupation_Classification variable based on 2-digit ANZSCO 2006
gen Occupation_Classification = jbmo62
drop if jbmo62 == 12
drop jbmo62

//Education Variable
drop if edhists == -7 | edhists == -4 | edhists == -3 | edhists == -1
gen Education = .

replace Education = 8 if edhists == 5 | edhists == 6 | edhists == 7 | edhists == 8 | edhists == 9
replace Education = 9 if edhists == 4
replace Education = 10 if edhists == 3
replace Education = 11 if edhists == 2
replace Education = 12 if edhists == 1 | edhigh == 4 | edhigh == 5
replace Education = 15 if edhigh == 3
replace Education = 16 if edhigh == 2
replace Education = 17 if edhigh == 1

drop edhigh
drop edhists

// Job Control variable
drop if inlist(jomfw, -8, -6, -5, -4, -1)
drop if inlist(jomls, -8, -6, -5, -4, -1)
drop if inlist(jomfd, -8, -6, -5, -4, -1)
gen Job_Control = (jomfw + jomls + jomfd) / 3

// calculate the cronbach's alpha for 'jomfw', 'jomls', 'jomfd'
alpha jomfw jomls jomfd

// standardize 'Job_Control'
egen mean_Job_Control = mean(Job_Control)
egen sd_Job_Control = sd(Job_Control)
gen std_Job_Control = (Job_Control - mean_Job_Control) / sd_Job_Control
drop mean_Job_Control sd_Job_Control Job_Control
rename std_Job_Control Job_Control

drop jomfw jomls jomfd
summarize Job_Control Education
* CONTROL VARIABLES

// filter by 'wave' for years 2005 to 2017
gen Year = wave + 2000
keep if Year >= 2005 & Year <= 2017

// filter by 'hgage' for individuals between 16 and 64 years
gen Age = hgage
keep if Age >= 16 & Age <= 64
drop hgage

// generate variable 'Gender'
recode hgsex (2 = 0) (1 = 1)
gen Gender = hgsex
drop hgsex

// generate variable 'Married'
drop if mrcurr == -4 | mrcurr == -3
recode mrcurr (1 2 = 1) (3/6 = 0) 
gen Married = mrcurr
drop mrcurr

// generate variable 'Healthy'
drop if gh1 == -5 | gh1 == -4
recode gh1 (1 2 3 = 0) (4 5 = 1)
gen Healthy = gh1
drop gh1

// generate variable 'Education_Father'
drop if fmfsch == -7 | fmfsch == -4 | fmfsch == -3
gen Education_Father = .
replace Education_Father = 1 if fmfsch == 1 // None
replace Education_Father = 2 if fmfsch == 2 // Primary 
replace Education_Father = 3 if fmfsch == 3 // Some secondary school
replace Education_Father = 4 if fmfsch == 4 // Secondary low
replace Education_Father = 5 if fmfsch == 5 // Secondary high
replace Education_Father = 6 if fmfpsq == 1 // Post-secondary
drop fmfsch fmfpsq

label define eduFatherLabel 1 "[1] None" 2 "[2] Primary" 3 "[3] Some secondary school" 4 "[4] Secondary low" 5 "[5] Secondary high" 6 "[6] Post-secondary"
label values Education_Father eduFatherLabel


// generate variable 'Education_Mother'
drop if fmmsch == -7 | fmmsch == -4 | fmmsch == -3
gen Education_Mother = .
replace Education_Mother = 1 if fmmsch == 1 // None
replace Education_Mother = 2 if fmmsch == 2 // Primary
replace Education_Mother = 3 if fmmsch == 3 // Some secondary school
replace Education_Mother = 4 if fmmsch == 4 // Secondary low
replace Education_Mother = 5 if fmmsch == 5 // Secondary high
replace Education_Mother = 6 if fmmpsq == 1 // Post-secondary
drop fmmsch fmmpsq

label define eduMotherLabel 1 "[1] None" 2 "[2] Primary" 3 "[3] Some secondary-school" 4 "[4] Secondary-low" 5 "[5] Secondary-high" 6 "[6] Post-secondary"
label values Education_Mother eduFatherLabel

// generate variable 'Living_In_Territory'
gen Living_In_Territory = hhstate
drop hhstate
label define TerritoryLabel 1 "[1] NSW" 2 "[2] VIC" 3 "[3] QLD" 4 "[4] SA" 5 "[5] WA" 6 "[6] TAS" 7 "[7] NT" 8 "[8] ACT"
label values Living_In_Territory TerritoryLabel

// Born in aus
drop if ancob == -10 | ancob == -4
gen Born_In_Australia = (ancob == 1101)
drop ancob

// 'Hours_Worked_Per_Week' variable
gen Hours_Worked_Per_Week = jbhruc
drop jbhruc

* Calculate average scores
bysort xwaveid: egen mean_ctbds = mean(ctbds)
bysort xwaveid: egen mean_ctwps = mean(ctwps)
bysort xwaveid: egen mean_ctsds = mean(ctsds)

* Remove age and education effects
regress mean_ctbds Age i.Education
predict residuals_ctbds, residuals

regress mean_ctwps Age i.Education
predict residuals_ctwps, residuals

regress mean_ctsds Age i.Education
predict residuals_ctsds, residuals

* Standardize residuals
egen std_res_ctbds = std(residuals_ctbds)
egen std_res_ctwps = std(residuals_ctwps)
egen std_res_ctsds = std(residuals_ctsds)

* Factor analysis
*gen Cognitive_Ability = (std_res_ctwps + std_res_ctsds) / 2
factor std_res_ctbds std_res_ctwps std_res_ctsds, factors(1)
predict Cognitive_Ability

// Drop variables not use
drop wlea_t yrenter yrleft ctbds ctwps ctsds mean_ctbds mean_ctwps mean_ctsds residuals_ctbds residuals_ctwps residuals_ctsds std_res_ctbds std_res_ctwps std_res_ctsds

* Preparations

destring xwaveid, replace
destring hhrpid, replace

// Table 1
// Overall descriptive statistics
preserve
asdoc sum Log_Gross_Labor_Income Education Job_Control Cognitive_Ability Age Hours_Worked_Per_Week Gender Married Healthy i.Education_Father i.Education_Mother i.Living_In_Territory Born_In_Australia, save(Descriptive_Stats_All.doc) replace
restore

// Create subsets for each group
preserve
keep if Entrepreneur == 3
asdoc sum Log_Gross_Labor_Income Education Job_Control Cognitive_Ability Age Hours_Worked_Per_Week Gender Married Healthy i.Education_Father i.Education_Mother i.Living_In_Territory Born_In_Australia, save(Descriptive_Stats_without.doc) replace
restore

preserve
keep if Entrepreneur == 2
asdoc sum Log_Gross_Labor_Income Education Job_Control Cognitive_Ability Age Hours_Worked_Per_Week Gender Married Healthy i.Education_Father i.Education_Mother i.Living_In_Territory Born_In_Australia, save(Descriptive_Stats_with.doc) replace
restore

preserve
keep if Entrepreneur == 0
asdoc sum Log_Gross_Labor_Income Education Job_Control Cognitive_Ability Age Hours_Worked_Per_Week Gender Married Healthy i.Education_Father i.Education_Mother i.Living_In_Territory Born_In_Australia, save(Descriptive_Stats_worker.doc) replace
restore



* Table 2

// Setting the panel data structure
xtset hhrpid Year

// Panel A: Cognitive ability not included

gen EducationEntrepreneur = Education*EntrepreneurGroup
gen AbilityEntrepreneur = Cognitive_Ability*EntrepreneurGroup

// Entrepreneurs (RE) 
xtreg Log_Gross_Labor_Income Education EntrepreneurGroup EducationEntrepreneur Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week if EntrepreneurGroup == 1, re
eststo model1A_Entrepreneurs_RE

// Employees (RE)
xtreg Log_Gross_Labor_Income Education EntrepreneurGroup EducationEntrepreneur Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week if EntrepreneurGroup == 0, re
eststo model2A_Employees_RE
esttab, label
// All (RE)
xtreg Log_Gross_Labor_Income Education EntrepreneurGroup EducationEntrepreneur Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, re
eststo model3A_All_RE
esttab, label

// All (FE)
xtreg Log_Gross_Labor_Income Education EntrepreneurGroup EducationEntrepreneur Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, fe
eststo model4A_All_FE
esttab, label

esttab using Table_trial_A.rtf, se nostar r2 label mtitle(Entrepreneurs_RE Employees_RE All_RE All_FE)

eststo clear

// Panel B: Cognitive ability included

// Entrepreneurs (RE) 
xtreg Log_Gross_Labor_Income Education EntrepreneurGroup EducationEntrepreneur Cognitive_Ability AbilityEntrepreneur Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week if EntrepreneurGroup == 1, re
eststo model1B_Entrepreneurs_RE

// Employees (RE)
xtreg Log_Gross_Labor_Income Education EntrepreneurGroup EducationEntrepreneur Cognitive_Ability AbilityEntrepreneur Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week if EntrepreneurGroup == 0, re
eststo model2B_Employees_RE

// All (RE)
xtreg Log_Gross_Labor_Income Education EntrepreneurGroup EducationEntrepreneur Cognitive_Ability AbilityEntrepreneur Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, re
eststo model3B_All_RE

// All (FE)
xtreg Log_Gross_Labor_Income Education EntrepreneurGroup EducationEntrepreneur Cognitive_Ability AbilityEntrepreneur Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, fe
eststo model4B_All_FE

esttab using Table_trial_2_B.rtf, se nostar r2 label mtitle(Entrepreneurs_RE Employees_RE All_RE All_FE)

eststo clear


// Table 3



// Panel A: Cognitive ability not included
egen Education_mean = mean(Education), by(hhrpid)
gen Education_demeaned = Education - Education_mean //1
gen EdemeanedEntrepreneur = Education_demeaned*EntrepreneurGroup //2
gen JobControlEdemeaned = Job_Control*Education_demeaned //3
gen JobControlEntrepreneur = Job_Control*EntrepreneurGroup //4
gen JobControlEntrepreneurEdemeaned = Job_Control*EntrepreneurGroup*Education_demeaned // 5



// All (RE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur Job_Control JobControlEdemeaned JobControlEntrepreneur JobControlEntrepreneurEdemeaned Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, re
eststo model2A_All_RE


xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur Job_Control JobControlEdemeaned JobControlEntrepreneur JobControlEntrepreneurEdemeaned Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, re
eststo model2A_All_RE

// All (FE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur Job_Control JobControlEdemeaned JobControlEntrepreneur JobControlEntrepreneurEdemeaned Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, fe
eststo model2A_All_FE

esttab using Table3_A.rtf, se nostar r2 label mtitle(All_RE All_FE)

eststo clear

// Panel B: Cognitive ability included

// All (RE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur Job_Control JobControlEdemeaned JobControlEntrepreneur JobControlEntrepreneurEdemeaned Cognitive_Ability AbilityEntrepreneur Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, re
eststo model2B_All_RE


// All (FE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur Job_Control JobControlEdemeaned JobControlEntrepreneur JobControlEntrepreneurEdemeaned Cognitive_Ability AbilityEntrepreneur Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, fe
eststo model2B_All_FE

esttab using Table3_B.rtf, se nostar r2 label mtitle(All_RE All_FE)


eststo clear



//==============================================


// Table 4

// Generate triple interaction

gen EWO = (Entrepreneur == 3)
gen EW = (Entrepreneur == 2)

gen EEWO = Education * EWO
gen EEW = Education * EW

gen AEWO = Cognitive_Ability * EWO
gen AEW = Cognitive_Ability * EW

// Panel A: Cognitive ability not included

// Entrepreneurs (RE) 
xtreg Log_Gross_Labor_Income Education Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week if Entrepreneur == 3, re
eststo model1A_Entrepreneurs_RE

// Employees (RE)
xtreg Log_Gross_Labor_Income Education Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week if Entrepreneur == 2, re
eststo model2A_Employees_RE

// All (RE)
xtreg Log_Gross_Labor_Income Education EWO EW EEWO EEW Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week , re
eststo model3A_All_RE

// All (FE)
xtreg Log_Gross_Labor_Income EWO EW EEWO EEW Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week , fe
eststo model4A_All_FE

esttab using Table4_A_d.rtf, se nostar r2 label

eststo clear

// Panel B: Cognitive ability included

// Entrepreneurs (RE) 
xtreg Log_Gross_Labor_Income Education Cognitive_Ability Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week if Entrepreneur == 3, re
eststo model1B_Entrepreneurs_RE

// Employees (RE)
xtreg Log_Gross_Labor_Income Education Cognitive_Ability Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week if Entrepreneur == 2, re
eststo model2B_Employees_RE

// All (RE)
xtreg Log_Gross_Labor_Income Education EWO EW EEWO EEW AEWO AEW Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week , re
eststo model3B_All_RE

// All (FE)
xtreg Log_Gross_Labor_Income EWO EW EEWO EEW AEWO AEW Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week , fe
eststo model4B_All_FE

esttab using Table4_B.rtf, se nostar r2 label

eststo clear

// Table 5

// Generate triple interaction

gen JE = Job_Control*Education
gen JEWO = Job_Control*EWO
gen JEW = Job_Control*EW
gen JEWOED = Job_Control*EWO*Education_demeaned
gen JEWED = Job_Control*EW*Education_demeaned


// Panel A: Cognitive ability not included

// All (RE)
xtreg Log_Gross_Labor_Income Education EWO EW EEWO EEW Job_Control JE JEWO JEW JEWOED JEWED Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, re
eststo model5A_All_RE

// All (FE)
xtreg Log_Gross_Labor_Income EWO EW EEWO EEW Job_Control JE JEWO JEW JEWOED JEWED Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, fe
eststo model5A_All_FE

esttab using Table5_A.rtf, se nostar r2 label

eststo clear

// Panel B: Cognitive ability included

// All (RE)
xtreg Log_Gross_Labor_Income Education EWO EW EEWO EEW Job_Control JE JEWO JEW JEWOED JEWED Cognitive_Ability AEWO AEW Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, re
eststo model5B_All_RE


// All (FE)
xtreg Log_Gross_Labor_Income EWO EW EEWO EEW Job_Control JE JEWO JEW JEWOED JEWED Cognitive_Ability AEWO AEW Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, fe
eststo model5B_All_FE

esttab using Table5_B.rtf, se nostar r2 label

eststo clear

//==============================================================

drop if losat == -4 | losat == -3
drop if jbmsall == -4 | jbmsall ==-3
//======================================================

// Table 3

// Panel A: Cognitive ability not included
gen AE = Cognitive_Ability*EntrepreneurGroup
summarize losat
scalar mean_losat = r(mean)

* Demean the variable 'losat'
gen losat_demeaned = losat - mean_losat
drop losat
rename losat_demeaned losat

gen LS_Edmean = losat * Education_demeaned //3
gen LS_Entrepreneur = losat * EntrepreneurGroup //4
gen LS_EntEdmean = losat * EntrepreneurGroup * Education_demeaned //5

// All (RE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur losat LS_Edmean LS_Entrepreneur LS_EntEdmean Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, re
eststo model2A_All_RE

// All (FE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur losat LS_Edmean LS_Entrepreneur LS_EntEdmean Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, fe
eststo model2A_All_FE

esttab using Table3_A_LifeSatisfaction.rtf, se nostar r2 label

eststo clear

// Panel B: Cognitive ability included

// All (RE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur losat LS_Edmean LS_Entrepreneur LS_EntEdmean Cognitive_Ability AE Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, re
eststo model2B_All_RE

// All (FE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur losat LS_Edmean LS_Entrepreneur LS_EntEdmean Cognitive_Ability AE Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, fe
eststo model2B_All_FE

esttab using Table3_B_LifeSatisfaction.rtf, se nostar r2 label

eststo clear

//====================================================
// Panel A: Cognitive ability not included
// Summarize jbmsall
summarize jbmsall
scalar mean_jbmsall = r(mean)

// Demean the variable 'jbmsall'
gen jbmsall_demeaned = jbmsall - mean_jbmsall
drop jbmsall
rename jbmsall_demeaned jbmsall

gen JS_Edmean = jbmsall * Education_demeaned //3
gen JS_Entrepreneur = jbmsall * EntrepreneurGroup //4
gen JS_EntEdmean = jbmsall * EntrepreneurGroup * Education_demeaned //5



// All (RE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur jbmsall JS_Edmean JS_Entrepreneur JS_EntEdmean Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, re
eststo model2A_All_RE

// All (FE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur jbmsall JS_Edmean JS_Entrepreneur JS_EntEdmean Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, fe
eststo model2A_All_FE

esttab using Table3_A_JobSatisfaction.rtf, se nostar r2 label

eststo clear

// Panel B: Cognitive ability included

// All (RE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur jbmsall JS_Edmean JS_Entrepreneur JS_EntEdmean Cognitive_Ability AE Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, re
eststo model2B_All_RE

// All (FE)
xtreg Log_Gross_Labor_Income Education_demeaned EntrepreneurGroup EdemeanedEntrepreneur jbmsall JS_Edmean JS_Entrepreneur JS_EntEdmean Cognitive_Ability AE Age Gender Married Healthy Education_Father Education_Mother Living_In_Territory Born_In_Australia Hours_Worked_Per_Week, fe
eststo model2B_All_FE

esttab using Table3_B_JobSatisfaction.rtf, se nostar r2 label

eststo clear

// ====================================================
// Postscript
// ====================================================
log close       // close log-file
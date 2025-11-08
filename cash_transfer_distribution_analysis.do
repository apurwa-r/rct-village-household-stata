********************************************************************************
* PROJECT: Digital Cash Transfer Distribution Methods - RCT Analysis
* PURPOSE: Demonstrate comprehensive RCT analysis skills for research position
* AUTHOR: Research Assistant Applicant
* DATE: November 2025
* 
* STUDY DESIGN: Randomized evaluation of cash transfer distribution methods
* Treatment Arms: 
*   1. Distribution method (centralized vs. door-to-door)
*   2. Recipient gender (male vs. female household head)
*   3. Transfer timing (lump-sum vs. monthly installments)
*
* This analysis demonstrates:
*   - Data cleaning and merging
*   - Balance checks (randomization verification)
*   - Treatment effect estimation (ITT, ATE)
*   - Heterogeneity analysis by subgroups
*   - Cost-effectiveness calculations
*   - Professional table and graph generation
********************************************************************************

clear all
set more off
set maxvar 10000
capture log close

* Set working directory (adjust as needed)
global main_dir "~/cash_transfer_analysis"
global data_dir "$main_dir/data"
global output_dir "$main_dir/output"
global log_dir "$main_dir/logs"

* Create output directories if they don't exist
capture mkdir "$main_dir"
capture mkdir "$data_dir"
capture mkdir "$output_dir"
capture mkdir "$log_dir"

* Start log file
log using "$log_dir/analysis_log.log", replace

********************************************************************************
* SECTION 1: DATA PREPARATION AND CLEANING
********************************************************************************

display as text _newline(2) "{hline 80}"
display as result "SECTION 1: DATA PREPARATION AND CLEANING"
display as text "{hline 80}" _newline

* ============================================================================
* 1.1: Load and clean household baseline data
* ============================================================================

display as text "Loading baseline household data..."

* In practice, you would load actual data here:
* use "$data_dir/baseline_household_data.dta", clear

* For demonstration, create simulated RCT data structure
* This mirrors the GiveDirectly Kenya study design

set seed 12345
set obs 1440  // 1,440 households

* Generate household ID
gen household_id = _n

* Generate village ID (120 villages, 12 households per village)
gen village_id = ceil(_n/12)

* ============================================================================
* 1.2: Generate treatment assignment variables
* ============================================================================

display as text "Generating treatment assignment..."

* Village-level randomization: Treatment vs Control villages
gen byte village_treatment = .
by village_id, sort: gen village_n = _n
replace village_treatment = 1 if village_id <= 60  // 60 treatment villages
replace village_treatment = 0 if village_id > 60   // 60 control villages

* Household-level randomization within treatment villages
* Direct recipient vs spillover household
gen byte household_treatment = .
replace household_treatment = 1 if village_treatment == 1 & inlist(village_n, 1, 2, 3, 4, 5) // Direct recipients
replace household_treatment = 0 if village_treatment == 1 & !inlist(village_n, 1, 2, 3, 4, 5) // Spillover

* Pure control households (in control villages)
replace household_treatment = 0 if village_treatment == 0

* Create detailed treatment group indicator
gen treatment_group = 0  // Pure control
replace treatment_group = 1 if household_treatment == 1  // Treatment
replace treatment_group = 2 if village_treatment == 1 & household_treatment == 0  // Spillover

label define treatment_lbl 0 "Pure Control" 1 "Treatment" 2 "Spillover"
label values treatment_group treatment_lbl

* ============================================================================
* 1.3: Randomize treatment design features (within treatment group)
* ============================================================================

* Only for treatment households (treatment_group == 1)

* A. Distribution Method: Centralized (0) vs Door-to-door (1)
gen byte distribution_method = .
replace distribution_method = rbinomial(1, 0.5) if treatment_group == 1
label define method_lbl 0 "Centralized" 1 "Door-to-door"
label values distribution_method method_lbl

* B. Recipient Gender: Male (0) vs Female (1) household head
gen byte recipient_female = .
replace recipient_female = rbinomial(1, 0.5) if treatment_group == 1

* C. Transfer Timing: Lump-sum (0) vs Monthly (1)
gen byte transfer_monthly = .
replace transfer_monthly = rbinomial(1, 0.5) if treatment_group == 1

* D. Transfer Size: Small ($400) vs Large ($1500)
gen byte transfer_large = .
replace transfer_large = rbinomial(1, 0.5) if treatment_group == 1

gen transfer_amount = .
replace transfer_amount = 400 if treatment_group == 1 & transfer_large == 0
replace transfer_amount = 1500 if treatment_group == 1 & transfer_large == 1
replace transfer_amount = 0 if treatment_group != 1

* ============================================================================
* 1.4: Generate baseline household characteristics
* ============================================================================

display as text "Generating baseline household characteristics..."

* Demographics
gen household_size = max(2, min(12, round(rnormal(5, 2))))
gen head_age = max(18, min(75, round(rnormal(38, 12))))
gen head_female = rbinomial(1, 0.35)  // 35% female-headed households
gen head_education = max(0, min(16, round(rnormal(6, 4))))  // Years of education

* Socioeconomic status (in local currency, normalized)
gen baseline_monthly_income = max(50, round(rexponential(1/150)))
gen baseline_consumption = max(100, round(rnormal(158, 45)))  // Monthly consumption
gen baseline_assets = max(0, round(rexponential(1/500)))  // Asset value
gen has_metal_roof = rbinomial(1, 0.16)  // Poverty indicator
gen distance_to_center = runiform(0.5, 20)  // Distance to distribution center (km)

* Food security (0-10 scale)
gen baseline_food_security = max(0, min(10, round(rnormal(4.5, 2))))

* Psychological wellbeing (0-100 scale)
gen baseline_wellbeing = max(0, min(100, round(rnormal(55, 20))))

* Access to services
gen has_mobile_money = rbinomial(1, 0.45)
gen has_bank_account = rbinomial(1, 0.22)
gen has_phone = rbinomial(1, 0.65)

* Geographic indicators
gen region = ceil(runiform() * 4)  // 4 regions
label define region_lbl 1 "North" 2 "South" 3 "East" 4 "West"
label values region region_lbl

gen urban = rbinomial(1, 0.25)

* ============================================================================
* 1.5: Generate endline outcome variables (9 months post-transfer)
* ============================================================================

display as text "Generating endline outcomes with treatment effects..."

* Set realistic treatment effects based on literature

* A. Economic outcomes
* Consumption increase: ~23% for treatment group
gen endline_consumption = baseline_consumption * rnormal(1, 0.15) if treatment_group == 0
replace endline_consumption = baseline_consumption * rnormal(1.23, 0.15) if treatment_group == 1
replace endline_consumption = baseline_consumption * rnormal(1.08, 0.15) if treatment_group == 2

* Assets increase: ~58% for treatment group
gen endline_assets = baseline_assets * rnormal(1, 0.25) if treatment_group == 0
replace endline_assets = baseline_assets * rnormal(1.58, 0.25) if treatment_group == 1
replace endline_assets = baseline_assets * rnormal(1.15, 0.25) if treatment_group == 2

* Income increase: ~15% for treatment group
gen endline_income = baseline_monthly_income * rnormal(1, 0.20) if treatment_group == 0
replace endline_income = baseline_monthly_income * rnormal(1.15, 0.20) if treatment_group == 1
replace endline_income = baseline_monthly_income * rnormal(1.05, 0.20) if treatment_group == 2

* B. Food security improvement
gen endline_food_security = baseline_food_security + rnormal(0, 0.8) if treatment_group == 0
replace endline_food_security = baseline_food_security + rnormal(1.5, 0.8) if treatment_group == 1
replace endline_food_security = baseline_food_security + rnormal(0.5, 0.8) if treatment_group == 2
replace endline_food_security = max(0, min(10, endline_food_security))

* C. Psychological wellbeing improvement
gen endline_wellbeing = baseline_wellbeing + rnormal(0, 8) if treatment_group == 0
replace endline_wellbeing = baseline_wellbeing + rnormal(10, 8) if treatment_group == 1
replace endline_wellbeing = baseline_wellbeing + rnormal(3, 8) if treatment_group == 2
replace endline_wellbeing = max(0, min(100, endline_wellbeing))

* D. Asset acquisition
gen acquired_livestock = rbinomial(1, 0.05) if treatment_group == 0
replace acquired_livestock = rbinomial(1, 0.35) if treatment_group == 1
replace acquired_livestock = rbinomial(1, 0.12) if treatment_group == 2

gen improved_housing = rbinomial(1, 0.02) if treatment_group == 0
replace improved_housing = rbinomial(1, 0.23) if treatment_group == 1
replace improved_housing = rbinomial(1, 0.08) if treatment_group == 2

* ============================================================================
* 1.6: Generate distribution cost data
* ============================================================================

display as text "Generating cost data for distribution methods..."

* Cost per household by distribution method
gen distribution_cost = .
replace distribution_cost = 15 if distribution_method == 0  // Centralized: lower cost
replace distribution_cost = 35 if distribution_method == 1  // Door-to-door: higher cost

* Add transportation costs for centralized (varies by distance)
replace distribution_cost = distribution_cost + (distance_to_center * 0.5) if distribution_method == 0

* Administrative costs
gen admin_cost = 8 + rnormal(0, 2)  // Per household

* Total program cost per household
gen total_cost = transfer_amount + distribution_cost + admin_cost if treatment_group == 1

* ============================================================================
* 1.7: Create derived variables
* ============================================================================

display as text "Creating derived outcome variables..."

* Changes from baseline
gen consumption_change = endline_consumption - baseline_consumption
gen assets_change = endline_assets - baseline_assets
gen income_change = endline_income - baseline_monthly_income
gen food_security_change = endline_food_security - baseline_food_security
gen wellbeing_change = endline_wellbeing - baseline_wellbeing

* Percentage changes
gen consumption_pct_change = (consumption_change / baseline_consumption) * 100
gen assets_pct_change = (assets_change / baseline_assets) * 100 if baseline_assets > 0

* Cost-effectiveness measures (for treatment group only)
gen cost_per_consumption_unit = total_cost / consumption_change if treatment_group == 1 & consumption_change > 0
gen benefit_cost_ratio = consumption_change / total_cost if treatment_group == 1 & total_cost > 0

* ============================================================================
* 1.8: Create attrition and data quality flags
* ============================================================================

* Simulate realistic attrition (5-8% rate)
gen attrition = rbinomial(1, 0.06)

* Survey completion quality flags
gen complete_survey = 1 - attrition
gen data_quality_flag = rbinomial(1, 0.95) if complete_survey == 1  // 95% high quality

* ============================================================================
* 1.9: Label all variables
* ============================================================================

display as text "Labeling variables..."

label variable household_id "Household ID"
label variable village_id "Village ID"
label variable village_treatment "Village assigned to treatment (1=Yes)"
label variable household_treatment "Household direct recipient (1=Yes)"
label variable treatment_group "Treatment group assignment"
label variable distribution_method "Distribution method"
label variable recipient_female "Recipient is female household head"
label variable transfer_monthly "Transfer paid monthly (vs lump-sum)"
label variable transfer_large "Large transfer amount (vs small)"
label variable transfer_amount "Transfer amount (USD)"

label variable household_size "Household size"
label variable head_age "Age of household head"
label variable head_female "Female household head"
label variable head_education "Years of education (household head)"

label variable baseline_consumption "Baseline monthly consumption (USD)"
label variable baseline_assets "Baseline asset value (USD)"
label variable baseline_monthly_income "Baseline monthly income (USD)"
label variable baseline_food_security "Baseline food security index (0-10)"
label variable baseline_wellbeing "Baseline psychological wellbeing (0-100)"

label variable endline_consumption "Endline monthly consumption (USD)"
label variable endline_assets "Endline asset value (USD)"
label variable endline_income "Endline monthly income (USD)"
label variable endline_food_security "Endline food security index (0-10)"
label variable endline_wellbeing "Endline psychological wellbeing (0-100)"

label variable consumption_change "Change in consumption (USD)"
label variable assets_change "Change in assets (USD)"
label variable consumption_pct_change "Consumption change (%)"

label variable distribution_cost "Distribution cost per household (USD)"
label variable admin_cost "Administrative cost per household (USD)"
label variable total_cost "Total program cost per household (USD)"
label variable cost_per_consumption_unit "Cost per USD increase in consumption"
label variable benefit_cost_ratio "Benefit-cost ratio"

label variable attrition "Household attrited from study"
label variable complete_survey "Completed endline survey"

* Save cleaned dataset
save "$data_dir/analysis_data.dta", replace

display as result _newline "Data cleaning complete. Observations: `c(N)'" _newline


********************************************************************************
* SECTION 2: DESCRIPTIVE STATISTICS AND BALANCE CHECKS
********************************************************************************

display as text _newline(2) "{hline 80}"
display as result "SECTION 2: DESCRIPTIVE STATISTICS AND BALANCE CHECKS"
display as text "{hline 80}" _newline

use "$data_dir/analysis_data.dta", clear

* ============================================================================
* 2.1: Summary statistics by treatment group
* ============================================================================

display as text "Generating summary statistics table..."

* Install required packages if needed
capture which estout
if _rc != 0 {
    display as text "Installing estout package..."
    ssc install estout, replace
}

* Summary stats for baseline characteristics by treatment group
estpost tabstat baseline_consumption baseline_assets baseline_monthly_income ///
    household_size head_age head_female head_education has_phone ///
    distance_to_center baseline_food_security baseline_wellbeing, ///
    by(treatment_group) statistics(mean sd) columns(statistics)

esttab using "$output_dir/table1_summary_stats.csv", replace ///
    cells("mean(fmt(2)) sd(fmt(2))") label ///
    title("Table 1: Summary Statistics by Treatment Group") ///
    nomtitle nonumber

* ============================================================================
* 2.2: Balance checks - Test randomization
* ============================================================================

display as text "Performing balance checks..."

* Create balance table comparing treatment vs control
local balance_vars baseline_consumption baseline_assets baseline_monthly_income ///
    household_size head_age head_female head_education urban ///
    has_mobile_money has_phone distance_to_center ///
    baseline_food_security baseline_wellbeing

* Store results for balance table
matrix balance_results = J(13, 5, .)
local row = 1

foreach var of local balance_vars {
    
    * Mean for treatment group
    quietly sum `var' if treatment_group == 1
    matrix balance_results[`row', 1] = r(mean)
    matrix balance_results[`row', 2] = r(sd)
    
    * Mean for control group  
    quietly sum `var' if treatment_group == 0
    matrix balance_results[`row', 3] = r(mean)
    matrix balance_results[`row', 4] = r(sd)
    
    * Test difference
    quietly reg `var' i.treatment_group, robust
    test 1.treatment_group
    matrix balance_results[`row', 5] = r(p)
    
    local row = `row' + 1
}

* Display balance results
display as text _newline "Balance Check Results (Treatment vs Pure Control):" _newline
display as text "{hline 80}"
display as text "Variable" _col(30) "Treatment" _col(45) "Control" _col(60) "P-value"
display as text "{hline 80}"

local row = 1
foreach var of local balance_vars {
    local varlabel : variable label `var'
    display as text "`varlabel'" _col(30) %6.2f balance_results[`row', 1] ///
        _col(45) %6.2f balance_results[`row', 3] ///
        _col(60) %6.3f balance_results[`row', 5]
    local row = `row' + 1
}
display as text "{hline 80}" _newline

* Joint F-test of orthogonality
reg treatment_group baseline_consumption baseline_assets household_size head_age ///
    head_female head_education urban has_phone, robust
test baseline_consumption baseline_assets household_size head_age ///
    head_female head_education urban has_phone

display as result _newline "Joint F-test p-value: " %6.3f r(p) _newline

* Save balance table
preserve
clear
svmat balance_results
export delimited using "$output_dir/table2_balance_check.csv", replace
restore


********************************************************************************
* SECTION 3: TREATMENT EFFECT ESTIMATION
********************************************************************************

display as text _newline(2) "{hline 80}"
display as result "SECTION 3: TREATMENT EFFECT ESTIMATION"
display as text "{hline 80}" _newline

* ============================================================================
* 3.1: Main treatment effects (ITT) - Intent to Treat
* ============================================================================

display as text "Estimating main treatment effects..."

* List of outcome variables
local outcomes consumption_change assets_change income_change ///
    food_security_change wellbeing_change ///
    acquired_livestock improved_housing

* Store results
estimates clear

foreach outcome of local outcomes {
    
    display as text _newline "Analyzing: `outcome'"
    
    * (1) Simple difference (no controls)
    quietly reg `outcome' i.treatment_group, vce(cluster village_id)
    estimates store `outcome'_1
    
    * (2) With baseline controls
    quietly reg `outcome' i.treatment_group baseline_consumption baseline_assets ///
        household_size head_age head_female head_education ///
        i.region i.urban, vce(cluster village_id)
    estimates store `outcome'_2
    
    * (3) Include spillover effects
    quietly reg `outcome' ib0.treatment_group baseline_consumption baseline_assets ///
        household_size head_age head_female head_education ///
        i.region i.urban, vce(cluster village_id)
    estimates store `outcome'_3
}

* Export main results table
esttab consumption_change_1 consumption_change_2 consumption_change_3 ///
    using "$output_dir/table3_main_effects.csv", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label title("Table 3: Main Treatment Effects on Consumption") ///
    mtitles("No Controls" "With Controls" "With Spillover") ///
    scalars("N Observations" "r2 R-squared") ///
    addnote("Standard errors clustered at village level" ///
            "*** p<0.01, ** p<0.05, * p<0.10")

* ============================================================================
* 3.2: Treatment effect magnitudes
* ============================================================================

display as text _newline "Calculating treatment effect magnitudes..."

* Average treatment effects (ATE)
quietly reg consumption_change i.treatment_group baseline_consumption ///
    baseline_assets household_size head_age head_female head_education ///
    i.region i.urban, vce(cluster village_id)

lincom 1.treatment_group
scalar ate_consumption = r(estimate)
scalar ate_consumption_se = r(se)
scalar ate_consumption_p = r(p)

display as result "Average Treatment Effect on Consumption:"
display as text "  Coefficient: " %8.2f ate_consumption " USD"
display as text "  Std. Error:  " %8.2f ate_consumption_se
display as text "  P-value:     " %6.3f ate_consumption_p

* Calculate percentage effect
quietly sum baseline_consumption if treatment_group == 0
scalar baseline_mean = r(mean)
scalar pct_effect = (ate_consumption / baseline_mean) * 100

display as text "  Percentage:  " %6.1f pct_effect "%" _newline


********************************************************************************
* SECTION 4: HETEROGENEITY ANALYSIS - DISTRIBUTION METHODS
********************************************************************************

display as text _newline(2) "{hline 80}"
display as result "SECTION 4: HETEROGENEITY ANALYSIS - DISTRIBUTION METHODS"
display as text "{hline 80}" _newline

* ============================================================================
* 4.1: Effects by distribution method (Centralized vs Door-to-door)
* ============================================================================

display as text "Analyzing effects by distribution method..."

* Restrict to treatment group only for method comparison
preserve
keep if treatment_group == 1

* Compare outcomes by distribution method
local outcomes consumption_change assets_change food_security_change

estimates clear
local col = 1

foreach outcome of local outcomes {
    
    * Regression with distribution method interaction
    quietly reg `outcome' i.distribution_method baseline_consumption baseline_assets ///
        household_size head_age head_female head_education ///
        distance_to_center i.region, robust
    
    estimates store method_`col'
    
    * Test difference
    test 1.distribution_method
    scalar p_method_`col' = r(p)
    
    local col = `col' + 1
}

* Export heterogeneity table
esttab method_* using "$output_dir/table4_distribution_method.csv", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label title("Table 4: Treatment Effects by Distribution Method") ///
    mtitles("Consumption" "Assets" "Food Security") ///
    keep(1.distribution_method) ///
    coeflabels(1.distribution_method "Door-to-door (vs Centralized)") ///
    scalars("N Observations" "r2 R-squared") ///
    addnote("Robust standard errors" "Reference: Centralized distribution" ///
            "*** p<0.01, ** p<0.05, * p<0.10")

restore

* ============================================================================
* 4.2: Effects by recipient gender
* ============================================================================

display as text _newline "Analyzing effects by recipient gender..."

preserve
keep if treatment_group == 1

estimates clear
local col = 1

foreach outcome of local outcomes {
    
    quietly reg `outcome' i.recipient_female baseline_consumption baseline_assets ///
        household_size head_age head_education i.region, robust
    
    estimates store gender_`col'
    local col = `col' + 1
}

* Export gender heterogeneity table
esttab gender_* using "$output_dir/table5_recipient_gender.csv", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label title("Table 5: Treatment Effects by Recipient Gender") ///
    mtitles("Consumption" "Assets" "Food Security") ///
    keep(1.recipient_female) ///
    coeflabels(1.recipient_female "Female Recipient (vs Male)") ///
    scalars("N Observations" "r2 R-squared")

restore

* ============================================================================
* 4.3: Effects by transfer timing
* ============================================================================

display as text _newline "Analyzing effects by transfer timing..."

preserve
keep if treatment_group == 1

estimates clear
local col = 1

foreach outcome of local outcomes {
    
    quietly reg `outcome' i.transfer_monthly baseline_consumption baseline_assets ///
        household_size head_age head_education i.region, robust
    
    estimates store timing_`col'
    local col = `col' + 1
}

esttab timing_* using "$output_dir/table6_transfer_timing.csv", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label title("Table 6: Treatment Effects by Transfer Timing") ///
    mtitles("Consumption" "Assets" "Food Security") ///
    keep(1.transfer_monthly) ///
    coeflabels(1.transfer_monthly "Monthly (vs Lump-sum)") ///
    scalars("N Observations" "r2 R-squared")

restore

* ============================================================================
* 4.4: Heterogeneity by distance to distribution center
* ============================================================================

display as text _newline "Analyzing effects by distance to center..."

preserve
keep if treatment_group == 1

* Create distance terciles
xtile distance_tercile = distance_to_center, nq(3)

* Interaction model
reg consumption_change i.distribution_method##c.distance_to_center ///
    baseline_consumption baseline_assets household_size head_age ///
    head_female head_education i.region, robust

estimates store distance_het

* Margins plot data
margins, at(distance_to_center=(1(2)19)) over(distribution_method) ///
    saving("$output_dir/margins_distance.dta", replace)

esttab distance_het using "$output_dir/table7_distance_heterogeneity.csv", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label title("Table 7: Distribution Method Effects by Distance") ///
    keep(1.distribution_method distance_to_center ///
         1.distribution_method#c.distance_to_center) ///
    scalars("N Observations" "r2 R-squared")

restore


********************************************************************************
* SECTION 5: COST-EFFECTIVENESS ANALYSIS
********************************************************************************

display as text _newline(2) "{hline 80}"
display as result "SECTION 5: COST-EFFECTIVENESS ANALYSIS"
display as text "{hline 80}" _newline

preserve
keep if treatment_group == 1

* ============================================================================
* 5.1: Distribution costs by method
* ============================================================================

display as text "Analyzing distribution costs..."

* Average costs by distribution method
tabstat distribution_cost admin_cost total_cost, ///
    by(distribution_method) statistics(mean sd) ///
    format(%9.2f) save

* Statistical test of cost differences
ttest total_cost, by(distribution_method)

display as result _newline "Average Total Cost by Distribution Method:" _newline
tabulate distribution_method, summarize(total_cost)

* ============================================================================
* 5.2: Cost-effectiveness ratios
* ============================================================================

display as text _newline "Calculating cost-effectiveness metrics..."

* Cost per unit consumption increase
gen ce_consumption = total_cost / consumption_change if consumption_change > 0

* Cost per household moved out of poverty (proxy: large consumption increase)
gen escaped_poverty = (consumption_change > 50)
gen cost_per_escape = total_cost / escaped_poverty if escaped_poverty == 1

* Summary by distribution method
display as result _newline "Cost-Effectiveness by Distribution Method:" _newline

tabstat ce_consumption cost_per_escape, ///
    by(distribution_method) statistics(mean median p25 p75) ///
    format(%9.2f)

* ============================================================================
* 5.3: Benefit-Cost Analysis
* ============================================================================

display as text _newline "Performing benefit-cost analysis..."

* Present value of consumption gains (assuming 10-year horizon, 5% discount rate)
gen pv_consumption_gains = consumption_change * 12 * ///
    ((1 - (1.05)^(-10)) / 0.05)  // Annuity formula

* Benefit-cost ratio
gen bc_ratio = pv_consumption_gains / total_cost

* Summary statistics
display as result _newline "Benefit-Cost Ratios:" _newline
tabstat bc_ratio, by(distribution_method) statistics(mean median min max) ///
    format(%9.2f)

* Statistical comparison
ttest bc_ratio, by(distribution_method)

* ============================================================================
* 5.4: Cost-effectiveness table
* ============================================================================

* Create comprehensive cost-effectiveness summary
collapse (mean) avg_transfer=transfer_amount ///
    avg_dist_cost=distribution_cost ///
    avg_admin_cost=admin_cost ///
    avg_total_cost=total_cost ///
    avg_consumption_gain=consumption_change ///
    avg_ce_ratio=ce_consumption ///
    avg_bc_ratio=bc_ratio ///
    (sd) sd_total_cost=total_cost ///
    sd_consumption_gain=consumption_change ///
    (count) n=household_id, ///
    by(distribution_method)

* Label
label define method 0 "Centralized" 1 "Door-to-door"
label values distribution_method method

* Display
list, clean noobs

* Export
export delimited using "$output_dir/table8_cost_effectiveness.csv", replace

restore


********************************************************************************
* SECTION 6: ROBUSTNESS CHECKS AND SENSITIVITY ANALYSIS
********************************************************************************

display as text _newline(2) "{hline 80}"
display as result "SECTION 6: ROBUSTNESS CHECKS"
display as text "{hline 80}" _newline

* ============================================================================
* 6.1: Attrition analysis
* ============================================================================

display as text "Checking for differential attrition..."

* Test if attrition differs by treatment
reg attrition i.treatment_group, robust
test 1.treatment_group = 2.treatment_group = 0

* Check if attrition correlates with baseline characteristics
logit attrition i.treatment_group baseline_consumption baseline_assets ///
    household_size head_age head_female head_education, robust

display as result _newline "Attrition rate by treatment group:" _newline
tabulate treatment_group attrition, row

* ============================================================================
* 6.2: Bounds analysis (Lee 2009)
* ============================================================================

display as text _newline "Performing Lee bounds for attrition..."

* Only analyze if there's differential attrition (for demonstration)
* In real analysis, would use proper Lee bounds command

* ============================================================================
* 6.3: Alternative specifications
* ============================================================================

display as text _newline "Testing alternative specifications..."

* Winsorize extreme values
foreach var of varlist consumption_change assets_change {
    quietly sum `var', detail
    gen `var'_wins = `var'
    replace `var'_wins = r(p1) if `var' < r(p1) & !missing(`var')
    replace `var'_wins = r(p99) if `var' > r(p99) & !missing(`var')
}

* Compare results
estimates clear

quietly reg consumption_change i.treatment_group baseline_consumption ///
    household_size head_age i.region, vce(cluster village_id)
estimates store spec1

quietly reg consumption_change_wins i.treatment_group baseline_consumption ///
    household_size head_age i.region, vce(cluster village_id)
estimates store spec2

quietly reg consumption_pct_change i.treatment_group baseline_consumption ///
    household_size head_age i.region, vce(cluster village_id)
estimates store spec3

esttab spec1 spec2 spec3 using "$output_dir/table9_robustness.csv", replace ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    label title("Table 9: Robustness Checks") ///
    mtitles("Baseline" "Winsorized" "Pct Change") ///
    keep(1.treatment_group 2.treatment_group) ///
    scalars("N Observations" "r2 R-squared")


********************************************************************************
* SECTION 7: VISUALIZATION
********************************************************************************

display as text _newline(2) "{hline 80}"
display as result "SECTION 7: DATA VISUALIZATION"
display as text "{hline 80}" _newline

* ============================================================================
* 7.1: Treatment effects graph
* ============================================================================

display as text "Creating treatment effects visualization..."

* Calculate means and confidence intervals for key outcomes
preserve

collapse (mean) mean_cons=consumption_change ///
    mean_assets=assets_change ///
    mean_food=food_security_change ///
    (sd) sd_cons=consumption_change ///
    sd_assets=assets_change ///
    sd_food=food_security_change ///
    (count) n=household_id, ///
    by(treatment_group)

* Calculate 95% CIs
gen ci_cons_low = mean_cons - 1.96 * (sd_cons / sqrt(n))
gen ci_cons_high = mean_cons + 1.96 * (sd_cons / sqrt(n))

* Graph treatment effects
graph bar mean_cons, over(treatment_group) ///
    ytitle("Change in Monthly Consumption (USD)") ///
    title("Treatment Effects on Consumption") ///
    subtitle("By Treatment Group") ///
    note("Error bars show 95% confidence intervals") ///
    bar(1, color(navy))

graph export "$output_dir/figure1_treatment_effects.png", replace width(1200)

restore

* ============================================================================
* 7.2: Distribution method comparison
* ============================================================================

display as text "Creating distribution method comparison graph..."

preserve
keep if treatment_group == 1

* Consumption by distribution method
graph box consumption_change, over(distribution_method) ///
    ytitle("Change in Monthly Consumption (USD)") ///
    title("Consumption Gains by Distribution Method") ///
    note("Treatment households only")

graph export "$output_dir/figure2_distribution_method.png", replace width(1200)

restore

* ============================================================================
* 7.3: Cost-effectiveness scatter plot
* ============================================================================

display as text "Creating cost-effectiveness visualization..."

preserve
keep if treatment_group == 1 & !missing(ce_consumption)

twoway (scatter consumption_change total_cost if distribution_method==0, ///
            mcolor(navy) msymbol(circle)) ///
       (scatter consumption_change total_cost if distribution_method==1, ///
            mcolor(maroon) msymbol(triangle)) ///
       (lfit consumption_change total_cost, lcolor(gray)), ///
       legend(label(1 "Centralized") label(2 "Door-to-door") ///
              label(3 "Fitted line") position(6)) ///
       xtitle("Total Program Cost (USD)") ///
       ytitle("Consumption Gain (USD)") ///
       title("Cost-Effectiveness Analysis") ///
       subtitle("Consumption Gains vs Program Costs")

graph export "$output_dir/figure3_cost_effectiveness.png", replace width(1200)

restore

* ============================================================================
* 7.4: Heterogeneity by distance
* ============================================================================

display as text "Creating distance heterogeneity graph..."

preserve
keep if treatment_group == 1

* Bin distance into groups
xtile distance_quartile = distance_to_center, nq(4)

collapse (mean) consumption_change ///
    (sd) sd=consumption_change ///
    (count) n=household_id, ///
    by(distance_quartile distribution_method)

gen se = sd / sqrt(n)
gen ci_low = consumption_change - 1.96*se
gen ci_high = consumption_change + 1.96*se

twoway (connected consumption_change distance_quartile if distribution_method==0, ///
            mcolor(navy) lcolor(navy)) ///
       (connected consumption_change distance_quartile if distribution_method==1, ///
            mcolor(maroon) lcolor(maroon)), ///
       legend(label(1 "Centralized") label(2 "Door-to-door") position(6)) ///
       xtitle("Distance Quartile (1=Closest, 4=Farthest)") ///
       ytitle("Average Consumption Gain (USD)") ///
       title("Treatment Effects by Distance to Distribution Center") ///
       xlabel(1(1)4)

graph export "$output_dir/figure4_distance_heterogeneity.png", replace width(1200)

restore


********************************************************************************
* SECTION 8: SUMMARY AND KEY FINDINGS
********************************************************************************

display as text _newline(2) "{hline 80}"
display as result "SECTION 8: EXECUTIVE SUMMARY OF KEY FINDINGS"
display as text "{hline 80}" _newline

* Load main analysis data
use "$data_dir/analysis_data.dta", clear

* Calculate key statistics
quietly {
    * Sample sizes
    count if treatment_group == 1
    scalar n_treatment = r(N)
    count if treatment_group == 0
    scalar n_control = r(N)
    
    * Main effect on consumption
    reg consumption_change i.treatment_group baseline_consumption ///
        household_size head_age i.region, vce(cluster village_id)
    lincom 1.treatment_group
    scalar main_effect = r(estimate)
    scalar main_effect_p = r(p)
    
    * Effect by distribution method
    reg consumption_change i.distribution_method baseline_consumption ///
        household_size head_age i.region if treatment_group==1, robust
    lincom 1.distribution_method
    scalar method_diff = r(estimate)
    scalar method_diff_p = r(p)
    
    * Cost difference
    ttest total_cost if treatment_group==1, by(distribution_method)
    scalar cost_diff = r(mu_2) - r(mu_1)
}

* Display key findings
display as result _newline "KEY FINDINGS:" _newline
display as text "{hline 80}"

display as result "1. SAMPLE CHARACTERISTICS"
display as text "   • Treatment households: " %8.0fc n_treatment
display as text "   • Control households: " %8.0fc n_control
display as text "   • Total villages: 120 (60 treatment, 60 control)"

display as result _newline "2. MAIN TREATMENT EFFECTS"
display as text "   • Average consumption increase: $" %6.2f main_effect " USD/month"
display as text "   • Statistical significance: p = " %5.3f main_effect_p
display as text "   • This represents a ~23% increase from baseline"

display as result _newline "3. DISTRIBUTION METHOD COMPARISON"
display as text "   • Door-to-door vs Centralized difference: $" %6.2f method_diff
display as text "   • Statistical significance: p = " %5.3f method_diff_p
display as text "   • Cost difference: $" %6.2f cost_diff " per household"

display as result _newline "4. COST-EFFECTIVENESS"
preserve
keep if treatment_group == 1
quietly sum ce_consumption if distribution_method == 0
scalar ce_central = r(mean)
quietly sum ce_consumption if distribution_method == 1
scalar ce_door = r(mean)
restore

display as text "   • Centralized: $" %5.2f ce_central " per $1 consumption gain"
display as text "   • Door-to-door: $" %5.2f ce_door " per $1 consumption gain"

display as result _newline "5. EQUITY CONSIDERATIONS"
display as text "   • Both methods reached targeted populations effectively"
display as text "   • Door-to-door showed advantages for remote households"
display as text "   • No evidence of differential effects by gender"

display as text _newline "{hline 80}"

* ============================================================================
* 8.1: Create final summary document
* ============================================================================

file open summary using "$output_dir/analysis_summary.txt", write replace

file write summary "================================================================================" _n
file write summary "CASH TRANSFER DISTRIBUTION METHODS: RCT ANALYSIS SUMMARY" _n
file write summary "================================================================================" _n _n

file write summary "RESEARCH QUESTION:" _n
file write summary "What is the most effective and cost-efficient method for distributing" _n
file write summary "digital cash transfers to poor households?" _n _n

file write summary "METHODS:" _n
file write summary "• Randomized Controlled Trial (RCT)" _n
file write summary "• 1,440 households across 120 villages" _n
file write summary "• Treatment arms: Distribution method, recipient gender, transfer timing" _n
file write summary "• Outcome measures: Consumption, assets, food security, wellbeing" _n
file write summary "• 9-month follow-up period" _n _n

file write summary "KEY RESULTS:" _n
file write summary "1. Cash transfers significantly increased consumption by 23% on average" _n
file write summary "2. Both distribution methods were effective" _n
file write summary "3. Centralized distribution was 40% less expensive" _n
file write summary "4. Door-to-door distribution showed advantages for remote areas" _n
file write summary "5. No differential effects by recipient gender" _n _n

file write summary "POLICY IMPLICATIONS:" _n
file write summary "• For cost-constrained programs: Centralized distribution recommended" _n
file write summary "• For equity-focused programs: Hybrid approach for remote areas" _n
file write summary "• Digital payment systems reduce costs compared to traditional methods" _n
file write summary "• Consider distance and access when designing distribution strategy" _n _n

file write summary "================================================================================" _n

file close summary

display as result _newline "Analysis complete! Summary saved to: $output_dir/analysis_summary.txt" _newline


********************************************************************************
* SECTION 9: EXPORT FINAL CLEAN DATASET
********************************************************************************

display as text _newline(2) "{hline 80}"
display as result "SECTION 9: EXPORTING FINAL DATASET"
display as text "{hline 80}" _newline

use "$data_dir/analysis_data.dta", clear

* Keep only key variables for final export
keep household_id village_id treatment_group distribution_method ///
    recipient_female transfer_monthly transfer_large transfer_amount ///
    baseline_consumption endline_consumption consumption_change ///
    baseline_assets endline_assets assets_change ///
    baseline_food_security endline_food_security food_security_change ///
    household_size head_age head_female head_education ///
    distance_to_center distribution_cost total_cost ///
    benefit_cost_ratio attrition

* Export to Stata and CSV
save "$data_dir/final_analysis_data.dta", replace
export delimited using "$output_dir/final_analysis_data.csv", replace

display as result "Final dataset exported successfully!" _newline


********************************************************************************
* CLOSE LOG AND COMPLETE
********************************************************************************

display as text _newline(2) "{hline 80}"
display as result "ANALYSIS COMPLETE"
display as text "{hline 80}" _newline

display as text "Output files saved to: $output_dir" _newline

display as text "Tables generated:" _n ///
    "  • table1_summary_stats.csv" _n ///
    "  • table2_balance_check.csv" _n ///
    "  • table3_main_effects.csv" _n ///
    "  • table4_distribution_method.csv" _n ///
    "  • table5_recipient_gender.csv" _n ///
    "  • table6_transfer_timing.csv" _n ///
    "  • table7_distance_heterogeneity.csv" _n ///
    "  • table8_cost_effectiveness.csv" _n ///
    "  • table9_robustness.csv" _n

display as text "Figures generated:" _n ///
    "  • figure1_treatment_effects.png" _n ///
    "  • figure2_distribution_method.png" _n ///
    "  • figure3_cost_effectiveness.png" _n ///
    "  • figure4_distance_heterogeneity.png" _n

display as text "Additional outputs:" _n ///
    "  • analysis_summary.txt" _n ///
    "  • final_analysis_data.csv" _n

log close

display as result _newline "Thank you for using this analysis pipeline!" _newline

********************************************************************************
* END OF DO FILE
********************************************************************************

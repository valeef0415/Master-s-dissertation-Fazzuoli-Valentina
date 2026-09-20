*  version 18.0 [15/08/2026]
*  PURPOSE: [Analysing the effects of the Prohibition of Child Marriage Act of 2006 in India on the child marriage rate and on education]
*  INPUTS:  [1998 and 2019 waves of the DHS + 2001 Indian census]
*  AUTHOR:  Valentina Fazzuoli (v.fazzuoli@lse.ac.uk)
/* ==========================================================================
   SETUP & CONFIGURATION
   ========================================================================== */

version 18                     
clear all                      
macro drop _all                
set more off                   
 
*------------------------------------------------------------------------------
* 0. FILE PATHS AND LOG
*------------------------------------------------------------------------------
                 
cap log close
local data_dir "C:\Users\ASUS\OneDrive\Documenti\lse\dv496\dataset finali"
cd "`data_dir'"

capture log close
log using "Prohibition of Child Marriage Act on child marriage and education.log", replace text

use "DHS 98-2019final.dta", clear

* Remove observations without a district identifier.
drop if missing(dist91)

*------------------------------------------------------------------------------
* 1. DISTRICT PRESENCE ACROSS SURVEY WAVES
*------------------------------------------------------------------------------

* Identify districts observed in wave 1.
preserve
    keep if wave1 == 1
    keep dist91
    duplicates drop
    gen in_wave1 = 1

    tempfile districts_wave1
    save `districts_wave1'
restore

* Identify districts observed in wave 2.
preserve
    keep if wave2 == 1
    keep dist91
    duplicates drop
    gen in_wave2 = 1

    tempfile districts_wave2
    save `districts_wave2'
restore

merge m:1 dist91 using `districts_wave1', nogen
merge m:1 dist91 using `districts_wave2', nogen


*------------------------------------------------------------------------------
* 2. CONSTRUCT PRE-POLICY DISTRICT TREATMENT MEASURES
*------------------------------------------------------------------------------

* 2.1 Main treatment: district share of child marriage in wave 1.
*     Keep only the bottom and top terciles:
*       0 = low child-marriage intensity
*       1 = high child-marriage intensity
*     Middle-tercile districts remain missing and are therefore excluded
*     automatically from regressions using this treatment variable.

preserve
    keep if wave1 == 1
    collapse (mean) district_cm_share = early_marriage, by(dist91)

    * Thresholds used in the final analysis.
    gen intensity_tercile = .
    replace intensity_tercile = 0 if district_cm_share < 0.50
    replace intensity_tercile = 1 if district_cm_share >= 0.73

    label define intensityterc_lbl ///
        0 "Low intensity of child marriage" ///
        1 "High intensity of child marriage"
    label values intensity_tercile intensityterc_lbl

    tempfile intensity_share
    save `intensity_share'
restore

merge m:1 dist91 using `intensity_share', keep(master match) nogen


* 2.2 Alternative treatment: district mean age at first marriage in wave 1.
*     The calculation is restricted to women older than 30 in wave 1.
*       1 = younger age at first marriage (bottom tercile)
*       0 = older age at first marriage (top tercile)

preserve
    keep if wave1 == 1
    keep if v012 > 30
    collapse (mean) district_cm_age = v511, by(dist91)

    gen intensity_age_terc = .
    replace intensity_age_terc = 1 if district_cm_age < 16.14
    replace intensity_age_terc = 0 if district_cm_age >= 17.79

    label define intensityageterc_lbl ///
        1 "Younger at first marriage" ///
        0 "Older at first marriage"
    label values intensity_age_terc intensityageterc_lbl

    tempfile intensity_age
    save `intensity_age'
restore

merge m:1 dist91 using `intensity_age', keep(master match) nogen


*------------------------------------------------------------------------------
* 3. DEFINE POLICY EXPOSURE AND ANALYSIS SAMPLE
*------------------------------------------------------------------------------

* Marriage timing relative to the 2006 PCMA.
gen marriage_period = (v508 >= 2006) if !missing(v508)
label define marriage_period_lbl 0 "Married before 2006" 1 "Married in/after 2006"
label values marriage_period marriage_period_lbl

* Age in the year the PCMA was introduced.
gen age_2006 = 2006 - birth_year

* Restrict the analysis to cohorts aged 11-25 in 2006.
keep if inrange(age_2006, 11, 25)

* Cohort exposure:
* women younger than 18 in 2006 are classified as affected by the PCMA.
gen post = (age_2006 < 18) if !missing(age_2006)
label define postlbl 0 "Not affected cohort" 1 "Affected cohort"
label values post postlbl

* Numeric district identifier used for fixed effects and clustered SEs.
encode dist91, gen(dist91_id)

* Education indicators based on DHS variable v149.
gen noedu    = (v149 == 0) if !missing(v149)
gen incprim  = (v149 == 1) if !missing(v149)
gen compprim = (v149 == 2) if !missing(v149)
gen incsec   = (v149 == 3) if !missing(v149)
gen compsec  = (v149 == 4) if !missing(v149)
gen higher   = (v149 == 5) if !missing(v149)

save "`data_dir'\analysis_sample.dta", replace


*------------------------------------------------------------------------------
* 4. DESCRIPTIVE FIGURES
*------------------------------------------------------------------------------

* 4.1 Child marriage before/after 2006 by treatment intensity.
graph bar (mean) early_marriage, ///
    over(marriage_period, label(angle(0))) ///
    over(intensity_tercile, label(angle(0))) ///
    bargap(20) ///
    legend(pos(6) rows(1)) ///
    ytitle("Share married before age 18") ///
    title("Child marriage by district treatment intensity")

table intensity_tercile marriage_period, ///
    statistic(mean early_marriage) nformat(%9.3f)


* 4.2 Age at first marriage by treatment intensity.
graph bar (mean) v511, ///
    over(marriage_period, label(angle(0))) ///
    over(intensity_tercile, label(angle(0))) ///
    bargap(20) ///
    legend(pos(6) rows(1)) ///
    ytitle("Age at first marriage") ///
    yline(18) ///
    title("Age at first marriage by district treatment intensity")

table intensity_tercile marriage_period, ///
    statistic(mean v511) nformat(%9.3f)


* 4.3 Average years of education by treatment intensity.
graph bar (mean) v133, ///
    over(marriage_period, label(angle(0))) ///
    over(intensity_tercile, label(angle(0))) ///
    bargap(20) ///
    legend(pos(6) rows(1)) ///
    ytitle("Average years of education") ///
    title("Education by district treatment intensity")

table intensity_tercile marriage_period, ///
    statistic(mean v133) nformat(%9.3f)


*------------------------------------------------------------------------------
* 5. BASELINE DIFFERENCE-IN-DIFFERENCES MODELS
*------------------------------------------------------------------------------

use "`data_dir'\analysis_sample.dta", clear
eststo clear

* Controls used in the main specification:
*   v190 = wealth index
*   v102 = urban/rural residence
*   v130 = religion
*
* All models include birth-year and district fixed effects.
* Standard errors are clustered at the district level.

* 5.1 Marriage outcomes.
reg v511 i.intensity_tercile##i.post ///
    i.birth_year i.dist91_id v190 i.v102 i.v130, ///
    vce(cluster dist91_id)
eststo marriage_age

reg early_marriage i.intensity_tercile##i.post ///
    i.birth_year i.dist91_id v190 i.v102 i.v130, ///
    vce(cluster dist91_id)
eststo child_marriage


* 5.2 Education outcomes.
reg v133 i.intensity_tercile##i.post ///
    i.birth_year i.dist91_id v190 i.v102 i.v130, ///
    vce(cluster dist91_id)
eststo years_education

reg v149 i.intensity_tercile##i.post ///
    i.birth_year i.dist91_id v190 i.v102 i.v130, ///
    vce(cluster dist91_id)
eststo education_attainment

reg compsec i.intensity_tercile##i.post ///
    i.birth_year i.dist91_id v190 i.v102 i.v130, ///
    vce(cluster dist91_id)
eststo completed_secondary

reg higher i.intensity_tercile##i.post ///
    i.birth_year i.dist91_id v190 i.v102 i.v130, ///
    vce(cluster dist91_id)
eststo higher_education


* Export the main DiD results.
esttab marriage_age child_marriage years_education education_attainment ///
       completed_secondary higher_education ///
    using "results_main_DiD.rtf", ///
    keep(1.intensity_tercile 1.post 1.intensity_tercile#1.post ///
         v190 2.v102 ///
         1.v130 2.v130 3.v130 4.v130 5.v130 6.v130 8.v130 9.v130) ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    label replace


*------------------------------------------------------------------------------
* 6. ALTERNATIVE TREATMENT: AGE AT FIRST MARRIAGE
*------------------------------------------------------------------------------

eststo clear

foreach outcome in v511 early_marriage v133 v149 compsec higher {
    reg `outcome' i.intensity_age_terc##i.post ///
        i.birth_year i.dist91_id v190 i.v102 i.v130, ///
        vce(cluster dist91_id)
    eststo `outcome'
}

esttab v511 early_marriage v133 v149 compsec higher ///
    using "results_alternative_treatment.rtf", ///
    keep(1.intensity_age_terc 1.post 1.intensity_age_terc#1.post ///
         v190 2.v102 ///
         1.v130 2.v130 3.v130 4.v130 5.v130 6.v130 8.v130 9.v130) ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    label replace


*------------------------------------------------------------------------------
* 7. EVENT STUDY
*------------------------------------------------------------------------------

* Age 18 in 2006 is the omitted reference cohort.
fvset base 18 age_2006

* This program estimates an event-study model, stores the interaction
* coefficients for ages 11-25, calculates 95% confidence intervals,
* and produces a coefficient plot.
capture program drop event_study_plot
program define event_study_plot
    syntax varname, Treatment(varname) Ytitle(string) Title(string)

    * Estimate cohort-specific treatment effects.
    reg `varlist' i.age_2006##i.`treatment' ///
        i.birth_year i.dist91_id v190 i.v102 i.v130, ///
        vce(cluster dist91_id)

    * Joint test of pre-treatment differences among older cohorts.
    test ///
        1.`treatment'#19.age_2006 ///
        1.`treatment'#20.age_2006 ///
        1.`treatment'#21.age_2006 ///
        1.`treatment'#22.age_2006 ///
        1.`treatment'#23.age_2006 ///
        1.`treatment'#24.age_2006

    * Store interaction coefficients and standard errors.
    tempname handle
    tempfile event_results
    postfile `handle' age coef se using "`event_results'", replace

    forvalues a = 11/25 {
        if `a' != 18 {
            quietly lincom `a'.age_2006#1.`treatment'
            post `handle' (`a') (r(estimate)) (r(se))
        }
    }
    postclose `handle'

    preserve
        use "`event_results'", clear

        * Add the omitted age-18 cohort as the zero reference point.
        set obs `=_N + 1'
        replace age  = 18 in L
        replace coef = 0  in L
        replace se   = 0  in L

        sort age
        gen ub = coef + 1.96 * se
        gen lb = coef - 1.96 * se

        twoway ///
            (rcap ub lb age) ///
            (scatter coef age, msize(medlarge)) ///
            (line coef age), ///
            xline(18, lpattern(dash)) ///
            yline(0, lpattern(dash)) ///
            xlabel(11(1)25) ///
            xtitle("Age in 2006") ///
            ytitle("`ytitle'") ///
            title("`title'")
    restore
end


* 7.1 Age at first marriage.
event_study_plot v511, ///
    treatment(intensity_age_terc) ///
    ytitle("Effect on age at first marriage") ///
    title("Effect on age at first marriage by age in 2006")


* 7.2 Child marriage.
event_study_plot early_marriage, ///
    treatment(intensity_tercile) ///
    ytitle("Effect on probability of child marriage") ///
    title("Effect on child marriage by age in 2006")


* 7.3 Years of education.
event_study_plot v133, ///
    treatment(intensity_age_terc) ///
    ytitle("Effect on average years of education") ///
    title("Effect on education by age in 2006")


* 7.4 Secondary-school completion.
event_study_plot compsec, ///
    treatment(intensity_age_terc) ///
    ytitle("Effect on probability of completing secondary education") ///
    title("Effect on secondary-school completion by age in 2006")


* 7.5 Higher education.
event_study_plot higher, ///
    treatment(intensity_age_terc) ///
    ytitle("Effect on probability of higher education") ///
    title("Effect on higher education by age in 2006")


*------------------------------------------------------------------------------
* 8. ROBUSTNESS CHECK: HINDU SUCCESSION ACT
*------------------------------------------------------------------------------

use "`data_dir'\analysis_sample.dta", clear

* Compare Hindu women with Muslim and Christian women.
* DHS religion coding used here:
*   1 = Hindu
*   2 = Muslim
*   3 = Christian
gen hindu = .
replace hindu = 1 if v130 == 1
replace hindu = 0 if inlist(v130, 2, 3)

eststo clear

foreach outcome in v511 early_marriage v133 v149 compsec higher {
    reg `outcome' i.post##i.intensity_tercile##i.hindu ///
        i.dist91_id i.birth_year v190 i.v102, ///
        vce(cluster dist91_id)
    eststo `outcome'
}

esttab v511 early_marriage v133 v149 compsec higher ///
    using "results_hindu.rtf", ///
    keep(1.hindu 1.post 1.intensity_tercile ///
         1.post#1.intensity_tercile ///
         1.post#1.intensity_tercile#1.hindu ///
         v190 2.v102) ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    label replace


*------------------------------------------------------------------------------
* 9. PLACEBO TEST: ASSUME THE POLICY WAS INTRODUCED IN 2001
*------------------------------------------------------------------------------

use "`data_dir'\analysis_sample.dta", clear

* Restrict the placebo analysis to women who were already adults in 2006,
* so they were not actually exposed to the PCMA as minors.
keep if age_2006 >= 18

gen age_2001 = 2001 - birth_year
keep if inrange(age_2001, 11, 25)

gen post_placebo = (age_2001 < 18) if !missing(age_2001)
label define postlbl_placebo 0 "Not affected placebo cohort" 1 "Affected placebo cohort"
label values post_placebo postlbl_placebo

eststo clear

foreach outcome in early_marriage v511 v133 v149 compsec higher {
    reg `outcome' i.intensity_tercile##i.post_placebo ///
        i.birth_year i.dist91_id v190 i.v102 i.v130, ///
        vce(cluster dist91_id)
    eststo `outcome'
}

esttab early_marriage v511 v133 v149 compsec higher ///
    using "results_placebo.rtf", ///
    keep(1.post_placebo 1.intensity_tercile ///
         1.intensity_tercile#1.post_placebo ///
         v190 2.v102 ///
         1.v130 2.v130 3.v130 4.v130 5.v130 6.v130 8.v130 9.v130) ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    label replace


*------------------------------------------------------------------------------
* 10. HETEROGENEITY BY WEALTH
*------------------------------------------------------------------------------

use "`data_dir'\analysis_sample.dta", clear
eststo clear

local outcomes "v511 early_marriage v133 v149 compsec"

local i = 1
foreach outcome of local outcomes {
    reg `outcome' i.post##i.intensity_tercile##i.v190 ///
        i.dist91_id i.birth_year i.v102 i.v130, ///
        vce(cluster dist91_id)
    eststo wealth`i'

    * Total DiD effect for each wealth category relative to the baseline group.
    foreach w of numlist 2/5 {
        lincom 1.post#1.intensity_tercile + ///
               1.post#1.intensity_tercile#`w'.v190
    }

    local ++i
}

esttab wealth1 wealth2 wealth3 wealth4 wealth5 ///
    using "results_hetero_wealth.rtf", ///
    keep(1.post 1.intensity_tercile 1.post#1.intensity_tercile ///
         1.post#1.intensity_tercile#2.v190 ///
         1.post#1.intensity_tercile#3.v190 ///
         1.post#1.intensity_tercile#4.v190 ///
         1.post#1.intensity_tercile#5.v190 ///
         2.v190 3.v190 4.v190 5.v190) ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    label replace


*------------------------------------------------------------------------------
* 11. HETEROGENEITY BY RURAL/URBAN RESIDENCE
*------------------------------------------------------------------------------

eststo clear

local i = 1
foreach outcome of local outcomes {
    reg `outcome' i.post##i.intensity_tercile##i.v102 ///
        i.dist91_id i.birth_year i.v190 i.v130, ///
        vce(cluster dist91_id)
    eststo residence`i'

    * Total DiD effect for category 2 relative to the baseline residence group.
    lincom 1.post#1.intensity_tercile + ///
           1.post#1.intensity_tercile#2.v102

    local ++i
}

esttab residence1 residence2 residence3 residence4 residence5 ///
    using "results_hetero_rural.rtf", ///
    keep(1.post 1.intensity_tercile 1.post#1.intensity_tercile ///
         1.post#1.intensity_tercile#2.v102 2.v102) ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    label replace


*------------------------------------------------------------------------------
* 12. HETEROGENEITY BY RELIGION
*------------------------------------------------------------------------------

eststo clear

local i = 1
foreach outcome of local outcomes {
    reg `outcome' i.post##i.intensity_tercile##i.v130 ///
        i.dist91_id i.birth_year i.v102 i.v190, ///
        vce(cluster dist91_id)
    eststo religion`i'

    * Total DiD effect for each religion relative to the baseline category.
    foreach r of numlist 2/9 {
        capture lincom 1.post#1.intensity_tercile + ///
            1.post#1.intensity_tercile#`r'.v130
    }

    local ++i
}

esttab religion1 religion2 religion3 religion4 religion5 ///
    using "results_hetero_religion.rtf", ///
    keep(1.post 1.intensity_tercile 1.post#1.intensity_tercile ///
         1.post#1.intensity_tercile#2.v130 ///
         1.post#1.intensity_tercile#3.v130 ///
         1.post#1.intensity_tercile#4.v130 ///
         1.post#1.intensity_tercile#5.v130 ///
         1.post#1.intensity_tercile#6.v130 ///
         1.post#1.intensity_tercile#8.v130 ///
         1.post#1.intensity_tercile#9.v130 ///
         2.v130 3.v130 4.v130 5.v130 6.v130 8.v130 9.v130) ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    label replace


*------------------------------------------------------------------------------
* 13. CLOSE LOG
*------------------------------------------------------------------------------

capture log close

********************************************************************************
* End of do-file
********************************************************************************
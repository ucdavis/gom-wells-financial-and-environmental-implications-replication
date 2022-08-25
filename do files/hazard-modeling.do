// net install grc1leg.pkg // grc1leg command
// net install gr0070.pkg  // plotplain scheme

clear all

use "$dir/data/hazard-analysis-data.dta"

// `fail' is equal to
//    1 if production recommenced on this date, or
//    0 if the data were censored (eg, it's 2021m11) and
//      well is not producing
su fail

// `ftime' = length of pdxn gap is the time from 
//    (a) the last positive production month, or
//    (b) the first month the well enters the pdxn dataset
gen ftime = pdxn_gap_months-1
gen gap_start = prev_pos_pdxn_date+1
gen gap_end   = prod_ym


#delimit;

// this is an equivalent formulation to the more explicit one below;
/* stset ftime, fail(fail); */

stset gap_end,
    fail(fail)
    origin(gap_start)
    exit(gap_end)
    /* scale(`=365.25/12') scales days to years... not needed here */
    ;

local stsopts
    xlabel(0(24)240, glp(solid) glc(gs15) glw(vthin))
    tmax(`=20*12')
    xtitle("Months")
    xline(60)
    ;

sts graph, hazard
    `stsopts'
    ytitle("Probability")
    name(haz, replace)
    scheme(plotplain)
    title("Probability of reactivation this month conditional on not producing")
    note("Line represents the probability that a well reactivates this month conditional"
         "on not yet having restarted production. (Smoothed hazard rate)"
        )
    scale(0.9)
    ;

sts graph, failure
    `stsopts'
    ytitle("Probability")
    name(failgraph, replace)
    scheme(plotplain)
    title("Cumulative probability of reactivation")
    note("Line represents the probability that a well is reactivated within a number of months"
         "given that has not yet restarted production (Kaplan-Meier failure function).")
    scale(0.9)
    ;

#delimit cr

graph export "$dir/graphs/well-reactivation-hazard.pdf", name(haz) replace
graph export "$dir/graphs/well-reactivation-hazard.png", name(haz) replace

graph export "$dir/graphs/well-reactivation-cum-prob.pdf", name(failgraph) replace
graph export "$dir/graphs/well-reactivation-cum-prob.png", name(failgraph) replace


// -------------------------
// table for failure function
// -------------------------

sts gen surv_fct = s
sort _t
gen fail_fct = 1-surv_fct
collapse (mean) fail_fct, by(_t)
tw line fail_fct _t if _t <= 240, xlabel(0(24)240, glp(solid) glc(gs15) glw(vthin)) ylabel(0(0.25)1)
list if inlist(_t, 12, 24, 36, 48, 60, 120, 240)

*Cost Estimates for Wells in Federal Deep Waters (i.e. greater 1,000 feet of water depth)
cls

* All wells in Federal Waters - Both Pacific and GoM
use "$dir/data/Wellbore Cost Data.dta", clear
    label list region_lbl
    keep if inlist(region_id, 4,5 )            //Keeps wells in federal waters
    
    label list currentstatus_lbl
    drop if inlist(CurrentStatus,2,7)            //Drops wells that have alrady been P&Aed, or hav enot yet been drilled. 

    label list well_category_lbl
    keep if inlist(well_category,4,5)            //Keeps wells that are in deep waters
    
    
    // Cost per foot is simply the BSEE cost estimate / well measured depth.
    // Dependent Variable of interest.
    gen cost_perfoot = cost_mean_logn    / md_orig_or_imputed
    gen costp50_perfoot = cost_p50 / md_orig_or_imputed
    gen costp70_perfoot = cost_p70 / md_orig_or_imputed
    gen costp90_perfoot = cost_p90 / md_orig_or_imputed

    // CHECK: we're still getting 1156 cost per foot for deepwater
    su costp50_perfoot if has_orig_md
    assert r(mean) == 1156
    assert r(sd) == 0

    // CHECK: we have variation in p70 cost
    su costp70_perfoot if has_orig_md
    assert r(sd) > 0

    // CHECK: we have variation in p90 cost
    su costp90_perfoot if has_orig_md
    assert r(sd) > 0

    su cost_perfoot if has_orig_md
    global COSTPERFOOT_DEEPWATER = r(mean)

    #delimit ;
    hist cost_perfoot if has_orig_md, 
        kden
        addplot(kdensity costp70_perfoot if has_orig_md || kdensity costp90_perfoot if has_orig_md)
        xline(1156)
        title("Federal Deepwater cost per foot")
        ;
    #delimit cr

    *First, see if water depth, distance to shore or sub-sea completion predict cost per foot. 
    reg cost_perfoot wd dist_km   ss_flag    if  has_orig_md
    reg cost_perfoot wd           ss_flag    if  has_orig_md
    reg cost_perfoot     dist_km  ss_flag md if  has_orig_md
    * Note that water depth and distance to shore are actually negatively correlated with cost per foot. 
    * Bottom line: use the average cost per foot for all. 
    
    sum cost_perfoot if  has_orig_md
    global COSTPERFOOT_DEEPWATER = r(mean)
    noi di "Cost Per Foot for All Wells Great than 1,000 feet of water depth = $COSTPERFOOT_DEEPWATER"
    
    // we cite cost/ft of $1230. Need to make sure this is the case!
    assert $COSTPERFOOT_DEEPWATER >= 1229.7 & $COSTPERFOOT_DEEPWATER <= 1229.9        //If this changes, the program will break
    assert round($COSTPERFOOT_DEEPWATER) == 1230
    
    
    *After this point, one obsevation is one 10 digit API number (i.e.) one well. NOT one borehold
    collapse   (sum)    totalMD         = md_for_summing_in_cost  ///
               (count)  boreholecount   = md_for_summing_in_cost ///
               (max)    risk1 risk2 risk3 ,  ///
               by(api10 well_category region_id risk_category supermajor) // Adding supermajor to the by group
    
    keep if inlist(well_category,4,5)
    
    gen PA_cost_estimate = $COSTPERFOOT_DEEPWATER * totalMD     //Only for wells in greater than 1,000 feet of water depth. 

        label values  risk_category risk_category_lbl
        label var risk_category "Risk Category"
        tab risk_category

        sort PA_cost_estimate 
        order api10 PA_cost_estimate totalMD boreholecount risk1 risk2 risk3 risk_category region_id

        save "$dir/data/Fed Deep Costs - Detailed.dta", replace

*Cost Estimates for Wells in Federal Shallow Waters (i.e. less than 1,000 feet of water depth)

cls

global dir .

* All wells in Federal Waters - Both Pacific and GoM
use "$dir/data/Wellbore Cost Data.dta", clear
    label list region_lbl
    keep if inlist(region_id, 4,5 )            //Keeps wells in federal waters
    
    label list currentstatus_lbl
    drop if inlist(CurrentStatus,2,7)            //Drops wells that have alrady been P&Aed, or have not yet been drilled. 

    label list well_category_lbl
    keep if inlist(well_category,3)            //Keeps wells that are in shallow waters

    // Cost per foot is simply the BSEE cost estimate / well measured depth.
    // Dependent Variable of interest.
    gen cost_perfoot = cost_mean_logn    / md_orig_or_imputed
    gen costp50_perfoot = cost_p50 / md_orig_or_imputed
    gen costp70_perfoot = cost_p70 / md_orig_or_imputed
    gen costp90_perfoot = cost_p90 / md_orig_or_imputed
    
    sum cost_perfoot costp50_perfoot  costp70_perfoot costp90_perfoot if has_orig_md
    sum cost_perfoot  if has_orig_md
    assert r(mean) >= 68.1 & r(mean) <= 68.2        //Program will break if the average cost per foot changes
    
    *Model costs per foot based on the water depth, distance to shore, and whether there is a sub-sea completion
    reg cost_perfoot wd dist_km ss_flag, vce(robust)
    reg cost_perfoot dist_km ss_flag, vce(robust)
    reg cost_perfoot wd ss_flag, vce(robust)
    *Note that the best model only includes water depth and the sub-sea flag. 
    
    reg cost_perfoot wd ss_flag, vce(robust)
        predict cost_perfoot_pred
    assert e(N) == 6865 // we cite this number

    // we cite this number
    count if !e(sample) & region_id == "FEDERAL GOM":region_lbl
    assert r(N) == 6176

    // ---------------------------
    // NOTE - we could exclude wells w/ out orig MD
    // ---------------------------
    // *Model costs per foot based on the water depth, distance to shore, and whether there is a sub-sea completion
    // reg cost_perfoot wd dist_km ss_flag if has_orig_md, vce(robust)
    // reg cost_perfoot    dist_km ss_flag if has_orig_md, vce(robust)
    // reg cost_perfoot wd         ss_flag if has_orig_md, vce(robust)
    // *Note that the best model only includes water depth and the sub-sea flag. 
    
    // reg cost_perfoot wd ss_flag if has_orig_md, vce(robust)
    //     predict cost_perfoot_pred



    twoway scatter cost_perfoot cost_perfoot_pred,    ///
        graphregion(color(white)) ///
        plotregion(margin(zero)) ///
        xsize(16) ysize(9)         ///
        xtitle("Model Prediction")        ///
        ytitle("BSEE Cost Estimate")
        
       graph export "$dir/graphs/Fed Shallow Water In Sample Cost per Foot Scatter.png", replace
    
    gen     PA_cost_estimate_borehole =  cost_perfoot      * md_for_summing_in_cost                                  // Use the provided cost per foot if avaiable. 
    replace PA_cost_estimate_borehole =  cost_perfoot_pred * md_for_summing_in_cost if PA_cost_estimate_borehole==.  // Use estimated cost per foot where not avaiable. 
    
    *Next, aggregate boreholes up to wells and aggregate costs. 
    collapse    (sum) 	totalMD 			= md_for_summing_in_cost  ///
    			(sum) 	PA_cost_estimate 	= PA_cost_estimate_borehole  ///
    			(count) boreholecount 		= md_for_summing_in_cost ///
    			(max) 	risk1 risk2 risk3 ,  ///
    			by(api10 well_category region_id risk_category supermajor) // Adding supermajor to the by group
    
        label values  risk_category risk_category_lbl
        label var risk_category "Risk Category"
        tab risk_category

        format PA_cost_estimate %12.0gc
        order api10 PA_cost_estimate totalMD boreholecount risk1 risk2 risk3 risk_category region_id supermajor

        save "$dir/data/Fed Shallow Costs - Detailed.dta", replace
    
    
    

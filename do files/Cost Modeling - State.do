*Cost Estimates for Wells in state waters 

// NOTE: requires geonear
// ssc install geonear
cls

use "$dir/data/Wellbore Cost Data.dta", clear

	label list currentstatus_lbl
    drop if inlist(CurrentStatus,2,7)			//Drops wells that have alrady been P&Aed, or hav enot yet been drilled. 

	label list well_category_lbl
	keep if inlist(well_category,1,2,3)			//Keeps wells that are in state water OR shallow federal. 
												*Note: wells in federal shallow water will be used to model state waters. 
												

    // Cost per foot is simply the BSEE cost estimate / well measured depth.
    // Dependent Variable of interest.
    gen cost_perfoot = cost_mean_logn    / md_orig_or_imputed
												
	su cost_perfoot if has_orig_md
    global COSTPERFOOT_SHALLOWWATER = r(mean)
	
	// Drops wells more than 15 km from shore.
    // Note that 9 miles is 14.48 km. Therefore, this is ones just outside of the
    // TX 9 mile limit and plenty in the 3-9 mile range.
    drop if dist_km>15

	*Check for subsea completions
	count if ss_flag==1
	tab region_id if ss_flag==1
	count if ss_flag==1 & inlist(region_id,1,2,3,6,7,8,9,10)
	assert r(N)==0		//Make sure that there are no-subsea completions in state waters. 
	drop if ss_flag==1 	//Drop subsea completions as not relevant for modeling purposes. Note there are only four that meet the prior criteria. 
	
	sum cost_perfoot  if has_orig_md
	assert r(mean) >= 52.62 & r(mean) <= 52.63		//Program will break if the average cost per foot changes
	
	*Model costs per foot based on the water depth and distance to shore
	reg cost_perfoot wd dist_km , vce(robust)
	reg cost_perfoot    dist_km , vce(robust)
	reg cost_perfoot wd         , vce(robust)
	*Note that the best model only includes water depth only. 

	sort cost_perfoot 
	count if cost_perfoot >= 250 & cost_perfoot!=.
	assert r(N) ==3 	//There are three wells with very high cost per foot. I will not remove, but will not include them in the regression. 
	
	reg cost_perfoot wd if cost_perfoot < 200, vce(robust)
	assert e(N) == 1708 // cited in paper

	predict cost_perfoot_pred
	exit
	
	twoway scatter cost_perfoot cost_perfoot_pred,    ///
		graphregion(color(white)) ///
		plotregion(margin(zero)) ///
		xsize(16) ysize(9)         ///
		xtitle("Model Prediction")        ///
		ytitle("BSEE Cost Estimate")
		
	gen UniqueID=_n	
	tempfile FullDataWithID
	save "`FullDataWithID'"
	
	count if missing(md_for_summing_in_cost)
	if r(N) > 0 {

		preserve
			keep UniqueID md_for_summing_in_cost api latitude longitude 
			keep if missing(md_for_summing_in_cost)
			drop md_for_summing_in_cost

			quietly: count
			if r(N) > 0 {
				sort UniqueID
				quietly by UniqueID:  gen dup = cond(_N==1,0,_n)
				tab dup
				count if dup != 0
				assert r(N) == 0
				drop dup
			}
			tempfile missingMD
			save "`missingMD'"

		restore 

		preserve
			keep UniqueID md_for_summing_in_cost api latitude longitude 
			keep if md_for_summing_in_cost != . 
			drop md_for_summing_in_cost
			
			sort UniqueID
			quietly by UniqueID:  gen dup = cond(_N==1,0,_n)
			tab dup
			drop dup
			
			tempfile NOmissingMD
			save "`NOmissingMD'"
		restore 

		
		
		use "`missingMD'", clear
			geonear UniqueID latitude longitude  using "`NOmissingMD'", neighbors(UniqueID latitude longitude )
			sum km_to_nid
			
			sort UniqueID
			by UniqueID:  gen dup = cond(_N==1,0,_n)
			tab dup
			drop dup
			
			tempfile NEIGHBORS
			save "`NEIGHBORS'"

			
		use "`NEIGHBORS'", clear
		keep UniqueID nid 
		rename UniqueID IDwithMissing
		rename nid UniqueID
		
		merge m:1 UniqueID using "`NOmissingMD'"
			keep if _merge==3
			
			rename api apicheck
			drop _merge latitude longitude
			
		merge m:1 UniqueID using  "`FullDataWithID'"
		
			keep if _merge==3
			keep UniqueID IDwithMissing md_for_summing_in_cost api apicheck  
			
			keep md_for_summing_in_cost IDwithMissing
			rename IDwithMissing UniqueID
			rename md_for_summing_in_cost md_for_summing_closeneighbor
			
			save "$dir/data/intermediate/Measured Depth - Closest Neighbor for Missing.dta", replace
			

		use "`FullDataWithID'", clear
			merge 1:1 UniqueID using "$dir/data/intermediate/Measured Depth - Closest Neighbor for Missing.dta"
			
		replace md_for_summing_in_cost = md_for_summing_closeneighbor if md_for_summing_in_cost==.
	}
		count
			local TotalObs = r(N)
		sum md_for_summing_in_cost
			local ObsWMD = r(N)
		assert `TotalObs' == `ObsWMD'	//Ensures that all wells have a measured depth. 
		
	gen PA_cost_estimate_borehole =  cost_perfoot_pred * md_for_summing_in_cost
	keep if inlist(well_category,1,2)

	
	collapse 	(sum) 	PA_cost_estimate 	= PA_cost_estimate_borehole ///
				(sum) 	totalMD 			= md_for_summing_in_cost ///
				(count) boreholecount 		= md_for_summing_in_cost ///
				(max) 	risk1 risk2 risk3  , ///
				by(api10 well_category region_id risk_category)

		label values  risk_category risk_category_lbl
		label var risk_category "Risk Category"
		tab risk_category

		sort PA_cost_estimate 
		format PA_cost_estimate %18.0gc
		order api10 PA_cost_estimate totalMD boreholecount risk1 risk2 risk3 risk_category
		save "$dir/data/State Waters Costs - Detailed.dta", replace

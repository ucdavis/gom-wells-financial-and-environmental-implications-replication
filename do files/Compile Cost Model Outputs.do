*Compile Cost Data


    *Detailed - Well Level Cost Estimates
    use "$dir/data/Fed Deep Costs - Detailed.dta", clear
        append using "$dir/data/Fed Shallow Costs - Detailed.dta"
        append using "$dir/data/State Waters Costs - Detailed.dta"
        
        label var PA_cost_estimate "Estimated Cost to P&A"
        label var totalMD "Total Measured Depth of all boreholes (i.e. sidetracks) in well"
        label var boreholecount "Counts of boreholes in well"
        label var risk1 "Well Temporarily P&Aed"
        label var risk2 "Inactive, Idle, Shut in, but not P&Aed"
        label var risk3 "Inactive Lease (only federal wells)"
        label var region_id "Region"
        label var supermajor "Supermajor"
        
        sort region_id well_category risk_category supermajor
        
        save "$dir/data/Detailed Cost Data Combined.dta", replace
    
        * Some data checks. 
        tab well_category
        tab boreholecount if api10!="00-000-00000"
        tab risk1
        tab risk2 
        tab risk3 
        tab risk_category
        tab region_id
        tab supermajor
        
        *These are old wells with no listed API. 
        *Note that all are in Louisiana and all are inactive. 
        tab well_category if api10=="00-000-00000"
        tab risk_category if api10=="00-000-00000"
        tab region_id if api10=="00-000-00000"        
        

// ----------------------------------
// summarise
// ----------------------------------
	collapse 	(sum) 	Aggregate_Cost 	= PA_cost_estimate ///
						Aggregate_MD 	= totalMD ///
						WellboreCount 	= boreholecount ///
				(count) WellCount 		= PA_cost_estimate,  ///
				by(risk_category region_id well_category supermajor) // Adding supermajor to the by group
	
	format Aggregate_Cost %18.0gc
	        
    // label var Aggregate_Cost "Estimated Cost to P&A Group"
    // label var Aggregate_MD "Total Measured Depth of all boreholes (i.e. sidetracks) in well"
    // label var region_id "Region"
    // label var WellCount "Counts of Wells in Category"
    // label var supermajor "Supermajor"
    
    sort region_id well_category risk_category supermajor
    
    save "$dir/data/Summary Cost Data Combined.dta", replace
    

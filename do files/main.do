clear all

// global dir "/Users/gregupton/Documents/GitHub/offshore-wells-plug-and-abandon"
// global dir "/Users/gregupton/Dropbox/CES RA Shared Folder/offshore-wells-plug-and-abandon"
// global dir "F:\Dropbox\WellPAv4_Sid"
global dir "E:\projects\gom-wells-financial-and-environmental-implications-replication\"
global RScript "C:\Users\mjagerton\Documents\R\R-4.1.2\bin\x64\RScript.exe"

cd $dir

// ---------------------------------------------------
// Hazard analysis
// ---------------------------------------------------

do "$dir/do files/hazard-modeling.do"

// -----------------------------------------------------
// These files USE "$dir/data/clean/Wellbore Cost Data.dta"
// 
// Then, they do cost modeling (regression or mean) and possibly
// make some graphs
// 
// Finally, they export (a) cost for each well, and (b) summary of costs
// to excel
// -----------------------------------------------------

do "$dir/do files/Cost Modeling - Federal Deep.do"
do "$dir/do files/Cost Modeling - Federal Shallow.do"
do "$dir/do files/Cost Modeling - State.do" 

// Appends estimated costs for fed deep, fed shallow, and state 
// for well AND summary
do "$dir/do files/Compile Cost Model Outputs.do"

*do "$dir/do files/Summary_Tables.do"   

shell "$RScript" "$dir\R\make-tables.R"


rm(list=ls())

library(data.table)
library(forcats)
library(tables)
library(haven)
library(xtable)


DROP_CALIFORNIA <- TRUE # FALSE
DIR <- "E:/projects/gom-wells-financial-and-environmental-implications-replication/"

# ingest
wells      <- read_dta(paste0(DIR, "data/Wellbore Cost Data.dta"))
well_costs <- read_dta(paste0(DIR, "data/Detailed Cost Data Combined.dta"))

# change "labelled" to "factor"
for (n in c("risk_category", "region_id", "well_category", "CurrentStatus")) {
  if (n != "CurrentStatus") {
    well_costs[[n]] <- as_factor(well_costs[[n]])
  }
  wells[[n]]      <- as_factor(wells[[n]])
}

# turn in to data.tables
setDT(well_costs)
setDT(wells)

# CHECK - all but 1 well has an MD, and it's P&Aed
stopifnot(all(wells[, .N, is.finite(md_orig_or_imputed)][,N] == c(103587,1)))
# stopifnot(all(wells[is.na(md_orig_or_imputed), .N] == 1))
stopifnot(all(wells[is.na(md_orig_or_imputed), CurrentStatus == "P&A"]))

# drop California & federal pacific
if (DROP_CALIFORNIA==T) {
  wells <- wells[!(region_id %like% "^(CA |FEDERAL PAC)")]
  well_costs <- well_costs[!(region_id %like% "^(CA |FEDERAL PAC)")]
}

# relabel by (relevant) well location category
for (x in list(well_costs, wells)) {
  x[
    i = ,
    j = `Well Location Detailed` := fct_recode(
      well_category,
      `Federal Shallow Waters` = "Federal Shallow Waters (Less than 1,000 feet water)",
      `Federal Deep Waters` = "Federal Deep Waters (1,000-5,000 feet water)",
      `Federal Ultra-Deep Waters` = "Federal Ultra-Deep Waters (5,000+ feet water)"
    )
  ]
  x[, `Well Location` := factor(`Well Location Detailed`)]
  x[`Well Location Detailed` == "Federal Ultra-Deep Waters", `Well Location` := "Federal Deep Waters"]
  x[,`Well Location` := factor(`Well Location`)]

  x[, `Well Location Compressed` := `well_category`]
  x[`well_category` %like% "^(State|Inland)", `Well Location Compressed` := "State Waters"]
  x[`well_category` %like% "Deep", `Well Location Compressed` := "Federal Deepwater (>1,000 feet water depth)"]
  x[, `Well Location Compressed` := factor(`Well Location Compressed`, levels=c("Federal Deepwater (>1,000 feet water depth)", "Federal Shallow Waters (Less than 1,000 feet water)", "State Waters"))]
  
  x[well_category %like% "Federal", `Well Location Federal Only` := factor(`Well Location Compressed`)]
  
  x[region_id %like% "^(AL|LA|TX|CA)", State := substr(region_id,1,2)]
  x[region_id %like% "^FED", State := "Federal"]
  
}

rm(x)


# ------------------------------------------
# has MD?
# ------------------------------------------ 

wells[, has_md := is.finite(md_orig_or_imputed)]
wells[, `MD Imputed` := ifelse(is.finite(md_orig_or_imputed) & is.na(md), 1, 0)]

# for table 1
wells[
  i = ,
  j = `:=`(
    `Wellbore Count` = 1L,
    `Well Count`     = ifelse(is.finite(is_original_wellbore), is_original_wellbore, 1L),
    `Status`         = factor(CurrentStatus),
    has_cost         = is.finite(cost_p50) %>% ifelse("BSEE Cost Estimate", "No BSEE Cost Estimate") %>% factor()
  )
]

# for table 2
wells[
  i = ,
  j = `:=`(
    spud_year      = year(spud_dt),
    cost_perfoot   = cost_p50 / md_orig_or_imputed,
    cost_perfoot70 = cost_p70 / md_orig_or_imputed,
    cost_perfoot90 = cost_p90 / md_orig_or_imputed
  )
]

wells[
  i = ,
  j = `:=`(
    `P50 Cost Per Foot (\\$/foot)`         = cost_perfoot,
    `P70 Cost Per Foot (\\$/foot)`         = cost_perfoot70,
    `P90 Cost Per Foot (\\$/foot)`         = cost_perfoot90,
    `Expected P\\&A Cost (million \\$)`    = cost_mean_logn/10^6,
    `Water Depth (ft)`                     = wd,
    `Measured Depth (ft)`                  = md,
    `Measured Depth, orig or imputed (ft)` = md_orig_or_imputed,
    `Distance to Shore (km)`               = dist_km,
    `Spud Year`                            = spud_year,
    `Subsea Completion`                    = ifelse(ss_flag, 1, 0),
    `Supermajor Ownership`                 = supermajor
  )
]
# ----------------------------------------------
# Table 1
# ----------------------------------------------

table_options(latexleftpad=F, latexrightpad=F, mathmode=F)
tab1 <- tabular( 
  (`Well Location` + 1 + Hline(2:3) + `Status` + 1) ~ (`Well Count`)*(length + sum)*Format(big.mark=",", justification="r"),
  data=wells,
  booktabs=T
)

tab1

# ----------------------------------------------
# Table 2
# ----------------------------------------------



# wells[
#   i = CurrentStatus != "P&A" & is.finite(),
#   j = .N,
#   keyby = .(
#     `Well Location` %like% "^Federal",
#     is.finite(`Measured Depth (ft)`),
#     is.finite(`Measured Depth, orig or imputed (ft)`)
#   )
# ]


# wells[CurrentStatus=="Active" & is.na(`Measured Depth, orig or imputed (ft)`)]


Mean   <- function(x) mean(x, na.rm=TRUE)
Median <- function(x) median(x, na.rm=TRUE)
SD     <- function(x) sd(x, na.rm=TRUE)

table_options(latexleftpad=F, latexrightpad=F, mathmode=F)

pctfmt <- function(x) {
  s <- format(x*100, digits=1) %>% sprintf("%s\\%%", .)
  return(s)
}


# table 2 summary stats
tab2a <- tabular(
  `Well Location Compressed`* # to break out inland waters vs offshore, just put `Well Location`*
    (
        (`P50 Cost Per Foot (\\$/foot)` + `P70 Cost Per Foot (\\$/foot)` + `P90 Cost Per Foot (\\$/foot)`) * Format(digits=1,big.mark=",") + 
        (`Expected P\\&A Cost (million \\$)`)                                                              * Format(digits=2) + 
        (`Water Depth (ft)` + `Measured Depth (ft)` + `Measured Depth, orig or imputed (ft)`)              * Format(digits=0, big.mark=",", scientific=F) + 
        (`Spud Year`)                                                                                      * Format(digits=0) + 
        (`Distance to Shore (km)`)                                                                         * Format(digits=0) + 
        (`Subsea Completion` + `MD Imputed`)                                                               * Format(pctfmt()) +
        (`Supermajor Ownership`)                                                                           * Format(pctfmt())
  ) ~ (has_cost)*(Mean + Median + SD),
  data=wells[CurrentStatus != "P&A"]
)

tab2a

# table 2 wellcount & wellbore count
tab2b <- tabular(
  `Well Location Compressed`*is_original_wellbore*(sum + length) ~ (has_cost),
  data=wells[CurrentStatus != "P&A"]
)

tab2b

# table_options(latexleftpad=F, latexrightpad=F, mathmode=F)
# latexTable(tab2a, mathmode=F)

# ---------------------------------------
# Prep well costs for tables
# ---------------------------------------


# so we can count
well_costs[, `Well Count` := 1L]
well_costs[, `Wellbore Count` := boreholecount]
well_costs[, `Aggregate_MD` := totalMD]
well_costs[, `Aggregate Cost (bn)` := PA_cost_estimate/10^9]

well_costs[, A      := ifelse(risk2, "Inactive Wells (A)", NA) %>% factor()]
well_costs[, B      := ifelse(risk1, "Temporary P\\&A (B)", NA) %>% factor()]
well_costs[, C      := ifelse(risk3, "Inactive Lease (C)", NA) %>% factor()]

well_costs[, AB    := ifelse(risk2 & risk1, "A\\&B", NA) %>% factor()] 
well_costs[, AC    := ifelse(risk2 & risk3, "A\\&C", NA) %>% factor()] 
well_costs[, BC    := ifelse(risk1 & risk3, "B\\&C", NA) %>% factor()]

well_costs[, ABC   := ifelse(risk3 & risk2 & risk1, "A\\&B\\&C", NA) %>% factor()] 
well_costs[, orABC := ifelse(risk1 | risk2 | risk3, "A or B or C", NA) %>% factor()]
well_costs[, orAB := ifelse(risk1 | risk2, "A or B", NA) %>% factor()]

well_costs[, Active := ifelse(risk2 + risk1 + risk3 == 0, "Active / Recently Active", NA) %>% factor()]

well_costs[is.finite(supermajor), Supermajor := ifelse(supermajor==1, "Supermajors", "Non-supermajors") %>% factor()]
well_costs[, Supermajor := factor(Supermajor, levels=c("Supermajors", "Non-supermajors"))]

# tables
dcast(well_costs, `Well Location` ~ ., value.var = c("Wellbore Count", "Well Count"), fun.aggregate = sum)

# ---------------------------------------
# Table 3
# ---------------------------------------

# See 
# wells[api10=="17-101-20856"]

# CHECK - who's in wells vs well-costs?
# stopifnot(all(well_costs$api10 %in% wells[CurrentStatus != "P&A" & has_md == T, api10]))

well_costs[!(api10 %in% wells[CurrentStatus != "P&A" & has_md == T, api10]), .N, `Well Location Detailed`]

dollarfmt <- function(x, digits, ...) {
  s <- format(x, digits=digits, trim=T, ...) %>% sprintf("\\$%s", .)
  return(s)
}

parensfmt <- function(x, ...) {
  s <- format(x, big.mark=",", trim=T, ...) %>% sprintf("\\textit{(%s)}", .)
  return(s)
}


# Panel B
tab3 <- tabular(
  (Total=1 + A + B + C + Active + AB + AC + BC + ABC + orABC)*((`Aggregate Cost (bn)`)*Format(dollarfmt(digits=1)) + (`Well Count`)*Format(parensfmt())) * (sum) ~ (Total=1) + (`Well Location Compressed`),
  data=well_costs
)


tab3


tabular(
  (Total=1 + A + B + C + Active + AB + AC + BC + ABC + orABC)*((`Aggregate Cost (bn)`)*Format(dollarfmt(digits=1)) + (`Well Count`)*Format(parensfmt())) * (sum) ~ (Total=1) + (`Well Location Compressed`),
  data=well_costs
)


# ---------------------------------------
# Average cost to P\&A wells
# ---------------------------------------


well_costs[
  i = ,
  j = sum(PA_cost_estimate)/sum(`Well Count`)/10^6,
  keyby = .(`Well Location Compressed`)
]


# Market cap


# ---------------------------------------
# Table 5
# ---------------------------------------

# Panel A
tab5 <- tabular(
  (Total=1 + A + B + C + Active)*((`Aggregate Cost (bn)`)*Format(dollarfmt(digits=1)) + (`Well Count`)*Format(parensfmt())) * (sum) ~ (Total=1) + (`Well Location Federal Only`*Supermajor),
  data=well_costs[well_category %like% "Federal"]
)

tab5


# ---------------------------------------
# Table 5
# ---------------------------------------

dollarfmt2 <- function(x, ...) {
  s <- round(x*1000, digits=0) %>% format(x, drop0trailing=T, nsmall=0, big.mark=",", trim=T, ...) %>% sprintf("\\$%s", .)
  return(s)
}


tab6 <- tabular(
  (Total=1 + A + B + Active + AB + orAB)*((`Aggregate Cost (bn)`)*Format(dollarfmt2()) + (`Well Count`)*Format(parensfmt())) * (sum) ~ (Total=1) + (factor(State)),
  data=well_costs[`Well Location` %like% "^(Inland|State)"]
)


tab6

tab7 <- tabular(
  (Total=1 + A + B + Active + AB + orAB)*((`Aggregate Cost (bn)`)*Format(dollarfmt2()) + (`Well Count`)*Format(parensfmt())) * (sum) ~ (Total=1) + (factor(State)*factor(`Well Location`)*DropEmpty() ),
  data=well_costs[`Well Location` %like% "^(Inland|State)"]
)

tab7

tabular(
  (Total=1 + A + B + Active + AB + orAB)*((`Aggregate Cost (bn)`) + (`Well Count`)*Format(parensfmt())) * (sum) ~ (Total=1) + (factor(State)*factor(`Well Location`)*DropEmpty() ),
  data=well_costs[`Well Location` %like% "^(Inland|State)"]
)

# ---------------------------------------
# Final print out of tables
# ---------------------------------------

fn <- ifelse(DROP_CALIFORNIA, "tables/all-new-tables-from-R-NO-PACIFIC.tex", "tables/all-new-tables-from-R.tex")

print_table <- function(x, file) {
  # xx <- gsub("\\phantom\\{\\d+\\}", "", x)
  xx <- gsub("_", "\\-", x)
  xxx <- gsub("P&A", "P\\\\&A", xx)
  cat(paste0(xxx), file=file, append=T)
  cat("\n\n\n\n\n\n",  file=file, append=T)
}

cat("
\\documentclass[10pt]{article} 
\\usepackage{amsmath,amssymb,setspace,rotating,subfigure,hyperref,multirow,comment,soul,color,booktabs}
\\usepackage[landscape]{geometry}
\\usepackage[para,online,flushleft]{threeparttable}

\\begin{document}



", file = fn, append = F)
toLatex(tab1,  mathmode=F, caption="Number of Wells and Well Bores Ever Drilled in GoM") %>% print_table(file=fn)
toLatex(tab2a, mathmode=F, caption="Summary Statistics for Unplugged GoM Wells") %>% print_table(file=fn)
toLatex(tab2b, mathmode=F, caption="Summary Statistics (part 2)") %>% print_table(file=fn)
toLatex(tab3,  mathmode=F, caption="Aggregated GoM P\\&A Cost Estimates (billion \\$) and Well Counts by Jurisdiction") %>% print_table(file=fn)
toLatex(tab5,  mathmode=F, caption="Aggregated Federal GoM P\\&A Cost Estimates (billion \\$) and Well Counts by Supermajor Ownership") %>% print_table(file=fn)
toLatex(tab6,  mathmode=F, caption="Aggregated P\\&A Cost Estimates (million \\$) and Well Counts in State Waters") %>% print_table(file=fn)
toLatex(tab7,  mathmode=F, caption="Aggregated P\\&A Cost Estimates (million \\$) and Well Counts in State Waters") %>% print_table(file=fn)


cat("
\\end{document} 
", file = fn, append = T)


# -----------------------------
# check sum stats
# -----------------------------

wells[
  i = `Well Location Federal Only` %like% "Deep" & is.finite(`P50 Cost Per Foot (\\$/foot)`),
  j = .N,
  keyby = .(`P50 Cost Per Foot (\\$/foot)` < 1156, `P50 Cost Per Foot (\\$/foot)`)
]

wells[
  i = `Well Location Federal Only` %like% "Deep",
  .N,
  keyby = .(is.na(`Measured Depth (ft)`), abs(`P50 Cost Per Foot (\\$/foot)` - 1156) > 1)
]


stopifnot(wells[
  i = `Well Location Federal Only` %like% "Deep" & abs(`P50 Cost Per Foot (\\$/foot)` - 1156) > 1,
  all(`MD Imputed` == 1),
])


# show sum stats for deep & shallow water
sstats <- function(x) list(mean=mean(x, na.rm=T), median=median(x, na.rm=T), sd = sd(x, na.rm=T))

wells[
  i = is.finite(`Measured Depth (ft)`),
  lapply(.SD, sstats),
  .SDcols = c("P50 Cost Per Foot (\\$/foot)", "P70 Cost Per Foot (\\$/foot)", "P90 Cost Per Foot (\\$/foot)"),
  keyby = .(`Well Location Federal Only`)
]





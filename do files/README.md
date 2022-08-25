# remaining checks to do

## Checks on sidetrack depth adjustment

1. Discrepancy between wells & boreholes in data
2. Identification of original wellbore
3. Calculation of marginal sidetrack depth
    - Exclusion of zero KOP from calculation
    - Do we want `share=mean(1-kop/md)` or `share = 1- mean(kop)/mean(md)`?
    - Ensure marginal depth is greater than 0
4. Move marginal sidetrack depth calcs into R or Stata for transparency

## Checks on sidetrack status

- New version in R (not saved!!!) ensures unique field/api. This only really matters in the FO Pacific

# ccrproj
The code and data associated with this repository are for projecting populations using the Hamilton-Perry Method, or cohort change ratios (CCRs). 

The three scripts currently available in the repository must be run in sequence starting with p12.proj.cnty.R [#1], then pct12.proj.cnty.R [#2], then pct12.proj.trct.R [#3]. The input data for the second and third are dependent upon the outputs of the previous script. 

The study region is a 23 county region near Georgia's coast including parts of SC and FL. The initial base and launch years for the CCR are 2000 and 2010. The projection period is from 2010 to 2050 in 10 year intervals.

The first script [#1] projects populations by age/sex cohorts using US Census p12 tables for 2000 and 2010.

The second script [#2] projects populations by age/sex/race/ethnicity cohorts controlled to the outputs of [#1] at the county level using pct12 tables for 2000 and 2010.

The third script [#3] projections populations by age/sex/race/ethnicity cohorts controlled to the outputs of [#2] at the tract level using pct12 tables for 2000 and 2010.

For details of this particular code and the specific sequence of controls used in the population projections, see our SocArXiv preprint: https://dx.doi.org/10.17605/OSF.IO/PKR75

For more details on CCR, see Baker et al, 2017. 

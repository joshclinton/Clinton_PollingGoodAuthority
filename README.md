Repository for Good Authority Piece on Polling & Weighting
Josh Clinton
Vanderbilt University
October 2024

Included is the code required to replicate the analysis and results in the Good Authority piece.  The one complication is that the CPS data being used and referenced when creating the weighting targets cannot be shared given usage limitations.  Even so, I provide all of the code used to wrangle the extracted data from https://cps.ipums.org/cps/

I extracted the following variables for samples containing the voter supplement (November) were: 
Type	Variable	Label
H	YEAR	Survey year
H	REGION	Region and division
H	STATEFIP	State (FIPS code)
H	COUNTY	County (FIPS code)
P	AGE	Age
P	SEX	Sex
P	RACE	Race
P	HISPAN	Hispanic origin
P	EDUC	Educational attainment recode
P	VOTED	Voted for the most recent November election
P	VOSUPPWT	Voter Supplement Weight

Data includes:

1) ClintonGoodAuthorityData.Rds
The survey data being analyzed.  Only the variables used are included in the extract.

Code includes:

1) ClintonReadCPS.do
STATA code I used to process the raw CPS extract  I downloaded by IPUMS CPS: https://cps.ipums.org/cps/

2) ClintonCreateCPSTargets.R: 
The code I used to process the CPS file I pre-processed in STATA using ClintonReadCPS.do

3) ClintonRecodeYouGov.R:  
This is the code I used to wrangle the survey data to match CPS Targets

4) Clinton_GoodAuthorityCode.R
The code used to produce the numbers in the piece (as well as other numbers).  This code sources the following weighing scripts.

5) ClintonWeightingCode2016.R: 
Weight data to 2016 CPS Voter Supplement multiple ways

6) ClintonWeightingCode2020.R: 
Weight data to 2020 CPS Voter Supplement multiple ways

7) ClintonWeightingCode2022.R: 
Weight data to 2022 CPS Voter Supplement multiple ways


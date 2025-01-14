******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************
**		Recode 2016 and 2020 CPS Voter Supplement Extract to create weighting Targets
**
**		Josh Clinton
**		Vanderbilt University
******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************


** NOTE: You need to set the Stata working directory to the path where the data file is located.

clear
set more off


clear
quietly infix              ///
  int     year      1-4    ///
  byte    region    5-6    ///
  byte    statefip  7-8    ///
  long    county    9-13   ///
  byte    age       14-15  ///
  byte    sex       16-16  ///
  int     race      17-19  ///
  int     hispan    20-22  ///
  int     educ      23-25  ///
  byte    voted     26-27  ///
  double  vosuppwt  28-37  ///
  using `"cps_00006.dat"'

replace vosuppwt = vosuppwt / 10000

format vosuppwt %10.4f

label var year     `"Survey year"'
label var age      `"Age"'
label var sex      `"Sex"'
label var race     `"Race"'
label var hispan   `"Hispanic origin"'
label var educ     `"Educational attainment recode"'
label var voted    `"Voted for the most recent November election"'
label var vosuppwt `"Voter Supplement Weight"'


format vosuppwt %10.4f

label var year     `"Survey year"'
label var region   `"Region and division"'
label var statefip `"State (FIPS code)"'
label var county   `"County (FIPS code)"'
label var age      `"Age"'
label var sex      `"Sex"'
label var race     `"Race"'
label var hispan   `"Hispanic origin"'
label var educ     `"Educational attainment recode"'
label var voted    `"Voted for the most recent November election"'
label var vosuppwt `"Voter Supplement Weight"'

label define region_lbl 11 `"New England Division"'
label define region_lbl 12 `"Middle Atlantic Division"', add
label define region_lbl 21 `"East North Central Division"', add
label define region_lbl 22 `"West North Central Division"', add
label define region_lbl 31 `"South Atlantic Division"', add
label define region_lbl 32 `"East South Central Division"', add
label define region_lbl 33 `"West South Central Division"', add
label define region_lbl 41 `"Mountain Division"', add
label define region_lbl 42 `"Pacific Division"', add
label define region_lbl 97 `"State not identified"', add
label values region region_lbl

label define statefip_lbl 01 `"Alabama"'
label define statefip_lbl 02 `"Alaska"', add
label define statefip_lbl 04 `"Arizona"', add
label define statefip_lbl 05 `"Arkansas"', add
label define statefip_lbl 06 `"California"', add
label define statefip_lbl 08 `"Colorado"', add
label define statefip_lbl 09 `"Connecticut"', add
label define statefip_lbl 10 `"Delaware"', add
label define statefip_lbl 11 `"District of Columbia"', add
label define statefip_lbl 12 `"Florida"', add
label define statefip_lbl 13 `"Georgia"', add
label define statefip_lbl 15 `"Hawaii"', add
label define statefip_lbl 16 `"Idaho"', add
label define statefip_lbl 17 `"Illinois"', add
label define statefip_lbl 18 `"Indiana"', add
label define statefip_lbl 19 `"Iowa"', add
label define statefip_lbl 20 `"Kansas"', add
label define statefip_lbl 21 `"Kentucky"', add
label define statefip_lbl 22 `"Louisiana"', add
label define statefip_lbl 23 `"Maine"', add
label define statefip_lbl 24 `"Maryland"', add
label define statefip_lbl 25 `"Massachusetts"', add
label define statefip_lbl 26 `"Michigan"', add
label define statefip_lbl 27 `"Minnesota"', add
label define statefip_lbl 28 `"Mississippi"', add
label define statefip_lbl 29 `"Missouri"', add
label define statefip_lbl 30 `"Montana"', add
label define statefip_lbl 31 `"Nebraska"', add
label define statefip_lbl 32 `"Nevada"', add
label define statefip_lbl 33 `"New Hampshire"', add
label define statefip_lbl 34 `"New Jersey"', add
label define statefip_lbl 35 `"New Mexico"', add
label define statefip_lbl 36 `"New York"', add
label define statefip_lbl 37 `"North Carolina"', add
label define statefip_lbl 38 `"North Dakota"', add
label define statefip_lbl 39 `"Ohio"', add
label define statefip_lbl 40 `"Oklahoma"', add
label define statefip_lbl 41 `"Oregon"', add
label define statefip_lbl 42 `"Pennsylvania"', add
label define statefip_lbl 44 `"Rhode Island"', add
label define statefip_lbl 45 `"South Carolina"', add
label define statefip_lbl 46 `"South Dakota"', add
label define statefip_lbl 47 `"Tennessee"', add
label define statefip_lbl 48 `"Texas"', add
label define statefip_lbl 49 `"Utah"', add
label define statefip_lbl 50 `"Vermont"', add
label define statefip_lbl 51 `"Virginia"', add
label define statefip_lbl 53 `"Washington"', add
label define statefip_lbl 54 `"West Virginia"', add
label define statefip_lbl 55 `"Wisconsin"', add
label define statefip_lbl 56 `"Wyoming"', add
label define statefip_lbl 61 `"Maine-New Hampshire-Vermont"', add
label define statefip_lbl 65 `"Montana-Idaho-Wyoming"', add
label define statefip_lbl 68 `"Alaska-Hawaii"', add
label define statefip_lbl 69 `"Nebraska-North Dakota-South Dakota"', add
label define statefip_lbl 70 `"Maine-Massachusetts-New Hampshire-Rhode Island-Vermont"', add
label define statefip_lbl 71 `"Michigan-Wisconsin"', add
label define statefip_lbl 72 `"Minnesota-Iowa"', add
label define statefip_lbl 73 `"Nebraska-North Dakota-South Dakota-Kansas"', add
label define statefip_lbl 74 `"Delaware-Virginia"', add
label define statefip_lbl 75 `"North Carolina-South Carolina"', add
label define statefip_lbl 76 `"Alabama-Mississippi"', add
label define statefip_lbl 77 `"Arkansas-Oklahoma"', add
label define statefip_lbl 78 `"Arizona-New Mexico-Colorado"', add
label define statefip_lbl 79 `"Idaho-Wyoming-Utah-Montana-Nevada"', add
label define statefip_lbl 80 `"Alaska-Washington-Hawaii"', add
label define statefip_lbl 81 `"New Hampshire-Maine-Vermont-Rhode Island"', add
label define statefip_lbl 83 `"South Carolina-Georgia"', add
label define statefip_lbl 84 `"Kentucky-Tennessee"', add
label define statefip_lbl 85 `"Arkansas-Louisiana-Oklahoma"', add
label define statefip_lbl 87 `"Iowa-N Dakota-S Dakota-Nebraska-Kansas-Minnesota-Missouri"', add
label define statefip_lbl 88 `"Washington-Oregon-Alaska-Hawaii"', add
label define statefip_lbl 89 `"Montana-Wyoming-Colorado-New Mexico-Utah-Nevada-Arizona"', add
label define statefip_lbl 90 `"Delaware-Maryland-Virginia-West Virginia"', add
label define statefip_lbl 99 `"State not identified"', add
label values statefip statefip_lbl

label define age_lbl 00 `"Under 1 year"'
label define age_lbl 01 `"1"', add
label define age_lbl 02 `"2"', add
label define age_lbl 03 `"3"', add
label define age_lbl 04 `"4"', add
label define age_lbl 05 `"5"', add
label define age_lbl 06 `"6"', add
label define age_lbl 07 `"7"', add
label define age_lbl 08 `"8"', add
label define age_lbl 09 `"9"', add
label define age_lbl 10 `"10"', add
label define age_lbl 11 `"11"', add
label define age_lbl 12 `"12"', add
label define age_lbl 13 `"13"', add
label define age_lbl 14 `"14"', add
label define age_lbl 15 `"15"', add
label define age_lbl 16 `"16"', add
label define age_lbl 17 `"17"', add
label define age_lbl 18 `"18"', add
label define age_lbl 19 `"19"', add
label define age_lbl 20 `"20"', add
label define age_lbl 21 `"21"', add
label define age_lbl 22 `"22"', add
label define age_lbl 23 `"23"', add
label define age_lbl 24 `"24"', add
label define age_lbl 25 `"25"', add
label define age_lbl 26 `"26"', add
label define age_lbl 27 `"27"', add
label define age_lbl 28 `"28"', add
label define age_lbl 29 `"29"', add
label define age_lbl 30 `"30"', add
label define age_lbl 31 `"31"', add
label define age_lbl 32 `"32"', add
label define age_lbl 33 `"33"', add
label define age_lbl 34 `"34"', add
label define age_lbl 35 `"35"', add
label define age_lbl 36 `"36"', add
label define age_lbl 37 `"37"', add
label define age_lbl 38 `"38"', add
label define age_lbl 39 `"39"', add
label define age_lbl 40 `"40"', add
label define age_lbl 41 `"41"', add
label define age_lbl 42 `"42"', add
label define age_lbl 43 `"43"', add
label define age_lbl 44 `"44"', add
label define age_lbl 45 `"45"', add
label define age_lbl 46 `"46"', add
label define age_lbl 47 `"47"', add
label define age_lbl 48 `"48"', add
label define age_lbl 49 `"49"', add
label define age_lbl 50 `"50"', add
label define age_lbl 51 `"51"', add
label define age_lbl 52 `"52"', add
label define age_lbl 53 `"53"', add
label define age_lbl 54 `"54"', add
label define age_lbl 55 `"55"', add
label define age_lbl 56 `"56"', add
label define age_lbl 57 `"57"', add
label define age_lbl 58 `"58"', add
label define age_lbl 59 `"59"', add
label define age_lbl 60 `"60"', add
label define age_lbl 61 `"61"', add
label define age_lbl 62 `"62"', add
label define age_lbl 63 `"63"', add
label define age_lbl 64 `"64"', add
label define age_lbl 65 `"65"', add
label define age_lbl 66 `"66"', add
label define age_lbl 67 `"67"', add
label define age_lbl 68 `"68"', add
label define age_lbl 69 `"69"', add
label define age_lbl 70 `"70"', add
label define age_lbl 71 `"71"', add
label define age_lbl 72 `"72"', add
label define age_lbl 73 `"73"', add
label define age_lbl 74 `"74"', add
label define age_lbl 75 `"75"', add
label define age_lbl 76 `"76"', add
label define age_lbl 77 `"77"', add
label define age_lbl 78 `"78"', add
label define age_lbl 79 `"79"', add
label define age_lbl 80 `"80"', add
label define age_lbl 81 `"81"', add
label define age_lbl 82 `"82"', add
label define age_lbl 83 `"83"', add
label define age_lbl 84 `"84"', add
label define age_lbl 85 `"85"', add
label define age_lbl 86 `"86"', add
label define age_lbl 87 `"87"', add
label define age_lbl 88 `"88"', add
label define age_lbl 89 `"89"', add
label define age_lbl 90 `"90 (90+, 1988-2002)"', add
label define age_lbl 91 `"91"', add
label define age_lbl 92 `"92"', add
label define age_lbl 93 `"93"', add
label define age_lbl 94 `"94"', add
label define age_lbl 95 `"95"', add
label define age_lbl 96 `"96"', add
label define age_lbl 97 `"97"', add
label define age_lbl 98 `"98"', add
label define age_lbl 99 `"99+"', add
label values age age_lbl

label define sex_lbl 1 `"Male"'
label define sex_lbl 2 `"Female"', add
label values sex sex_lbl

label define race_lbl 100 `"White"'
label define race_lbl 200 `"Black"', add
label define race_lbl 300 `"American Indian/Aleut/Eskimo"', add
label define race_lbl 650 `"Asian or Pacific Islander"', add
label define race_lbl 651 `"Asian only"', add
label define race_lbl 652 `"Hawaiian/Pacific Islander only"', add
label define race_lbl 700 `"Other (single) race, n.e.c."', add
label define race_lbl 801 `"White-Black"', add
label define race_lbl 802 `"White-American Indian"', add
label define race_lbl 803 `"White-Asian"', add
label define race_lbl 804 `"White-Hawaiian/Pacific Islander"', add
label define race_lbl 805 `"Black-American Indian"', add
label define race_lbl 806 `"Black-Asian"', add
label define race_lbl 807 `"Black-Hawaiian/Pacific Islander"', add
label define race_lbl 808 `"American Indian-Asian"', add
label define race_lbl 809 `"Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 810 `"White-Black-American Indian"', add
label define race_lbl 811 `"White-Black-Asian"', add
label define race_lbl 812 `"White-American Indian-Asian"', add
label define race_lbl 813 `"White-Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 814 `"White-Black-American Indian-Asian"', add
label define race_lbl 815 `"American Indian-Hawaiian/Pacific Islander"', add
label define race_lbl 816 `"White-Black--Hawaiian/Pacific Islander"', add
label define race_lbl 817 `"White-American Indian-Hawaiian/Pacific Islander"', add
label define race_lbl 818 `"Black-American Indian-Asian"', add
label define race_lbl 819 `"White-American Indian-Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 820 `"Two or three races, unspecified"', add
label define race_lbl 830 `"Four or five races, unspecified"', add
label define race_lbl 999 `"Blank"', add
label values race race_lbl

label define hispan_lbl 000 `"Not Hispanic"'
label define hispan_lbl 100 `"Mexican"', add
label define hispan_lbl 102 `"Mexican American"', add
label define hispan_lbl 103 `"Mexicano/Mexicana"', add
label define hispan_lbl 104 `"Chicano/Chicana"', add
label define hispan_lbl 108 `"Mexican (Mexicano)"', add
label define hispan_lbl 109 `"Mexicano/Chicano"', add
label define hispan_lbl 200 `"Puerto Rican"', add
label define hispan_lbl 300 `"Cuban"', add
label define hispan_lbl 400 `"Dominican"', add
label define hispan_lbl 500 `"Salvadoran"', add
label define hispan_lbl 600 `"Other Hispanic"', add
label define hispan_lbl 610 `"Central/South American"', add
label define hispan_lbl 611 `"Central American, (excluding Salvadoran)"', add
label define hispan_lbl 612 `"South American"', add
label define hispan_lbl 901 `"Do not know"', add
label define hispan_lbl 902 `"N/A (and no response 1985-87)"', add
label values hispan hispan_lbl

label define educ_lbl 000 `"NIU or no schooling"'
label define educ_lbl 001 `"NIU or blank"', add
label define educ_lbl 002 `"None or preschool"', add
label define educ_lbl 010 `"Grades 1, 2, 3, or 4"', add
label define educ_lbl 011 `"Grade 1"', add
label define educ_lbl 012 `"Grade 2"', add
label define educ_lbl 013 `"Grade 3"', add
label define educ_lbl 014 `"Grade 4"', add
label define educ_lbl 020 `"Grades 5 or 6"', add
label define educ_lbl 021 `"Grade 5"', add
label define educ_lbl 022 `"Grade 6"', add
label define educ_lbl 030 `"Grades 7 or 8"', add
label define educ_lbl 031 `"Grade 7"', add
label define educ_lbl 032 `"Grade 8"', add
label define educ_lbl 040 `"Grade 9"', add
label define educ_lbl 050 `"Grade 10"', add
label define educ_lbl 060 `"Grade 11"', add
label define educ_lbl 070 `"Grade 12"', add
label define educ_lbl 071 `"12th grade, no diploma"', add
label define educ_lbl 072 `"12th grade, diploma unclear"', add
label define educ_lbl 073 `"High school diploma or equivalent"', add
label define educ_lbl 080 `"1 year of college"', add
label define educ_lbl 081 `"Some college but no degree"', add
label define educ_lbl 090 `"2 years of college"', add
label define educ_lbl 091 `"Associate's degree, occupational/vocational program"', add
label define educ_lbl 092 `"Associate's degree, academic program"', add
label define educ_lbl 100 `"3 years of college"', add
label define educ_lbl 110 `"4 years of college"', add
label define educ_lbl 111 `"Bachelor's degree"', add
label define educ_lbl 120 `"5+ years of college"', add
label define educ_lbl 121 `"5 years of college"', add
label define educ_lbl 122 `"6+ years of college"', add
label define educ_lbl 123 `"Master's degree"', add
label define educ_lbl 124 `"Professional school degree"', add
label define educ_lbl 125 `"Doctorate degree"', add
label define educ_lbl 999 `"Missing/Unknown"', add
label values educ educ_lbl

label define voted_lbl 01 `"Did not vote"'
label define voted_lbl 02 `"Voted"', add
label define voted_lbl 96 `"Refused"', add
label define voted_lbl 97 `"Don't know"', add
label define voted_lbl 98 `"No Response"', add
label define voted_lbl 99 `"Not in universe"', add
label values voted voted_lbl

***
***				Now process and recode
***


keep if voted == 2

gen age_grp = ""

replace age_grp = "18-29" if age >= 18 & age <= 29  // 18-29
replace age_grp = "30-44" if age >= 30 & age <= 44  // 30-44
replace age_grp = "45-64" if age >= 45 & age <= 64  // 45-64
replace age_grp = "65+" if age >= 65              // 65+


drop age

gen educ_grp = ""

replace educ_grp = "Hs Or Less" if inlist(educ, 000, 001, 002, 010, 011, 012, 013, 014, 020, 021, 022, 030, 031, 032, 040, 050, 060, 070, 071, 072, 073)
// Grade school, some high school, high school graduate

replace educ_grp = "Some College" if inlist(educ, 080, 081, 090, 091, 092)
// Some college, vocational training/school, Associate's degree

replace educ_grp = "College" if inlist(educ, 100, 110, 111)
// 2-year college degree, 4-year college degree, Bachelor's degree

replace educ_grp = "Postgrad" if inlist(educ, 120, 121, 122, 123, 124, 125)
// Postgraduate work, Master's degree, Professional school degree, Doctorate degree



drop educ

gen race_grp = ""

replace race_grp = "White" if inlist(race, 100, 801, 802, 803, 804, 810, 811, 812, 813, 814, 816, 817, 819)
// Grouping all "White" related categories and multiracial groups involving White

replace race_grp = "Black" if inlist(race, 200, 801, 805, 806, 807, 810, 811, 814, 816, 818)
// Grouping all "Black" related categories and multiracial groups involving Black

replace race_grp = "Asian" if inlist(race, 650, 651, 803, 808, 809, 812, 813, 814, 818, 819)
// Grouping all "Asian" related categories and multiracial groups involving Asian

replace race_grp = "Hispanic" if inlist(race, 652)
// Grouping "Hawaiian/Pacific Islander" category into Hispanic/Other

replace race_grp = "Other" if inlist(race, 300, 700, 805, 806, 807, 808, 809, 815, 818, 819, 820, 830)
// Grouping all other "Other", multiracial, and unspecified categories

replace race_grp = "Hispanic" if inrange(hispan, 100, 900)


drop race hispan

gen educ_race = ""

* Non-White College
replace educ_race = "Non-White College" if race_grp != "White" & educ_grp == "College"
replace educ_race = "Non-White College" if race_grp != "White" & educ_grp == "Postgrad"

* Non-White Non-College
replace educ_race = "Non-White Non-College" if race_grp != "White" & educ_grp == "Hs Or Less"
replace educ_race = "Non-White Non-College" if race_grp != "White" & educ_grp == "Some College"

* White College
replace educ_race = "White College" if race_grp == "White" & educ_grp == "College"
replace educ_race = "White College" if race_grp == "White" & educ_grp == "Postgrad"

* White Non-College
replace educ_race = "White Non-College" if race_grp == "White" & educ_grp == "Hs Or Less"
replace educ_race = "White Non-College" if race_grp == "White" & educ_grp == "Some College"

* Recode regions into Northeast, Midwest, South, and West
gen census_region = ""

* Assign Northeast region (New England and Middle Atlantic Divisions)
replace census_region = "Northeast" if region == 11 | region == 12

* Assign Midwest region (East North Central and West North Central Divisions)
replace census_region = "Midwest" if region == 21 | region == 22

* Assign South region (South Atlantic, East South Central, and West South Central Divisions)
replace census_region = "South" if region == 31 | region == 32 | region == 33

* Assign West region (Mountain and Pacific Divisions)
replace census_region = "West" if region == 41 | region == 42

* Assign missing for state not identified
replace census_region = "" if region == 97

drop region

gen educ_sex = ""

* Non-White College
replace educ_sex = "Male College" if sex == 1 & educ_grp == "College"
replace educ_sex = "Male College" if sex == 1 & educ_grp == "Postgrad"

* Non-White Non-College
replace educ_sex = "Male Non-College" if sex == 1 & educ_grp == "Hs Or Less"
replace educ_sex = "Male Non-College" if sex == 1 & educ_grp == "Some College"

* Non-White College
replace educ_sex = "Female College" if sex == 2 & educ_grp == "College"
replace educ_sex = "Female College" if sex == 2 & educ_grp == "Postgrad"

* Non-White Non-College
replace educ_sex = "Female Non-College" if sex == 2 & educ_grp == "Hs Or Less"
replace educ_sex = "Female Non-College" if sex == 2 & educ_grp == "Some College"


save "CPSVoter.dta", replace

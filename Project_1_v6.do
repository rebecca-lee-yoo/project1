*Title: PBHLT 6101 PROJECT 1 - ARMY ANTHROPOMETRIC DATA ANALYSIS
*Author: Rebecca Yoo
*Date: 2026-02-16
*Stata Version:BE 19.5
*File Version: 1

clear
*Save working directory
cd "C:\Users\Rebecca\OneDrive - University of Utah\1_Data\4_Projects"
*Open dataset
use "C:\Users\Rebecca\OneDrive - University of Utah\1_Data\4_Projects\ansur2allV2.dta" 







**********STEP 2: DATA PREPARATION**********

*CREATE OBSERVATION ID
gen obs_id = _n



*MISSING VALUES
*Identify variables with missing values by looking at the range
codebook
*These variables have missing values: thumbtipreach span footlength kneeheightmidpatella waistheightomphalion functionalleglength cervicaleheight trochanterionheight stature waistcircumference chestcircumference bicristalbreadth hipbreadth weightkg
*Codebook showed me the range, so I can see the minimum values, ie -99, but I want to see all the negative values of each variable. Use a loop to look at all variables. Limit to looking at only numeric variables to avoid an error on "<0".
ds, has(type numeric)
foreach var of varlist `r(varlist)'{
	tab `var' if `var'<0
}
*It seems like -99 is the only negative value out of all the variables. -77 and -88 aren't recorded anywhere.
*Nevertheless, create code to change all missing values to State missing values so that the code is useful regardless of the dataset.
* New codebook:
* -77 not recorded --> .a
* -88 refused measurement --> .b
* -99 unknown missing --> .c
mvdecode _all, mv(-77 = .a \ -88 = .b \ -99 = .c)
*Check my work. Note that the tab function's default is to show non-missing values only, which is why we need to choose the missing option to tell it to show missing values.
ds, has(type numeric)
foreach var of varlist `r(varlist)'{
	tab `var' if `var'>=., missing
}
*Check my work
misstable summ


*UNREASONABLE VALUES
**Look for unreasonable values using codebook
codebook
*Looks like only weight variables are unreasonable. Create new binary variable for each of the weight variables and code 1 if unreasonable.
*
*
*
*Look at weightkg (Measured weight)
*weightkg_sus for suspicious weightkg values
*Look at boxplot and summ of weightkg to look at range
graph hbox weightkg
gen weightkg_sus = 0
replace weightkg_sus = 1 if weightkg>500
*It seems there are weights that are 10 times larger than they should be. Those values are above 500 kg. I'll replace those suspicious values with the weight divided by 10, which is more realistic
replace weightkg = weightkg/10 if weightkg_sus==1
*Check what the weights look like now.
graph hbox weightkg
*Looks much better
*
*New codebook: weightkg_sus code
*** 1 if unreasonable bc weight was higher than 500; replaced weightkg value with weightkg divided by 10
*** 0 if reasonable
*
graph hbox weightkg
*Confirm that the fixed weightkg looks similar to the self-reported value weightlbs
**Create a new var with measured weight in lbs
gen weightkg_to_lb = weightkg * 2.20462
**Calculate difference in measured and reported weight
gen weight_diff = weightkg_to_lb - weightlbs
levelsof gender
histogram weight_diff if weightkg_sus==1 & gender=="Female", frequency
histogram weight_diff if weightkg_sus==1 & gender=="Male", frequency
*Most weight difference is within an absolute value of 10 lb. Females have some differences up to about 25 lb.
*
*
*
*Look at weightlbs (reported weight)
graph hbox weightlbs
*Some weight is too low. They are suspicious.
*Create a new variable called weightlbs_sus if weightlbs is too low
gen weightlbs_sus=0
replace weightlbs_sus = 1 if weightlbs<5
replace weightlbs = .d if weightkg_sus==1
*The measured vs reported weights were different for some observations. Let's mark that as suspicious too. If the difference is more than 10 lbs, mark as suspicious.
replace weightlbs_sus = 2 if weight_diff > 10 | weight_diff < -10
*
*New codebook: weightlbs_sus
** 0 if reasonable
** 1 if unreasonable bc of low weight; weightlbs replaced with .
** 2 if suspicious bc weight difference from measured weight and self-reported weight exceeds 10 lbs; no replacement
*
*Check my work. 
*Show the values of weight and suspicious (0/1) to check if all weight>500kg is marked as suspicious (1)
list weightkg weightkg_sus if weightkg>500
*Also we can see a few weird values, but overall weightkg looks good
twoway (scatter weightlbs weightkg)



*DUPLICATES
*Find duplicates. For all variables except obs_id since obs_id is definitely going to have unique values for each row
ds obs_id, not
duplicates report `r(varlist)'
*There's no duplicates. But if there were, save it in a different file using the following code. Consider editing the code for "obs_id, not" to make sure the obs_id isn't considered
*<
*duplicates tag , generate(complete_duplicate)
*preserve
*keep if complete_duplicate > 0
*save duplicates_only.dta, replace
*restore
*>
*Are there any variables that SHOULD be unique? --> No. They are measurements or demographic data. Duplicates in these variables are possible.



*REMOVE MINORS
*Remove data on minors, who are age<18
*See how many minors there are
tab age if age<18 
*There are two minors
*Erase all data about the minors, but keep the age so that there's a breadcrumb on why all the other data is erased
*Want to use "foreach" command, but replacing with missing is different for numeric vs string variables. So, create two "foreach" commands: 1 for numeric variables which will change to ".d" and 1 for string variables which will change to ""
*Make missing for numeric variables. Since age is numeric, we need to remove that from the list of numeric variables that's being changed
**First, save all the numeric variables using ds. This saves list of numeric variables to `r(varlist)'. 
ds, has(type numeric)
**Remove age from num_vars
**Do the loop that removes all numeric data for minors but skip if the variable is age or obs_id
foreach num_var of varlist `r(varlist)' {
	if "`num_var'" == "age" continue
	if "`num_var'" == "obs_id" continue
	replace `num_var'=.d if age<18
}
*Make missing for string variables
ds, has(type string)
foreach str_var of varlist `r(varlist)' {
	replace `str_var'="" if age<18
}
*Check my work
list _all if age<18



*UNIT CONVERSIONS
*Convert measurements from mm to cm
*Find measurements that are in mm using summ
summarize
*Save list of mm variables into a macro called mm_var
local mm_var thumbtipreach span footlength kneeheightmidpatella waistheightomphalion functionalleglength cervicaleheight trochanterionheight stature waistcircumference chestcircumference bicristalbreadth hipbreadth hipbreadthsitting
*Create loop to create new variables in cm
foreach v of varlist `mm_var'{
	gen `v'_cm = `v' / 10
}
*Check my work
summ



*LABELS
label variable subjectnumericrace "Subject Numeric Race; a single or multi-digit code indicating a subject's self-reported race or races (verified through interview)."
label define subjectnumericrace_lbl 1 "White" 2 "Black" 3 "Hispanic" 4 "Asian" 5 "Native American" 6 "Pacific Islander" 8 "Other"
label values subjectnumericrace subjectnumericrace_lbl
*
label variable dodrace "Department of Defense Race; a single digit indicating a subject's self-reported preferred single race where selecting multiple races is not an option. This variable is intended to be comparable to the Defense Manpower Data Center demographic data."
label define dodrace_lbl 1 "White" 2 "Black" 3 "Hispanic" 4 "Asian" 5 "Native American" 6 "Pacific Islander" 8 "Other"
label values dodrace dodrace_lbl
*
label variable ethnicity "self-reported ethnicity (verified through interview); e.g. 'Mexican', 'Vietnamese'"
label variable gender "Male or Female"
label variable age "Participant's age in years"
label variable component "Army National Guard, Army Reserve, or Regular Army"
label variable branch "Combat Arms, Combat Support, or Combat Service Support"
label variable writingpreference "Writing Preference; Right hand, Left hand, or Either hand (No preference)"
label variable installation "U.S. Army installation where the measurement occurred; e.g. 'Fort Hood', 'Camp Shelby'"
label variable test_date "Date of test. Missing label"
**Note the variable test_date was not listed in the PDF
label variable weightlbs "Weight in Pounds; self-reported"
label variable heightin "Height in Inches; self-reported"
label variable thumbtipreach "Thumbtip Reach "
label variable span "Span"
label variable footlength "Ball of Foot Length"
label variable kneeheightmidpatella "Knee Height, Midpatella"
label variable waistheightomphalion "Waist Height (Omphalion)"
label variable functionalleglength "Functional Leg Length"
label variable cervicaleheight "cervical height, height to neck cervical"
label variable trochanterionheight "trochanterion height, height to hip"
label variable stature "Stature"
label variable waistcircumference "Waist Circumference (Omphalion)"
label variable chestcircumference "Chest Circumference"
label variable bicristalbreadth "Bicristal Breadth"
label variable hipbreadth "Hip Breadth"
label variable hipbreadthsitting "Hip Breadth, Sitting"
label variable weightkg "Weight (in kg*10)"
label variable date "Date the participant was measured, numeric date"
label variable strdate "Date the participant was measured, non-numeric date"
label variable weightkg_sus "Unlikely weightkg value"
label variable weightlbs_sus "Unlikely wieghtlbs value"
label variable obs_id "Observation ID"
label variable weightkg_to_lb "measured weight, lb"
label variable weight_diff "measured minus reported weight, lb"
label variable thumbtipreach_cm "Thumbtip Reach, cm"
label variable span_cm "Span, cm"
label variable footlength_cm "Ball of Foot Length, cm"
label variable kneeheightmidpatella_cm "Knee Height, Midpatella, cm"
label variable waistheightomphalion_cm "Waist Height (Omphalion), cm"
label variable functionalleglength_cm "Functional Leg Length, cm"
label variable cervicaleheight_cm "cervical height, height to neck cervical, cm"
label variable trochanterionheight_cm "trochanterion height, height to hip, cm"
label variable stature_cm "Stature, cm"
label variable waistcircumference_cm "Waist Circumference (Omphalion), cm"
label variable chestcircumference_cm "Chest Circumference, cm"
label variable bicristalbreadth_cm "Bicristal Breadth, cm"
label variable hipbreadth_cm "Hip Breadth, cm"
label variable hipbreadthsitting_cm "Hip Breadth, Sitting, cm"
*Check my work: Look at the variables panel



*BMI
*Create new variables for BMI: continuous and categorical variables
*First, calculate BMI using equation: BMI = 10000 * weightkg / (height_cm^2)
**Source of BMI equation: https://www.cdc.gov/growth-chart-training/hcp/using-bmi/body-mass-index.html
*Use measured values, not self-reported values. These are weightkg and stature. Stature is reported in mm
gen bmi = 10000 * weightkg / ((stature/10)^2)
label variable bmi "Body Mass Index"
*Create BMI categories: Underweight <18.5, Healthy 18.5 - <25, Overweight 25 - <30, Obese =>30
gen bmi_cat = bmi
label variable bmi_cat "BMI category"
recode bmi_cat (0/18.5 = 1) (18.5/25 = 2) (25/30 = 3) (30/300 = 4)
label define bmi_cat_lbl 1 "underweight" 2 "healthy" 3 "overweight" 4 "obese"
label values bmi_cat bmi_cat_lbl
*Check my work
tabstat bmi, stat(min max) by(bmi_cat)



*SEASON
*Use date of measurement to define season
*Season by month are Spring [Mar-May] Summer [June-Aug] Fall [Sep-Nov] Winter [Dec-Feb]. Based on https://climate.ncsu.edu/learn/seasons/
gen season = 0
label variable season "Season, based on date"
replace season = 1 if inlist(month(date), 3, 4, 5)
replace season = 2 if inlist(month(date), 6, 7, 8)
replace season = 3 if inlist(month(date), 9, 10, 11)
replace season = 4 if inlist(month(date), 12, 1, 2)
label define season_lbl 1 "spring" 2 "summer" 3 "fall" 4 "winter"
label values season season_lbl
*Check my work
gen month = month(date)
tab month season
drop month



*GENDER & HAND
*Gender & Preferred Hand: Create numerical variable
*First check what the current data looks like
levelsof gender
levelsof writingpreference
*Writingpreference has a typo called "Either han"
replace writingpreference="Either hand (No preference)" if writingpreference=="Either han"
*Check to see writing preference values look good
levelsof writingpreference
*Now, the data is clean and ready to be worked with
*Use "encode" to change string var to num var
encode gender, generate(gender_num)
encode writingpreference, generate(writing_num)
*Check my work
tab gender gender_num
tab writingpreference writing_num



*BODY TYPE
*Create: A variable that creates categories of body types based on height, weight and other measurements. Have a rationale for how you made these categories. You should have different algorithms for women and men. You should have between 3 and 6 types. Label the values of this categorical variable. 
*Think of body types: shape matters (waist-to-hip ratio). BMI is also informative, as it includes weight and height info.
*Create new variable: waist/hip measurements
**Small waist/hip ratio = hourglass shape
**Large waist/hip ratio = box-like shape
*Categories will include:
**male, female
**BMI: low, medium, high
*Total of 18 categories
*Rationale for body types: In clothing design, the waist and hip sizes are important to design for since it affects both fit and comfort. These categories are distinct from sizes. While size indicates how small or big the garment should be, body types guide the proportions of the garment to ensure it would fit and be comfortable. BMI was also included to take into account the height and weight of the population because it further stratifies the appropriate shape of the garment for different groups of people.
gen body_cat=0
gen wth_ratio = waistcircumference/hipbreadth
label variable wth_ratio "Ratio of waist circumference to hip breadth"
*
summ bmi, detail
local bmi_q1 = r(p25)
local bmi_q3 = r(p75)
*
summ wth_ratio, detail
local wth_q1 = r(p25)
local wth_q3 = r(p75)
*
**male, bmi low, wth all
replace body_cat = 1 if gender_num==1 & bmi < `bmi_q1' & wth_ratio < `wth_q1'
replace body_cat = 2 if gender_num==1 & bmi < `bmi_q1' & wth_ratio >= `wth_q1' & wth_ratio <= `wth_q3'
replace body_cat = 3 if gender_num==1 & bmi < `bmi_q1' & wth_ratio > `wth_q3'
**male, bmi medium, wth all
replace body_cat = 4 if gender_num==1 & bmi >= `bmi_q1' & bmi <= `bmi_q3' & wth_ratio < `wth_q1'
replace body_cat = 5 if gender_num==1 & bmi >= `bmi_q1' & bmi <= `bmi_q3' & wth_ratio >= `wth_q1' & wth_ratio <= `wth_q3'
replace body_cat = 6 if gender_num==1 & bmi >= `bmi_q1' & bmi <= `bmi_q3' & wth_ratio > `wth_q3'
**male, bmi high, wth all
replace body_cat = 7 if gender_num==1 & bmi > `bmi_q3' & wth_ratio < `wth_q1'
replace body_cat = 8 if gender_num==1 & bmi > `bmi_q3' & wth_ratio >= `wth_q1' & wth_ratio <= `wth_q3'
replace body_cat = 9 if gender_num==1 & bmi > `bmi_q3' & wth_ratio > `wth_q3'
**female, bmi low, wth all
replace body_cat = 10 if gender_num==2 & bmi < `bmi_q1' & wth_ratio < `wth_q1'
replace body_cat = 11 if gender_num==2 & bmi < `bmi_q1' & wth_ratio >= `wth_q1' & wth_ratio <= `wth_q3'
replace body_cat = 12 if gender_num==2 & bmi < `bmi_q1' & wth_ratio > `wth_q3'
**female, bmi medium, wth all
replace body_cat = 13 if gender_num==2 & bmi >= `bmi_q1' & bmi <= `bmi_q3' & wth_ratio < `wth_q1'
replace body_cat = 14 if gender_num==2 & bmi >= `bmi_q1' & bmi <= `bmi_q3' & wth_ratio >= `wth_q1' & wth_ratio <= `wth_q3'
replace body_cat = 15 if gender_num==2 & bmi >= `bmi_q1' & bmi <= `bmi_q3' & wth_ratio > `wth_q3'
**male, bmi high, wth all
replace body_cat = 16 if gender_num==2 & bmi > `bmi_q3' & wth_ratio < `wth_q1'
replace body_cat = 17 if gender_num==2 & bmi > `bmi_q3' & wth_ratio >= `wth_q1' & wth_ratio <= `wth_q3'
replace body_cat = 18 if gender_num==2 & bmi > `bmi_q3' & wth_ratio > `wth_q3'
*Include a missing category if bmi or wth_ratio were missing
replace body_cat = . if bmi >= . | wth_ratio >= .
*
*Check my work. See frequency of each body_cat
tab body_cat, missing
*Label
label variable body_cat "Body type category"
label define body_cat_lbl 1 "light hourglass male" 2 "light box male" 3 "light triangle male" 4 "medium hourglass male" 5 "medium box male" 6 "medium triangle male" 7 "heavy hourglass male " 8 "heavy box male" 9 "heavy triangle male" 10 "light hourglass female" 11 "light box female" 12 "light triangle female" 13 "medium hourglass female" 14 "medium box female" 15 "medium triangle female" 16 "heavy hourglass female" 17 "heavy box female" 18 "heavy triangle female"
label values body_cat body_cat_lbl
*BMI labelled as light, medium, or heavy (low, medium, high)
*Hip-to-waist ratio labelled as shapes hourglass, box, or triangle (low, medium, high)













************ STEP 3 - SAMPLE CHARACTERISTICS ***************

*ANTHROPOMETRIC CHARACTERISTICS
*a) Generate statistics describing the anthropometric measures of the sample. Summarize measures for the entire sample in a table. For clarity organize the table by body part (like it is organized above). Note the number of missing values and suspicious values for each variable. This can be put in a separate table if you prefer. This information will be important for your data quality assessment (3.2 below). 
*Create list of variables and descriptive statistics
summ
*Copy table to excel and clean
*Include only anthropometric measures. Delete non-anthropometric variables
*
ds has(type numeric)
tabstat `r(varlist)', stat(n mean sd min max)
*
*Count missing values
misstable summarize
**The output shows types of missing values. Create a new column that uses sum() to find total # of missing values
**Sort the variable names to quickly copy over information of missing values
**Create a new column called "Missing values (n)" and copy missing values
*
*Count suspicious values
**Using excel, create a new column to write the code below. The column's equation is: =concat("graph hbox ", <insert variable name cell>). Drag the equation down. Copy and paste that to below.
**Inspect each 
*
*Create varlist of anthropometric measures that will be used temporarily for the loop
local anthro_vars weightkg bicristalbreadth_cm chestcircumference_cm hipbreadthsitting_cm hipbreadth_cm waistcircumference_cm cervicaleheight_cm footlength_cm functionalleglength_cm kneeheightmidpatella_cm span_cm stature_cm thumbtipreach_cm trochanterionheight_cm waistheightomphalion_cm
*Define what "suspicious" means. I'm deciding that outliers on a box plot will be considered suspicious.
*Create loop that will code a missing value for every outlier. I chose missing values instead of 1/0 so that I can use misstable for efficient summary. New variables called var_susp is created for each anthropometric variable
foreach v of varlist `anthro_vars'{
	summ `v', detail
	local q1 = r(p25)
	local q3 = r(p75)
	local iqr = `q3' - `q1'
	*find number of suspicious values in variable
	gen `v'_susp = 99
	replace `v'_susp = .s if `v' < (`q1' - 1.5 * `iqr')
	replace `v'_susp = .t if `v' > (`q3' + 1.5 * `iqr')
}
*Use misstable to find how many missing values (suspicious values) are in each variable
misstable summarize
*Copy table in excel and transfer the missing values into the main table. Label the column: Suspicious values (n)
*
*Code for if I want to drop all the var_susp variables
*foreach v of varlist `anthro_vars'{
*	drop `v'_susp
*}
**
*Organize the variables by body part. To do this, create a new column called "Code for organization" and code it 1-General, 2-MEASURES OF TRUNK CIRCUMFERENCE, and 3-MEASURES OF STATURE. Sort by this code to get the table organized for publication.



*HIP HEIGHT PROPORTION
*Create a figure that shows the differences in % of total height attributable to the height to hip by sex.
**Create a new variable to calculate hip height / total height. Multiply by 100 for percentage.
gen hip_over_stature = trochanterionheight / stature * 100
*Create box plot to show difference in the proportions between male and female
graph hbox hip_over_stature, over(gender)
bysort gender: summarize hip_over_stature, detail
*Description: 
*The percent of total height attributable to the hip height describes, roughly, the proportional height of the lower body to the total height. In plain terms, individuals with longer legs are expected to have a higher percent than individuals with shorter legs. In the figure, we observe females having a higher percent than males, indicating that females have proportionally taller lower bodies than males. The median percentage for females and males is 51.8% and 51.2%, respectively. The median proportional height of the lower body to the full height is 0.6% higher in females than for males in the sample group.





*3.2 Data quality assessment
****************HEEEEELLLLLLLPPPPPPPPPPP*********************
*In less than a page, describe data quality issues that you found and how they were dealt with.   









************ STEP 4 - RELATIONSHIPS BW ANTHROPOMETRIC MEASURES ***************

*RELATIONSHIP BETWEEN MEASURES OF STATURE
*a) correlation between the measures of stature (SEE ABOVE TABLE).  Identify the two measures that are most highly correlated and provide a figure that demonstrates the relationship between these measures. Describe in a short paragraph.
pwcorr kneeheightmidpatella cervicaleheight trochanterionheight waistheightomphalion functionalleglength footlength thumbtipreach span, sig
*Look for variables with highest correlation coefficients
*Highest correlation coeff are with: waistheightomphalion cervicaleheight
*Create fgigure that demonstrates relationship bw these variables
twoway (scatter waistheightomphalion cervicaleheight) (lowess waistheightomphalion cervicaleheight)
regress waistheightomphalion cervicaleheight
*The slope of the linear regression is less than 0. Inverting the slope would make the relationship easier to discuss. Switch x and y variables
regress cervicaleheight waistheightomphalion
twoway (scatter cervicaleheight waistheightomphalion) (lfit cervicaleheight waistheightomphalion)
*Description paragraph: The correlation coefficients for measures of stature variables range between 0.15 and 0.94 and every p-value is less than 0.0001. The two variables with the highest correlation coefficient are waistheightomphalion and cervicaleheight. When fit with a linear regression, the regression shows that for every unit increase of waistheightomphalion, there is a 1.26 unit increase in cervicaleheight. Although both variables are provided in units of mm, the relationship is holds in inches also. In other words, for every inch increase of height to the hip, there is 1.26 inches of increase height to the neck cervical. The R-squared value is 0.8852 for the linear regression.



*RELATIONSHIP BETWEEN HEIGHT AND OTHER VARIABLES, STRATIFIED BY SEX
*c) Assess the level of correlation between overall height (stature) and the other measures of stature, stratified by sex. Which measure(s) of stature are highly correlated with overall height (variable stature)?  Describe the results and differences between women and men in a table and paragraph.
*Assess the level of correlation between overall height (stature) and the other measures of stature, stratified by sex
bysort gender: pwcorr stature kneeheightmidpatella cervicaleheight trochanterionheight waistheightomphalion functionalleglength footlength thumbtipreach span, sig
*Output: Female - Correlation Coeff = 0.9845 for cervicaleheight
*        Male   - Correlation Coeff = 0.9840 for cervicaleheight
*Copy the output into an excel sheet. Delete irrelevant data (the columns right of stature) and organize by males and females into a single table.
*The variables with the three highest correlation coefficients and the two lowest correlation coefficients are the same for both males and females. The variables with the highest correlation coefficients (reported in parenthesis as female and male, respectively) are height to neck cervical (0.9845, 0.9840), waist height (0.9097, 0.9074), and height to hip(0.8586, 0.8527). The two variables with the lowest correlation coefficients are forward arm reach and foot length. The variable with the third lowest correlation coefficient is arm span for females and functional leg length for males. The correlation coefficient is consistently higher for females than males except for arm span, which has a correlation coefficient that is 0.24 higher for males than that of females.



*RELATIONSHIP BETWEEN ACTUAL AND REPORTED, WEIGHT
*a) Compare the self-reported weight to the measured weight. Describe the distribution of the difference and provide a figure that demonstrates how these two measures differ. 1 fig
*A variable for weight difference was already created, called weight_diff = measured - reported
histogram weight_diff
*This histogram doesn't look good.
*Try removing empty values
histogram weight_diff if weightlbs < . & weightkg < ., percent
*This still isn't helpful.
*Look at a box plot instead.
graph hbox weight_diff
summ weight_diff, detail
*The difference is the measured weight (weightkg) minus the reported weight (weightlbs). The interquartile range is from -2.39 to 3.66 with the median at 0.52, showing that typically, the reported weight is lower than the measured weight by 0.52 lbs. The 10th and 90th percentiles are -5.6 and 7.4, respectively, while the range is -145.8 to 194.4. This shows that most weight differences are reasonable and there were some outliers that are obviously incorrect.
*If we want to look at reasonable values only, we would limit the range to a weight difference of 10 lbs.
*Recall codebook: weightlbs_sus
** 0 if reasonable
** 1 if unreasonable bc of low weight; weightlbs replaced with .
** 2 if suspicious bc weight difference from measured weight and self-reported weight exceeds 10 lbs; no replacement <-- This is the important line
*Recall codebook: weightkg_sus code
*** 0 if reasonable
*** 1 if unreasonable bc weight was higher than 500; replaced weightkg value with weightkg divided by 10
histogram weight_diff if weightlbs_sus != 2 , percent
*This histogram shows a better distribution of the majority of the observations.



*b) Show the extent to which the difference between reported and measured weight varies by sex. 1 fig
*Recall this was partially done when cleaning data.
*Look at box plot
graph hbox weight_diff if gender=="Female", name(female_wt_diff_box)
graph hbox weight_diff if gender=="Male", name(male_wt_diff_box)
*It doesn't look great. Let's look at summ to get some useful data
summ weight_diff if gender=="Female", detail
*IQR is -1.2 to 4.1 with median at 1.2
summ weight_diff if gender=="Male", detail
*IQR is -3.0 to 3.3 with median at 0.1
*This shows that males generally had a larger range of differences but females overall had more of a difference in measured vs reported weight.
*If we want to look at reasonable values only, we would limit the range to a weight difference of 10 lbs.
histogram weight_diff if weightlbs_sus != 2 & gender=="Female", percent
histogram weight_diff if weightlbs_sus != 2 & gender=="Male", percent
*This histogram shows a better distribution of the majority of the observations.


*RELATIONSHIP BETWEEN ACTUAL AND REPORTED, HEIGHT
*c) Compare the self-reported height to the measured height. Describe the distribution of the difference and provide a figure that demonstrates how these two measures differ. 1 fig
*Look at existing data
twoway (scatter stature heightin)
*Stature is reported in mm. Change to inches to be comparable with heightin
gen staturein = stature * 0.0393701
*Check my work to see if stature was converted properly
twoway (scatter staturein heightin)
*This looks good
*Look at individual variables
graph hbox staturein heightin
*The self-reported height has more outliers than the measured height.
*
*Calculate the difference. Difference = reported - measured
gen height_diff = staturein - heightin
*Look at distribution
histogram height_diff, percent
*This looks bad because of outliers
*Look at boxplot
graph hbox height_diff
*This mainly shows all the outliers
*Use summ to get more useful descriptive data
summ height_diff, detail
*Description: The distribution of the difference is a tall and narrow normal curve, showing that most heights are generally precise within a certain range. The IQR ranges from -1.2 to -0.24, showing that the measured height is usually up to 1 inch higher than the reported height. The 10th and 90th percentiles are -1.7 and -0.2, respectively, while the range is from -24.1 to 10.8.



*d) Summarize the findings for 4.2 a-c in a paragraph. Discuss in terms of the bias associated with using reported weight and height in lieu of measuring weight and height.




*4.3 Relationship between derived `body type' and weight/BMI
*a) Compare the distributions of weight and BMI for each of the body types you created in 1e above. Describe your findings in a paragraph with a figure and/or table. Focus on how well your body types describe differences in weight and obesity.
heatplot bmi weightkg_to_lb if gender_num==1 , by(body_cat, compact)
heatplot bmi weightkg_to_lb if gender_num==2 , by(body_cat, compact)

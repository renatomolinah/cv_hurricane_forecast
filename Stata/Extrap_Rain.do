/******************************  METADATA *********************************
Replication for Molina, Letson, McNoldy, Mozumder, and Varkony, 2021.
"Striving for Improvement: The Perceived Value of Improving Hurricane Forecast Accuracy".
Script Description: This STATA script generates all extrapolations of WTP for Precipitation in the main paper. Code developed for STATA v16.
Date: February, 2021
**************************************************************************/

#delimit ;
clear all;
set more off;

/****************************** Setup *********************************/;

use "YOUR_DATA_FOLDER/data.dta";

cd "YOUR_TABLE_FOLDER/Tables";

encode hurricane, gen (hurr);

global X0 rain_bid1 rain_bid2 rain_answer1 rain_answer2 rain_order rain_rate;

global X1 income female experience evacuate voice action long_risk;

global X2 age owner tenure short_risk hurricane_awareness fema_awareness
	nfip_awareness damage hh_size  c_dist;
	
/****************************** WTP calculation *********************************/;

* Exposed 20mph;

quietly doubleb $X0 $X1 $X2
	florence;
	estadd local pref "X";
	estadd local add "X";

summarize rain_order, meanonly;
scalar rain_order_m = r(mean);
	
summarize female20, meanonly;
scalar female_m20 = r(mean);

summarize experience, meanonly;
scalar experience_m = r(mean);	

summarize evacuate, meanonly;
scalar evacuate_m = r(mean);

summarize voice, meanonly;
scalar voice_m = r(mean);

summarize action, meanonly;
scalar action_m = r(mean);
	
eststo wtp13_20:quietly nlcom 
	(WTP:(rain_order_m*_b[rain_order] 
		+ female_m20*_b[female] 
		+ experience_m*_b[experience] 
		+ evacuate_m*_b[evacuate] 
		+ voice_m*_b[voice]
		+ action_m*_b[action]
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "X";
	

* Exposed 30mph;

quietly doubleb $X0 $X1 $X2
	florence;
	estadd local pref "X";
	estadd local add "X";

summarize rain_order, meanonly;
scalar rain_order_m = r(mean);
	
summarize female30, meanonly;
scalar female_m30 = r(mean);

summarize experience, meanonly;
scalar experience_m = r(mean);	

summarize evacuate, meanonly;
scalar evacuate_m = r(mean);

summarize voice, meanonly;
scalar voice_m = r(mean);

summarize action, meanonly;
scalar action_m = r(mean);
	
eststo wtp13_30:quietly nlcom 
	(WTP:(rain_order_m*_b[rain_order] 
		+ female_m30*_b[female] 
		+ experience_m*_b[experience] 
		+ evacuate_m*_b[evacuate] 
		+ voice_m*_b[voice]
		+ action_m*_b[action]
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "X";
	
* Exposed 40mph;

quietly doubleb $X0 $X1 $X2
	florence;
	estadd local pref "X";
	estadd local add "X";

summarize rain_order, meanonly;
scalar rain_order_m = r(mean);
	
summarize female40, meanonly;
scalar female_m40 = r(mean);

summarize experience, meanonly;
scalar experience_m = r(mean);	

summarize evacuate, meanonly;
scalar evacuate_m = r(mean);

summarize voice, meanonly;
scalar voice_m = r(mean);

summarize action, meanonly;
scalar action_m = r(mean);
	
eststo wtp13_40:quietly nlcom 
	(WTP:(rain_order_m*_b[rain_order] 
		+ female_m40*_b[female] 
		+ experience_m*_b[experience] 
		+ evacuate_m*_b[evacuate] 
		+ voice_m*_b[voice]
		+ action_m*_b[action]
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "X";
		
* Exposed 50mph;

quietly doubleb $X0 $X1 $X2
	florence;
	estadd local pref "X";
	estadd local add "X";

summarize rain_order, meanonly;
scalar rain_order_m = r(mean);
	
summarize female50, meanonly;
scalar female_m50 = r(mean);

summarize experience, meanonly;
scalar experience_m = r(mean);	

summarize evacuate, meanonly;
scalar evacuate_m = r(mean);

summarize voice, meanonly;
scalar voice_m = r(mean);

summarize action, meanonly;
scalar action_m = r(mean);
	
eststo wtp13_50:quietly nlcom 
	(WTP:(rain_order_m*_b[rain_order] 
		+ female_m50*_b[female] 
		+ experience_m*_b[experience] 
		+ evacuate_m*_b[evacuate] 
		+ voice_m*_b[voice]
		+ action_m*_b[action]
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "X";
			
		
/****************************** Output *********************************/;	
		
esttab wtp13 wtp13_20 wtp13_30 wtp13_40 wtp13_50 
	,
	se(%3.2f) b(2) label 
	mgroups("Sample" "20 mph" "30 mph" "40 mph" "50 mph", pattern(1 1 1 1 1))
	nonum nomtitle
	scalars(
		"pref Control Set 1" 
		"add Control Set 2");		
		
esttab wtp13 wtp13_20 wtp13_30 wtp13_40 wtp13_50 
	using "extrap_rain.tex", replace
	se(%3.2f) b(2) label 
	mgroups("Sample" "20 mph" "30 mph" "40 mph" "50 mph", pattern(1 1 1 1 1))
	nonum nomtitle
	scalars(
		"pref Control Set 1" 
		"add Control Set 2");		
		

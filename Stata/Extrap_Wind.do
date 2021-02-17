/******************************  METADATA *********************************
Replication for Molina, Letson, McNoldy, Mozumder, and Varkony, 2021.
"Striving for Improvement: The Perceived Value of Improving Hurricane Forecast Accuracy".
Script Description: This STATA script generates all extrapolations of WTP for Wind Speed in the main paper. Code developed for STATA v16.
Date: February, 2021
**************************************************************************/

#delimit ;
clear all;
set more off;

/****************************** Setup *********************************/;

global X0 wind_bid1 wind_bid2 wind_answer1 wind_answer2 wind_order wind_rate;

global X1 income female experience evacuate voice action long_risk;

global X2 age owner tenure short_risk hurricane_awareness fema_awareness
	nfip_awareness damage hh_size  c_dist;
	
	
/****************************** WTP calculation *********************************/;	

* Sample;

quietly doubleb $X0 $X1 $X2
	florence;
	estadd local pref "X";
	estadd local add "X";

summarize wind_order, meanonly;
scalar wind_order_m = r(mean);
	
summarize income, meanonly;
scalar income_m = r(mean);

summarize experience, meanonly;
scalar experience_m = r(mean);	

summarize evacuate, meanonly;
scalar evacuate_m = r(mean);

summarize voice, meanonly;
scalar voice_m = r(mean);

summarize action, meanonly;
scalar action_m = r(mean);

summarize long_risk, meanonly;
scalar long_risk_m = r(mean);

eststo wtp13:quietly nlcom 
	(WTP:(wind_order_m*_b[wind_order] 
		+ income_m*_b[income] 
		+ experience_m*_b[experience] 
		+ evacuate_m*_b[evacuate] 
		+ voice_m*_b[voice] 
		+ action_m*_b[action] 
		+ long_risk_m*_b[long_risk]
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "X";				
	
* Exposed 20mph;

quietly doubleb $X0 $X1 $X2
	florence;
	estadd local pref "X";
	estadd local add "X";

summarize wind_order, meanonly;
scalar wind_order_m = r(mean);
	
summarize income20, meanonly;
scalar income_m20 = r(mean);

summarize experience, meanonly;
scalar experience_m = r(mean);	

summarize evacuate, meanonly;
scalar evacuate_m = r(mean);

summarize voice, meanonly;
scalar voice_m = r(mean);

summarize action, meanonly;
scalar action_m = r(mean);

summarize long_risk, meanonly;
scalar long_risk_m = r(mean);

eststo wtp13_20:quietly nlcom 
	(WTP:(wind_order_m*_b[wind_order] 
		+ income_m20*_b[income] 
		+ experience_m*_b[experience] 
		+ evacuate_m*_b[evacuate] 
		+ voice_m*_b[voice] 
		+ action_m*_b[action] 
		+ long_risk_m*_b[long_risk]
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "X";
	

* Exposed 30mph;

quietly doubleb $X0 $X1 $X2
	florence;
	estadd local pref "X";
	estadd local add "X";

summarize wind_order, meanonly;
scalar wind_order_m = r(mean);
	
summarize income30, meanonly;
scalar income_m30 = r(mean);

summarize experience, meanonly;
scalar experience_m = r(mean);	

summarize evacuate, meanonly;
scalar evacuate_m = r(mean);

summarize voice, meanonly;
scalar voice_m = r(mean);

summarize action, meanonly;
scalar action_m = r(mean);

summarize long_risk, meanonly;
scalar long_risk_m = r(mean);

eststo wtp13_30:quietly nlcom 
	(WTP:(wind_order_m*_b[wind_order] 
		+ income_m30*_b[income] 
		+ experience_m*_b[experience] 
		+ evacuate_m*_b[evacuate] 
		+ voice_m*_b[voice] 
		+ action_m*_b[action] 
		+ long_risk_m*_b[long_risk]
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "X";
	
* Exposed 40mph;

quietly doubleb $X0 $X1 $X2
	florence;
	estadd local pref "X";
	estadd local add "X";

summarize wind_order, meanonly;
scalar wind_order_m = r(mean);
	
summarize income40, meanonly;
scalar income_m40 = r(mean);

summarize experience, meanonly;
scalar experience_m = r(mean);	

summarize evacuate, meanonly;
scalar evacuate_m = r(mean);

summarize voice, meanonly;
scalar voice_m = r(mean);

summarize action, meanonly;
scalar action_m = r(mean);

summarize long_risk, meanonly;
scalar long_risk_m = r(mean);

eststo wtp13_40:quietly nlcom 
	(WTP:(wind_order_m*_b[wind_order] 
		+ income_m40*_b[income] 
		+ experience_m*_b[experience] 
		+ evacuate_m*_b[evacuate] 
		+ voice_m*_b[voice] 
		+ action_m*_b[action] 
		+ long_risk_m*_b[long_risk]
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "X";
		
* Exposed 50mph;

quietly doubleb $X0 $X1 $X2
	florence;
	estadd local pref "X";
	estadd local add "X";

summarize wind_order, meanonly;
scalar wind_order_m = r(mean);
	
summarize income50, meanonly;
scalar income_m50 = r(mean);

summarize experience, meanonly;
scalar experience_m = r(mean);	

summarize evacuate, meanonly;
scalar evacuate_m = r(mean);

summarize voice, meanonly;
scalar voice_m = r(mean);

summarize action, meanonly;
scalar action_m = r(mean);

summarize long_risk, meanonly;
scalar long_risk_m = r(mean);

eststo wtp13_50:quietly nlcom 
	(WTP:(wind_order_m*_b[wind_order] 
		+ income_m50*_b[income] 
		+ experience_m*_b[experience] 
		+ evacuate_m*_b[evacuate] 
		+ voice_m*_b[voice] 
		+ action_m*_b[action] 
		+ long_risk_m*_b[long_risk]
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
	using "extrap_wind.tex", replace
	se(%3.2f) b(2) label 
	mgroups("Sample" "20 mph" "30 mph" "40 mph" "50 mph", pattern(1 1 1 1 1))
	nonum nomtitle
	scalars(
		"pref Control Set 1" 
		"add Control Set 2");		
		

/******************************  METADATA *********************************
Replication for Molina, Letson, McNoldy, Mozumder, and Varkony, 2021.
"Striving for Improvement: The Perceived Value of Improving Hurricane Forecast Accuracy".
Script Description: This STATA script generates all Wind Speed estimates in the main paper. Code developed for STATA v16.
Date: February, 2021
**************************************************************************/

#delimit ;
clear all;
set more off;

/****************************** Setup *********************************/;

use "YOUR_DATA_FOLDER/data.dta";

cd "YOUR_TABLE_FOLDER/Tables";

destring, replace;

encode hurricane, gen (hurr);

global X0 wind_bid1 wind_bid2 wind_answer1 wind_answer2 wind_order wind_rate;

global X1 income female experience evacuate voice action long_risk;

global X2 age owner tenure short_risk hurricane_awareness fema_awareness
	nfip_awareness damage hh_size c_dist;
	
/******************************  Analysis *********************************/;		

* BOTH STORMS;

eststo r01:quietly doubleb wind_bid1 wind_bid2 wind_answer1 wind_answer2 
	;
	estadd local pref "";
	estadd local add "";

* Plain;

eststo r11:quietly doubleb $X0
	florence;
	estadd local pref "";
	estadd local add "";
	
summarize wind_order, meanonly;
scalar wind_order_m = r(mean);
	
eststo wtp11:quietly nlcom 
	(WTP:(_b[_cons] 
		+ wind_order_m*_b[wind_order]
		)),
	post noheader;
	estadd local pref "";
	estadd local add "";	
	
* Control set 1;

eststo r12:quietly doubleb $X0 $X1
	florence;
	estadd local pref "X";
	estadd local add "";

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
	
eststo wtp12:quietly nlcom 
	(WTP:(wind_order_m*_b[wind_order] 
		+ income_m*_b[income] 
		+ experience_m*_b[experience] 
		+ evacuate_m*_b[evacuate] 
		+ voice_m*_b[voice] 
		+ action_m*_b[action]
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "";	
	
* Control set 1 + Control set 2;

eststo r13:quietly doubleb $X0 $X1 $X2
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
	
	
* FLORENCE;

eststo r02:quietly doubleb wind_bid1 wind_bid2 wind_answer1 wind_answer2 
	if hurr == 1;
	estadd local pref "";
	estadd local add "";

* Plain;

eststo r21:quietly doubleb $X0
	if hurr == 1;
	estadd local pref "";
	estadd local add "";
	
summarize wind_order
	if hurr == 1, meanonly;
scalar wind_order_m = r(mean);
	
eststo wtp21:quietly nlcom 
	(WTP:(_b[_cons] 
		+ wind_order_m*_b[wind_order]
		)),
	post noheader;
	estadd local pref "";
	estadd local add "";	
	
* Control set 1;

eststo r22:quietly doubleb $X0 $X1
	if hurr == 1;
	estadd local pref "X";
	estadd local add "";

summarize wind_order
	if hurr == 1, meanonly;
scalar wind_order_m = r(mean);
	
summarize income
	if hurr == 1, meanonly;
scalar income_m = r(mean);

summarize experience
	if hurr == 1, meanonly;
scalar experience_m = r(mean);	

summarize evacuate
	if hurr == 1, meanonly;
scalar evacuate_m = r(mean);	

summarize voice
	if hurr == 1, meanonly;
scalar voice_m = r(mean);

summarize action
	if hurr == 1, meanonly;
scalar action_m = r(mean);

eststo wtp22:quietly nlcom 
	(WTP:(wind_order_m*_b[wind_order] 
		+ income_m*_b[income] 
		+ experience_m*_b[experience]
		+ evacuate_m*_b[evacuate]
		+ voice_m*_b[voice] 
		+ action_m*_b[action] 
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "";	
	
* Control set 1 + Control set 2;

eststo r23:quietly doubleb $X0 $X1 $X2
	if hurr == 1;
	estadd local pref "X";
	estadd local add "X";

summarize wind_order
	if hurr == 1, meanonly;
scalar wind_order_m = r(mean);
	
summarize income
	if hurr == 1, meanonly;
scalar income_m = r(mean);

summarize experience
	if hurr == 1, meanonly;
scalar experience_m = r(mean);

summarize evacuate
	if hurr == 1, meanonly;
scalar evacuate_m = r(mean);	

summarize voice
	if hurr == 1, meanonly;
scalar voice_m = r(mean);

summarize action
	if hurr == 1, meanonly;
scalar action_m = r(mean);

summarize long_risk
	if hurr == 1, meanonly;
scalar long_risk_m = r(mean);

	
eststo wtp23:quietly nlcom 
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
	
* MICHAEL;

eststo r03:quietly doubleb wind_bid1 wind_bid2 wind_answer1 wind_answer2 
	if hurr == 2;
	estadd local pref "";
	estadd local add "";

* Plain;

eststo r31:quietly doubleb $X0
	if hurr == 2;
	estadd local pref "";
	estadd local add "";
	
summarize wind_order
	if hurr == 2, meanonly;
scalar wind_order_m = r(mean);
	
eststo wtp31:quietly nlcom (
	WTP:(_b[_cons] 
		+ wind_order_m*_b[wind_order])),
	post noheader;
	estadd local pref "";
	estadd local add "";	
	
* Control set 1;

eststo r32:quietly doubleb $X0 $X1
	if hurr == 2;
	estadd local pref "X";
	estadd local add "";

summarize wind_order
	if hurr == 2, meanonly;
scalar wind_order_m = r(mean);
	
summarize experience
	if hurr == 2, meanonly;
scalar experience_m = r(mean);

summarize voice
	if hurr == 2, meanonly;
scalar voice_m = r(mean);

summarize action
	if hurr == 2, meanonly;
scalar action_m = r(mean);
	
eststo wtp32:quietly nlcom 
	(WTP:(wind_order_m*_b[wind_order] 
		+ experience_m*_b[experience]   
		+ voice_m*_b[voice] 
		+ action_m*_b[action] 
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "";	
	
* Control set 1 + Control set 2;

eststo r33:quietly doubleb $X0 $X1 $X2
	if hurr == 2;
	estadd local pref "X";
	estadd local add "X";

summarize wind_order
	if hurr == 2, meanonly;
scalar wind_order_m = r(mean);
	
summarize experience
	if hurr == 2, meanonly;
scalar experience_m = r(mean);

summarize voice
	if hurr == 2, meanonly;
scalar voice_m = r(mean);

summarize action
	if hurr == 2, meanonly;
scalar action_m = r(mean);

summarize owner
	if hurr == 2, meanonly;
scalar owner_m = r(mean);
	
eststo wtp33:quietly nlcom 
	(WTP:(wind_order_m*_b[wind_order]  
		+ experience_m*_b[experience]
		+ voice_m*_b[voice]
		+ action_m*_b[action]
		+ owner_m*_b[owner]
			)),
			post noheader;
	estadd local pref "X";
	estadd local add "X";	
	
/****************************** Output *********************************/;		
	
* Labels;

* Control set 1;

label variable wind_order "Order";
label variable wind_rate "Rate";		
label variable income "Florence";
label variable income "Income";
label variable female "Female";
label variable experience "Experience";
label variable evacuate "Evacuated";
label variable voice "Voice";
label variable action "Action";
label variable c_dist "Coast Distance";

* Control set 2;

label variable age "Age";
label variable owner "Owner";
label variable tenure "Household Tenure";
label variable short_risk "Short Risk Perception";
label variable long_risk "Long Risk Perception";
label variable hurricane_awareness "Hurricane Insurance Awareness";
label variable fema_awareness "FEMA Program Awareness";
label variable nfip_awareness "NFIP Insurance awareness";
label variable damage "Damage";
label variable hh_size "Household Size";				
	
* Regression tables;

esttab  r11 r12 r13 r21 r22 r23 r31 r32 r33
	using "wind.tex", replace keep(_cons wind_order wind_rate $X1)
	se(%3.2f) b(2) label
	mgroups("Full Sample" "Florence" "Michael", pattern(1 0 0 1 0 0 1)
		prefix(\multicolumn{@span}{c}{)
		suffix(}) span )
	nonum nomtitle 
	scalars(
		"ll Log-Likelihood"
		"chi2 $\chi^2$"
		"add Control Set 2");

esttab  r11 r12 r13 r21 r22 r23 r31 r32 r33
	using "full_wind.tex", replace
	se(%3.2f) b(2) label 
	mgroups("Full Sample" "Florence" "Michael", pattern(1 0 0 1 0 0 1)
		prefix(\multicolumn{@span}{c}{)
		suffix(}) span )
	nonum nomtitle 
	scalars(
		"ll Log-Likelihood"
		"chi2 $\chi^2$");		
		
* WTP tables;		
		
esttab wtp11 wtp12 wtp13 wtp21 wtp22 wtp23 wtp31 wtp32 wtp33
	,
	se(%3.2f) b(2) label 
	mgroups("Full Sample" "Florence" "Michael", pattern(1 0 0 1 0 0 1))
	nonum nomtitle
	scalars(
		"pref Control Set 1" 
		"add Control Set 2");					
		
esttab wtp11 wtp12 wtp13 wtp21 wtp22 wtp23 wtp31 wtp32 wtp33
	using "wtp_wind.tex", replace
	se(%3.2f) b(2) label 
	mgroups("Full Sample" "Florence" "Michael", pattern(1 0 0 1 0 0 1)
		prefix(\multicolumn{@span}{c}{)
		suffix(}) span )
	nonum nomtitle
	scalars(
		"pref Control Set 1" 
		"add Control Set 2");

//import netwrok data, risk factors, google trends data, and gnosis prive/volume data
cd "C:\Users\fs00675\OneDrive - University of Surrey\Documents\Gnosis Project"
local files : dir "." files "*.csv"
foreach file of local files {
    display "Importing `file'..."
    import delimited "`file'", clear
	local filename = subinstr("`file'", ".csv", "", .) 
    save "`filename'.dta", replace
}


//table of descriptive statistics for price and volume



// LOAD the Data
use "GNO_price", clear
describe

drop conversionSymbol conversionType time

gen date=dofc(timestamp) 

keep close date volumefrom volumeto 

gen cur1="GNO"

tsset date // classify dataset as time series
* calculate returns
gen ret_gno = close/L.close-1 // generate returns in levels

gen ret_gnol=log(close)-log(L.close) // generate returns in logs

label var ret_gno "GNO Return" // labeling variables
label var close "GNO close price"

local retb "ret_gno" // local reference 


save GNOp.dta, replace

tabulate cur1 
tab cur1

summarize `retb' 
su ret_gno* volumefrom

	
********************************************
*Summary Statistics (Descriptive Statistics)

*We usually tend to show mean, s.d and N (observations) to get a general idea about the sample we are dealing with.
ssc install estout


eststo clear // to clear previously estimated standard deviations etc. 
*(qui)etly
eststo: quietly estpost su 


esttab using example.csv, cells("mean sd") label nodepvar replace 
  
*Ploting Figures
********************************************

* Generate a line plot
* Generate a line plot
graph twoway tsline close

* Plot figure of prices between two dates
local intdate = td(1Apr2017)
dis `intdate'
local findate = td(01Apr2024)
dis `findate'

* Generate a line plot with less cramped x-axis labels
graph twoway tsline close if date > `intdate' & date < `findate', ///
    tlabel(`intdate'(366)`findate', format(%tdMon_CCYY)) /// // Display labels every year
    ytitle("Price (USD)") ///
    xtitle("Date") ///
    title("Gnosis Price Over Time") ///
    legend(label(1 "GNO"))
   
* Save the graph as a PNG file
graph export "GNOprice.png", replace

//histogram
local retb "ret_gno" 
histogram `retb', title("Histogram of Returns")
graph export "retGNOhist.png", replace
// More complicated histogram with truncation
local cutoff=0.25
hist `retb' if `retb' > -`cutoff' & `retb' < `cutoff', ///
    xtitle("Return") ///
    saving(hist0, replace) ///
    title("Distribution of Gnosis Returns") /// Title for the complicated histogram
    width(0.01) ///
    xsc(r(-`cutoff' `cutoff')) ///
    xlabel(-`cutoff'(0.2)`cutoff') ///
    ylabel(0(50)350) ///
    ysc(r(0 350)) ///
    graphregion(style(none) color(gs16)) ///
    frequency normal

********************************************
*DEALING WITH OUTLIERS






//





********************************************************************************
*3) MERGING
********************************************************************************

* a) Network data

// Merge network data file
import delimited "gno", clear
br
/* We are interested in the following series:
number of wallets,
number of active addresses,
number of transaction counts,
number of payment counts (and many others)
*/


keep time adrbalcnt adractcnt txcnt txtfrvaladjntv 
label var adrbalcnt "number of wallets with positive balance"
label var adractcnt "number of active addresses"
label var txcnt "number of transaction counts"
label var txtfrvaladjntv "number of payment counts"


eststo clear
local yvar "ret_gno"
local k=1
cap gen lagvar=`yvar'

foreach i of varlist ret_gno {
   forvalues j=1/7 {
       replace lagvar=L`j'.`i'

       eststo: qui reg `i' lagvar adrbalcnt adractcnt txcnt txtfrvaladjntv
   }
}

label var adrbalcnt "number of wallets with positive balance"
label var adractcnt "number of active addresses"
label var txcnt "number of transaction counts"
label var txtfrvaladjntv "number of payment counts"

esttab, r2(4) ar2(4) label star(* 0.1) ///
   mtitles("Return t+1" "Return t+2" "Return t+3" "Return t+4" "Return t+5" "Return t+6" "Return t+7") ///
   replace




gen date = date(time,"YMD") // to change the year-month-date into a stata date, so that we can merge datasets 
drop time
tempfile tmpfile
save "`tmpfile'"
merge 1:1 date using GNOp.dta // this one would

rename _merge _merge1
save GNOfinal.dta, replace
//a traditional
use "FamaFrench5factors - copy", clear
br
rename date date_str
gen date_numeric = date(date, "YMD")
rename date_numeric date

rename mktrf market_risk_premium
rename smb size_factor
rename hml value_factor
rename rmw profitability_factor
rename cma investment_factor
rename rf risk_free_rate
drop umd
save "FamaFrench5factors - copy", replace 

use "GNOfinal.dta", clear
merge 1:1 date using "FamaFrench5factors - copy"
rename _merge _merge2
save GNOfinal.dta, replace

br
* b) CRYPTO FACTORs

use "factor_size - copy.dta", clear

br

*This command tells Stata to list the data, but only for observations in rows 1 through 5.
list in 1/5

rename date date_str
*Make sure that dates variable in proper Stata format
gen date = date(date_str,"DMY")
br
* Rename varibles
rename (p1 p2 p3) (f_size1 f_size2 f_size3)

* Labeling variables. You can do forval and foreach loops in stata
forval i=1/3{
	label var f_size`i' "Crypto Size factor, portfolio `i'" 
}

save "factor_size - copy.dta", replace

tempfile tmpfile
save "`tmpfile'"


use "`tmpfile'", clear //load dataset 



use "GNOfinal.dta", clear
merge 1:1 date using "factor_size - copy.dta"
rename _merge _merge3
save GNOfinal.dta, replace

br



save GNOfinal.dta, replace

//merge dollar


********************************************************************************
*4) REGRESSIONS
********************************************************************************

use GNOfinal.dta, clear
reg ret_gno f_size1 f_size2 f_size3
reg ret_gno market_risk_premium size_factor value_factor profitability_factor investment_factor risk_free_rate

use GNOfinal.dta, clear

* Run regressions for f_size1, f_size2, and f_size3 on the growth variables
* Clear previous estimates
eststo clear

local yvar f_size1 f_size2 f_size3

local dvs adrbalcnt adractcnt txcnt txtfrvaladjntv 

foreach var of varlist `yvar' {
    foreach x of varlist `dvs' {
  
        eststo: qui reg `var' `x'
    }
}



//The stationarity is a fundamental step in time series analysis. If a time series is non-stationary, it can affect the validity and reliability of statistical tests and models.

//There are several methods to check for stationarity. One of the most common ways in the context of econometrics and time series analysis is the Augmented Dickey-Fuller (ADF) test.


dfuller close, lags(7)


dfuller ret_gno, lags(7)


dfuller adractcnt, lags(7)

tsset date
gen dn_adractcnt = adractcnt/L.adractcnt-1
label var dn_adractcnt "Active Address Count, Growth"


foreach x of varlist adrbalcnt txcnt txtfrvaladjntv {
	local xlab "`: variable label `x''"
	disp "`xlab'"
	}

foreach x of varlist adrbalcnt txcnt txtfrvaladjntv {
	local xlab "`: variable label `x''"
    gen dn_`x' = (`x' - L.`x') / L.`x'
    label var dn_`x' "`xlab', Growth"
}




* Contemporaneous regression for ret gno and varian;es

eststo clear
local yvar  ret_gno
local xvar1 dn_adractcnt  

* Declare a list of variables, which we call dsv   
local dvs adrbalcnt txcnt txtfrvaladjntv 
foreach var of varlist `dvs' {
	eststo: qui reg `yvar' dn_`var'

}


 
esttab, r2(4) ar2(4) label star(* 0.1)
esttab using table1.tex, r2(4) ar2(4) label star(* 0.1) nonumbers nogaps brackets compress scalars(F) booktabs title(GNO Network Data) mtitles("reg 1" "reg 2" "reg 3" "reg 4") p(3) b(4) addnotes () replace

* Predictive regressions

* NOTICE: We regress y on lag of x
use "GNOfinal.dta", clear
eststo clear
local yvar "ret_gno"
local k=1
*gen test1=L`k'.`yvar'
gen lagvar=`yvar'
foreach i of varlist ret_gno {
   forvalues j=1/7 {
    replace lagvar=l`j'.`i'
	label var lagvar "Return t"
	eststo: qui reg `i' lagvar
  }
}


esttab, r2(4) ar2(4) label star(* 0.1)
esttab, r2(4) ar2(4) label star(* 0.1) mtitles("Return t+1" "Return t+2" "Return t+3" "Return t+4" "Return t+5" "Return t+6" "Return t+7") replace

esttab using table1.tex, r2(4) ar2(4) label star(* 0.1) nonumbers nogaps brackets compress scalars(F) booktabs title( Momentum) mtitles("reg 1" "reg 2" "reg 3" "reg 4") p(3) b(4) addnotes () replace

////////GNO good trends before weekly freq

* Google search data
import delimited "multiTimeline", clear rowrange(2:) varn(2)
br
keep week gnocoinworldwide



gen date = date(week,"YMD") // to change the year-month-date into a stata date, so that we can merge datasets 
save "multiTimeline", replace

use GNOfinal.dta, clear
merge 1:1 date using multiTimeline
rename _merge _merge4
save GNOfinal.dta, replace

use GNOfinal.dta, clear

reg ret_gno gnocoinworldwide

********************************************************
* Weekly frequency
*******************************************************
use GNOfinal.dta, clear
gen week = wofd(date)

sort week
collapse (mean) ret_gno, by(week)
tsset week

eststo clear
local yvar "ret_gno"
local k=1
*gen test1=L`k'.`yvar'
cap gen lagvar=`yvar'

foreach i of varlist ret_gno {
   forvalues j=1/7 {
       replace lagvar=L`j'.`i'

       eststo: qui reg `i' lagvar f_size1 f_size2 f_size3
   }
}

label var lagvar "Return t"
esttab, r2(4) ar2(4) label star(* 0.1)
esttab, r2(4) ar2(4) label star(* 0.1) ///
   mtitles("Return t+1" "Return t+2" "Return t+3" "Return t+4" "Return t+5" "Return t+6" "Return t+7") ///
   replace

//
eststo clear
local yvar "ret_gno"
local k=1
*gen test1=L`k'.`yvar'
cap gen lagvar=`yvar'

foreach i of varlist ret_gno {
   forvalues j=1/7 {
       replace lagvar=L`j'.`i'

       eststo: qui reg `i' lagvar market_risk_premium size_factor value_factor profitability_factor investment_factor risk_free_rate
   }
}

esttab, r2(4) ar2(4) label star(* 0.1)
esttab, r2(4) ar2(4) label star(* 0.1) ///
   mtitles("Return t+1" "Return t+2" "Return t+3" "Return t+4" "Return t+5" "Return t+6" "Return t+7") ///
   replace
* Google search data
import delimited "BTC_google.csv", clear rowrange(2:) varn(2)
keep week bitcoinworldwide
gen date = date(week,"YMD")
drop week
gen week = wofd(date)
save google.dta,replace

* Merge weekly prices with weekly google
use BTCfinal.dta, clear
gen week = wofd(date)

sort week
collapse (mean) ret_btc, by(week)

merge 1:1 week using google.dta 

tsset week
gen Dgoogle=bitcoinworldwide/L.bitcoinworldwide-1 // change in google interest


//






********************************************************************************
*5) EVENT STUDY
********************************************************************************
* Load the data and set it up as a time series
use GNOfinal.dta, clear
tsset date

* Define the event date for the analysis as November 6, 2022
local event_date = td(06nov2022)

* Generate a variable that counts the number of days from the event date
gen days_from_event = date - `event_date'

* Set the estimation and event windows based on daily data
* Here, we'll use -250 to -30 for the estimation window, and -5 to +5 for the event window
local est_window_start = -250
local est_window_end = -30
local event_window_start = -10
local event_window_end = 10


* Filter data for the event window and plot returns
twoway (line close days_from_event if -10< days_from_event &  days_from_event< 10, lcolor(blue) lpattern(solid) ///
        title("Gnosis Price (USD) During FTX Collapse") ytitle("Price") xtitle("Days from FTX Collapse"))


* Flag observations in the estimation and event windows
gen in_estimation_window = (days_from_event >= `est_window_start' & days_from_event <= `est_window_end')
gen in_event_window = (days_from_event >= `event_window_start' & days_from_event <= `event_window_end')

* Check that we have enough observations in both windows
count if in_estimation_window == 1
assert r(N) >= 200  // Example threshold: at least 200 observations in estimation window
count if in_event_window == 1
assert r(N) >= 10   // Example threshold: at least 10 observations in event window

* Estimating Normal Performance using OLS regression over the estimation window

* We assume there's a variable `size` for predicting `ret_btc`
reg ret_gno adrbalcnt if in_estimation_window == 1

* Save regression coefficients and RMSE
scalar alpha = _b[_cons]
scalar beta = _b[adrbalcnt]
scalar rmse = e(rmse)

* Predict normal performance (expected returns) in the event window using the saved parameters
gen predicted_ret_gno = alpha + beta * adrbalcnt

* Calculating Abnormal Returns (ARs) as the difference between actual and predicted returns
gen abnormal_ret = ret_gno - predicted_ret_gno

* Calculate Cumulative Abnormal Return (CAR) in the event window
gen CAR = sum(abnormal_ret) if in_event_window == 1

* Calculate the standard deviation of ARs in the event window by sorting the dataset and using egen
sort date
* Calculate the standard deviation of abnormal returns in the estimation window
egen ar_sd = sd(abnormal_ret) if in_estimation_window == 1

* Replace missing values in ar_sd with the standard deviation from the estimation window
replace ar_sd = ar_sd[_n-1] if missing(ar_sd)


* Count number of days in the event window
count if in_event_window == 1
local n_event_days = r(N)



* Calculate the test statistic for CAR significance
gen CAR_test = (1 / sqrt(`n_event_days')) * (CAR / ar_sd) if in_event_window == 1

list CAR ar_sd CAR_test if days_from_event==3



* Filter data for the event window and plot returns
twoway (line ret_gno days_from_event if in_event_window == 1, lcolor(blue) lpattern(solid) ///
        title("Gnosis Returns During FTX Collapse") ytitle("Returns") xtitle("Days from FTX Collapse")) ///
       (line predicted_ret_gno days_from_event if in_event_window == 1, lcolor(red) lpattern(dash)) ///
       (line abnormal_ret days_from_event if in_event_window == 1, lcolor(green) lpattern(shortdash)) ///
       , legend(order(1 "Actual Returns" 2 "Expected Returns" 3 "Abnormal Returns"))

	
//own btc dataset

import delimited btc.csv, clear

gen time_date = date(time, "YMD")  // Convert string to Stata date format
format time_date %td  // Format the new date variable
drop time  // Drop the original string variable
rename time_date date  // Rename the new date variable to 'time'


tsset date
* Define the event date for the analysis as November 6, 2022
local event_date = td(06nov2022)

* Generate a variable that counts the number of days from the event date
gen days_from_event = date - `event_date'

* Set the estimation and event windows based on daily data
* Here, we'll use -250 to -30 for the estimation window, and -5 to +5 for the event window
local est_window_start = -250
local est_window_end = -30
local event_window_start = -10
local event_window_end = 10


* Filter data for the event window and plot returns
twoway (line priceusd days_from_event if -10< days_from_event &  days_from_event< 10, lcolor(blue) lpattern(solid) ///
        title("Bitcoin Price (USD) During FTX Collapse") ytitle("Price") xtitle("Days from FTX Collapse"))

		
//GNOSIS EVENT STUDY 
use GNOfinal.dta, clear
tsset date

* Define the event date for the analysis as November 6, 2022
local event_date = td(30aug2021)

* Generate a variable that counts the number of days from the event date
gen days_from_event = date - `event_date'

* Set the estimation and event windows based on daily data
* Here, we'll use -250 to -30 for the estimation window, and -5 to +5 for the event window
local est_window_start = -250
local est_window_end = -30
local event_window_start = -10
local event_window_end = 10

twoway (line close days_from_event if -10< days_from_event &  days_from_event< 10, lcolor(blue) lpattern(solid) ///
        title("Gnosis Price (USD) During Binance Listing ") ytitle("Price") xtitle("Days from Binance Listing"))

		
//GNOSIS BINANCE LISTING AUG 2021

use GNOfinal.dta, clear
tsset date

local event_date = td(30aug2021)

gen days_from_event = date - `event_date'


local est_window_start = -250
local est_window_end = -30
local event_window_start = -10
local event_window_end = 10

gen in_estimation_window = (days_from_event >= `est_window_start' & days_from_event <= `est_window_end')
gen in_event_window = (days_from_event >= `event_window_start' & days_from_event <= `event_window_end')

count if in_estimation_window == 1
assert r(N) >= 200  
count if in_event_window == 1
assert r(N) >= 10   

reg ret_gno adrbalcnt if in_estimation_window == 1

* Save regression coefficients and RMSE
scalar alpha = _b[_cons]
scalar beta = _b[adrbalcnt]
scalar rmse = e(rmse)

* Predict normal performance (expected returns) in the event window using the saved parameters
gen predicted_ret_gno = alpha + beta * adrbalcnt

* Calculating Abnormal Returns (ARs) as the difference between actual and predicted returns
gen abnormal_ret = ret_gno - predicted_ret_gno

* Calculate Cumulative Abnormal Return (CAR) in the event window
gen CAR = sum(abnormal_ret) if in_event_window == 1

* Calculate the standard deviation of ARs in the event window by sorting the dataset and using egen
sort date
* Calculate the standard deviation of abnormal returns in the estimation window
egen ar_sd = sd(abnormal_ret) if in_estimation_window == 1

* Replace missing values in ar_sd with the standard deviation from the estimation window
replace ar_sd = ar_sd[_n-1] if missing(ar_sd)


* Count number of days in the event window
count if in_event_window == 1
local n_event_days = r(N)

* Filter data for the event window and plot returns
twoway (line ret_gno days_from_event if in_event_window == 1, lcolor(blue) lpattern(solid) ///
        title("Gnosis Returns During Binance Listng") ytitle("Returns") xtitle("Days from Binance Listing")) ///
       (line predicted_ret_gno days_from_event if in_event_window == 1, lcolor(red) lpattern(dash)) ///
       (line abnormal_ret days_from_event if in_event_window == 1, lcolor(green) lpattern(shortdash)) ///
       , legend(order(1 "Actual Returns" 2 "Expected Returns" 3 "Abnormal Returns"))

	   
gen total_volume = volumefrom + volumeto
// Bar chart showing total trading volume during the event window
twoway (bar total_volume days_from_event if in_event_window == 1, ///
        barwidth(0.8) lcolor(blue) ///
        title("Total Trading Volume During Gnosis Binance Listing") ///
        ytitle("Volume") xtitle("Days from Binance Listing")), ///
       legend(off)
use "GNOfinal.dta",clear
br
save "C:\Users\fs00675\OneDrive - University of Surrey\Documents\Gnosis Project\GNOfinal.dta", replace



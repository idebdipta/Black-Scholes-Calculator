#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

#define inv_sqrt_2xPI 0.39894228040143270286

double norm_cdf(const double x) {
    double k = 1.0/(1.0 + 0.2316419*x);
    double k_sum = k*(0.319381530 + k*(-0.356563782 + k*(1.781477937 + k*(-1.821255978 + 1.330274429*k))));

    if (x >= 0.0) {
        return (1.0 - (1.0/(pow(2*M_PI,0.5)))*exp(-0.5*x*x) * k_sum);
    } else {
        return 1.0 - norm_cdf(-x);
    }
}

double d_j(const int j, const double S, const double K, const double r, const double v, const double T) {
    return (log(S/K) + (r + (pow(-1,j-1))*0.5*v*v)*T)/(v*(pow(T,0.5)));
}

// Calculate the European vanilla call price based on
// underlying S, strike K, risk-free rate r, volatility of
// underlying sigma and time to maturity T
double call_price(const double S, const double K, const double r, const double v, const double T) {
    return S * norm_cdf(d_j(1, S, K, r, v, T))-K*exp(-r*T) * norm_cdf(d_j(2, S, K, r, v, T));
}

// Calculate the European vanilla put price based on
// underlying S, strike K, risk-free rate r, volatility of
// underlying sigma and time to maturity T
double put_price(const double S, const double K, const double r, const double v, const double T) {
    return -S*norm_cdf(-d_j(1, S, K, r, v, T))+K*exp(-r*T) * norm_cdf(-d_j(2, S, K, r, v, T));
}
float CNDF ( float InputX ) 
{
    int sign;

    float OutputX;
    float xInput;
    float xNPrimeofX;
    float expValues;
    float xK2;
    float xK2_2, xK2_3;
    float xK2_4, xK2_5;
    float xLocal, xLocal_1;
    float xLocal_2, xLocal_3;

	printf("\n cdf called for input = %lf\n", InputX);
    // Check for negative value of InputX
    if (InputX < 0.0) {
        InputX = -InputX;
        sign = 1;
    } else 
        sign = 0;

    xInput = InputX;
 
    // Compute NPrimeX term common to both four & six decimal accuracy calcs
    expValues = exp(-0.5f * InputX * InputX);
    xNPrimeofX = expValues;
    xNPrimeofX = xNPrimeofX * inv_sqrt_2xPI;

    xK2 = 0.2316419 * xInput;
    xK2 = 1.0 + xK2;
    xK2 = 1.0 / xK2;
    xK2_2 = xK2 * xK2;
    xK2_3 = xK2_2 * xK2;
    xK2_4 = xK2_3 * xK2;
    xK2_5 = xK2_4 * xK2;
    
    xLocal_1 = xK2 * 0.319381530;
    xLocal_2 = xK2_2 * (-0.356563782);
    xLocal_3 = xK2_3 * 1.781477937;
    xLocal_2 = xLocal_2 + xLocal_3;
    xLocal_3 = xK2_4 * (-1.821255978);
    xLocal_2 = xLocal_2 + xLocal_3;
    xLocal_3 = xK2_5 * 1.330274429;
    xLocal_2 = xLocal_2 + xLocal_3;

    xLocal_1 = xLocal_2 + xLocal_1;
    xLocal   = xLocal_1 * xNPrimeofX;
    xLocal   = 1.0 - xLocal;

    OutputX  = xLocal;
    
    if (sign) {
        OutputX = 1.0 - OutputX;
    }
    
    double normal = norm_cdf(InputX);
	printf("\n cdf called for output = %lf, other one is %lf\n", OutputX, normal);
    return OutputX;
} 


float blackscholes( float sptprice,
                            float strike, float rate, float volatility,
                            float time, int otype)
{
    float OptionPrice;

    // local private working variables for the calculation
    float xStockPrice;
    float xStrikePrice;
    float xRiskFreeRate;
    float xVolatility;
    float xTime;
    float xSqrtTime;

    float logValues;
    float xLogTerm;
    float xD1; 
    float xD2;
    float xPowerTerm;
    float xDen;
    float d1;
    float d2;
    float FutureValueX;
    float NofXd1;
    float NofXd2;
    float NegNofXd1;
    float NegNofXd2;    
    
    xStockPrice = sptprice;
    xStrikePrice = strike;
    xRiskFreeRate = rate;
    xVolatility = volatility;

    xTime = time;
    xSqrtTime = sqrt(xTime);

    logValues = log( sptprice / strike );
        
    xLogTerm = logValues;
        
    
    xPowerTerm = xVolatility * xVolatility;
    xPowerTerm = xPowerTerm * 0.5;
        
    xD1 = xRiskFreeRate + xPowerTerm;
    xD1 = xD1 * xTime;
    xD1 = xD1 + xLogTerm;

    xDen = xVolatility * xSqrtTime;
    xD1 = xD1 / xDen;
    xD2 = xD1 -  xDen;

    d1 = xD1;
    d2 = xD2;
    
    NofXd1 = CNDF( d1 );
    NofXd2 = CNDF( d2 );

    FutureValueX = strike * ( exp( -(rate)*(time) ) );        
    if (otype == 0) {            
        OptionPrice = (sptprice * NofXd1) - (FutureValueX * NofXd2);
    } else { 
        NegNofXd1 = (1.0 - NofXd1);
        NegNofXd2 = (1.0 - NofXd2);
        OptionPrice = (FutureValueX * NegNofXd2) - (sptprice * NegNofXd1);
    }
    
    return OptionPrice;
}


/*
float blackscholes( float sptprice,
                            float strike, float rate, float volatility,
                            float time, int otype)
*/

double discoverCallIn(int itcount, double targetval, double miniv, double maxiv,
						float underlying, float strike, float riskfree, float days){
	double temp, midval, midiv;						
	double minval = call_price(underlying, strike, riskfree, miniv, days);
	double maxval = call_price(underlying, strike, riskfree, maxiv, days);
	
	printf("\n IterationCount = %d, miniv = %lf, maxiv = %lf, minval = %lf, maxval = %lf",
			itcount, miniv, maxiv, minval, maxval);
	if(targetval < minval-0.05){
		return discoverCallIn((itcount+1), targetval, miniv/2, miniv, underlying, strike, riskfree, days);
	}
	else if(targetval > maxval+0.05){
		return discoverCallIn((itcount+1), targetval, maxiv, maxiv+(maxiv-miniv), underlying, strike, riskfree, days);
	}
	else{
		temp = maxiv - miniv; 
		midiv = (maxiv+miniv)/2;
		//Return midvalue
		if(temp < 0.0002) return midiv;
		if(midiv < 0.0001) return midiv;
		
		midval = call_price(underlying, strike, riskfree, midiv, days);
		if(targetval < midval)
			return discoverCallIn((itcount+1), targetval, miniv, midiv, underlying, strike, riskfree, days);
		else if(targetval > midval)
			return discoverCallIn((itcount+1), targetval, midiv, maxiv, underlying, strike, riskfree, days);
		else return midiv;
	}
}

double discoverCallIn2(int itcount, double targetval, double miniv, double maxiv,
						float underlying, float strike, float riskfree, float days){
	int flag = 0;
	double val = 0;
	while(flag == 0){
		double temp, midval, midiv;						
		double minval = call_price(underlying, strike, riskfree, miniv, days);
		double maxval = call_price(underlying, strike, riskfree, maxiv, days);
		
		printf("\n IterationCount = %d, miniv = %lf, maxiv = %lf, minval = %lf, maxval = %lf",
				itcount, miniv, maxiv, minval, maxval);
		if(targetval < minval){
			itcount++;
			maxiv = miniv; miniv = miniv/2;
			midiv = (maxiv+miniv)/2;
			if(midiv < 0.0001) return midiv;
			continue;
		}
		else if(targetval > maxval){
			itcount++;
			miniv = maxiv; 
			maxiv = maxiv+1.0;
			continue;
		}
		else{
			temp = maxiv - miniv; 
			midiv = (maxiv+miniv)/2;
			//Return midvalue
			if(temp < 0.0002) return midiv;
			if(midiv < 0.0001) return midiv;
			
			midval = call_price(underlying, strike, riskfree, midiv, days);
			itcount++;
			if(targetval < midval)
				maxiv = midiv;
			else if(targetval > midval)
				miniv = midiv;
			else return midiv;
		}
		if(itcount > 100) return maxiv;
	}
	return val;
}


int main(int argc, char *argv[])
{
	 float temp;
     float strike_price = atof(argv[1]);
     float asset_price = atof(argv[2]);
     float volatality = atof(argv[3])/100.0;
     float risk_free_rate = atof(argv[4])/100.0;
     float days_to_exp = atof(argv[5])/250.0;
     
     float cprice = atof(argv[6]);
     float pprice = atof(argv[7]);
     int i = 0;
     double callprice = 0;
     printf("Strike Price: %f \n", strike_price);
     printf("Asset Price:  %f \n", asset_price);
     printf("Volatality:      %f \n", volatality);
     printf("Risk Free:    %f \n", risk_free_rate);
     printf("Days to Exp:  %f \n", days_to_exp);
     //printf("Put Value:    %f \n", blackscholes(asset_price, strike_price, risk_free_rate, volatality, days_to_exp, 1));
     printf("Put Value:   %f \n", put_price(asset_price, strike_price, risk_free_rate, volatality, days_to_exp));
     
     callprice = call_price(asset_price, strike_price, risk_free_rate, volatality, days_to_exp);
     //printf("Call Value:    %f \n", blackscholes(asset_price, strike_price, risk_free_rate, volatality, days_to_exp, 0));
     printf("Call Value:   %f \n", callprice);
     
     
     /*
     for(i = 0; i<=20; i++){
		 temp = 0.05*i;
		 temp = CNDF(temp);
		 printf("\n Value of Normal for %2d \t %lf", i, temp);
	 }*/
	 if(cprice > 0){
		 //temp = (float)discoverCallIn(0, cprice, 0.0001, 5.0, asset_price, strike_price, risk_free_rate, days_to_exp);
		 double temp2 = (float)discoverCallIn2(0, cprice, 0, 0.1, asset_price, strike_price, risk_free_rate, days_to_exp);
		 
		 printf("\n Discovered implied volatality = %lf, vol2 = %lf\n", temp, temp2);
	 }
     return 0;
}

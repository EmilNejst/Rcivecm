# Rcivecm
A cointegration package for R.

## The general idea
R is lacking a proper package for thorough cointegration analysis of time series data.
This project aims at remedying that. We seek to provide the full analytical power of
the principles outlines primarily in Juselius(2007) and Johansen(1996). We will also
aim at incoporating more modern extensions applying the bootstrap to correct statistics
for features of non-normality. 

## The scope
The project focuses solely on linear cointegration in the I(1) framework, seeking to
provide a fully fletched analysis suite for that particular framework. As such we are
*not* going to develop functionality allowing for non-linearities, measurement errors,
I(2) cointegration, fractional cointegration and so on. Packages handling those
extensions should be developed independently. 

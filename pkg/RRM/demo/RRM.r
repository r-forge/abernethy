## file RRM.r
##
## A demonstration of the completion of the small example set included with the ReliaWiki article
## detailing the ReliaSoft Ranking Method

## initial data entry		
fail<-c(	10	,
	40	,
	40	,
	50	)
		
susp<-c(	20	,
	60	)
		
disc<-c(	30	,
	30	,
	70	,
	100	)
		
left<-c(	20	,
	20	,
	10	)
		
right<-c(	80	,
	80	,
	85	)

## formation of interval argument dataframe for function RRM
## this includes discoveries and true intervals using zero (or any non-positive numeric value including NA)
## for left time entry for the discovery data (i.e.  left-censored data)

interval_arg<-data.frame(left=c(rep(0,length(disc)),left), right=c(disc,right))

## Now simply make the call to RRM

example<-RRMw(fail,susp,interval_arg)
example



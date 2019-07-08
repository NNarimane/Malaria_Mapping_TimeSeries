##################################
########## 
library(tidyverse)

###############################################################
## Read in data on case numbers of Amazonas State municipality.
## Based on csv file provided by Raquel.

#BML <- read.csv("/home/claudia/Documentos/PosDoc_PROCC/malaria/AnalisesRaquel/AnaliseExploratoria/AM_municipality.csv")
load("/home/claudia/Documentos/PosDoc_PROCC/malaria/AnalisesRaquel/AnaliseExploratoria/API_Noti_week_state_final.Rdata")

## Rank by Year, State and Type
dRankms <- d8 %>% 
  filter(LEVEL == "MU") %>% 
  select(CODE, STATE, NAME, YEAR, TYPE, CASES, POP_SIZE) %>% 
  group_by(CODE, STATE, YEAR, TYPE, POP_SIZE) %>% 
  mutate(SUMCASES = sum(CASES, na.rm = TRUE)) %>%
  select(-CASES) %>% 
  distinct() %>% 
  mutate(API = SUMCASES/POP_SIZE * 1000) %>% 
  arrange(YEAR, TYPE, API)


dRankms <- dRankms %>% 
  mutate(ONE = 1) %>% 
  group_by(YEAR, STATE, TYPE) %>% 
  mutate(RANK = cumsum(ONE)) 

## Filter by state
BML <- dRankms %>% 
  filter(STATE == "MT")

#"RO" "AC" "AM" "RR" "PA" "AP" "TO" "MA" "MT"

###############################################################
## Define years & municipalities, and create matrices for
## storing case numbers, populations and API

years <- seq(from=2004, to=2018, by=1)
N_years <- length(years)

munic <- as.vector(unique(BML$NAME))
N_munic <- length(munic)

AM_cases <- matrix(NA, nrow=N_munic, ncol=N_years)
colnames(AM_cases) <- years
rownames(AM_cases) <- munic

AM_pop <- matrix(NA, nrow=N_munic, ncol=N_years)
colnames(AM_pop) <- years
rownames(AM_pop) <- munic

AM_API <- matrix(NA, nrow=N_munic, ncol=N_years)
colnames(AM_API) <- years
rownames(AM_API) <- munic

for(i in 1:N_munic)
{
	for(j in 1:N_years)
	{
		index <- which( BML$NAME == munic[i] & BML$YEAR == years[j] & BML$TYPE == "Vivax" )

		AM_cases[i,j] <- BML$SUMCASES[index]

		AM_pop[i,j] <- BML$POP_SIZE[index]

		AM_API[i,j] <- BML$API[index]
	}
}


###############################################################
## Create a ranking of municipalities by API for each year.

AM_API_rank <- matrix(NA, nrow=N_munic, ncol=N_years)
colnames(AM_API_rank) <- years
rownames(AM_API_rank) <- munic

for(j in 1:N_years)
{
	munic_rank <- munic[order( AM_API[,j], decreasing=TRUE )]

	for(i in 1:N_munic)
	{
		AM_API_rank[i,j] <- which( munic_rank == munic[i] )
	}
}


###############################################################
## For each municipality, define the lower and upper bounds for
## plotting the league table. This is set so that the width is
## proportional to the number of cases.

AM_league_upper <- matrix(NA, nrow=N_munic, ncol=N_years)
colnames(AM_league_upper) <- years
rownames(AM_league_upper) <- munic

AM_league_lower <- matrix(NA, nrow=N_munic, ncol=N_years)
colnames(AM_league_lower) <- years
rownames(AM_league_lower) <- munic


for(j in 1:N_years)
{
	for(i in 1:N_munic)
	{
		AM_league_lower[i,j] <- sum(AM_cases[which(AM_API_rank[,j] <= AM_API_rank[i,j]),j])
		AM_league_upper[i,j] <- sum(AM_cases[which(AM_API_rank[,j] <= AM_API_rank[i,j]-1),j])
	}
}

for(j in 1:N_years)
{
	AM_league_lower[,j] <- AM_league_lower[,j]/max(AM_league_lower[,j])
	AM_league_upper[,j] <- AM_league_upper[,j]/max(AM_league_upper[,j])
}

AM_league_lower <- 1 - AM_league_lower

AM_league_upper <- 1 - AM_league_upper



###############################################################
## Define the bounds for plotting each year

year_seq_lower <- seq(from=0, by=1.5, length=N_years)
year_seq_upper <- seq(from=1, by=1.5, length=N_years)



###############################################################
## Assign a colour to each municipality.
## Randomly permuting the colours can improve visualisation.

munic_cols <- rainbow(N_munic, start=0, end=4/6)[AM_API_rank[,1]]

munic_cols_trans <- rainbow(N_munic, start=0, end=4/6, alpha=0.5)[AM_API_rank[,1]]


munic_cols_light       <- rainbow(N_munic, start=0, end=4/6, alpha=0.2)[AM_API_rank[,1]]

munic_cols_light_trans <- rainbow(N_munic, start=0, end=4/6, alpha=0.1)[AM_API_rank[,1]]



rand_permut <- sample(N_munic)

munic_cols       <- munic_cols[rand_permut]
munic_cols_trans <- munic_cols_trans[rand_permut]


munic_cols_light       <- munic_cols_light[rand_permut]
munic_cols_light_trans <- munic_cols_light_trans[rand_permut]


###############################################################
## Plot the Amazonas State Vivax League. Municipality are ranked
## by API for each year, with width proportional to total case
## numbers.


key_index <- which(AM_league_upper[,N_years] - AM_league_lower[,N_years] > 0.02)


munic_labels <- names( 0.5*(AM_league_upper[key_index,N_years] + AM_league_lower[key_index,N_years]) )

munic_at <- 0.5*(AM_league_upper[key_index,N_years] + AM_league_lower[key_index,N_years])

### State name
SNAME = "Mato Grosso"


tiff( file="/home/claudia/Documentos/PosDoc_PROCC/malaria/AnalisesRaquel/Plots_Michael/MatoGrosso_State_Vivax_League_v2.tif", width=18, height=14, units="cm", res=500)

par(mfrow=c(1,1))
par(mar = c(2.5,2.5,1,4))
par(mgp=c(1.3,0.5,0))

plot(x=1e10, y=1e10,
xlim=c(0, max(year_seq_upper)), ylim=c(0,1),
xaxs="i", yaxs="i", xaxt="n", yaxt="n", bty='n',
xlab="time (years)", ylab="proportion of cases",
#main="Acre State Vivax League" )
main= paste0(SNAME, " ", "State Vivax League"))


for(j in 1:N_years)
{
	for(i in 1:N_munic)
	{
		polygon( x=c( year_seq_lower[j], year_seq_upper[j], year_seq_upper[j], year_seq_lower[j] ),
			   y=c( AM_league_lower[i,j], AM_league_lower[i,j], AM_league_upper[i,j], AM_league_upper[i,j] ),
                     col=munic_cols_light[i], border=NA ) 
	}
}

for(j in 1:(N_years-1))
{
	for(i in 1:N_munic)
	{
		polygon( x=c( year_seq_upper[j], year_seq_lower[j+1], year_seq_lower[j+1], year_seq_upper[j] ),
			   y=c( AM_league_lower[i,j], AM_league_lower[i,j+1], AM_league_upper[i,j+1], AM_league_upper[i,j] ),
                     col=munic_cols_light_trans[i], border=NA ) 
	}
}




for(j in 1:N_years)
{
	for(i in key_index)
	{
		polygon( x=c( year_seq_lower[j], year_seq_upper[j], year_seq_upper[j], year_seq_lower[j] ),
			   y=c( AM_league_lower[i,j], AM_league_lower[i,j], AM_league_upper[i,j], AM_league_upper[i,j] ),
                     col=munic_cols[i], border=NA ) 
	}
}

for(j in 1:(N_years-1))
{
	for(i in key_index)
	{
		polygon( x=c( year_seq_upper[j], year_seq_lower[j+1], year_seq_lower[j+1], year_seq_upper[j] ),
			   y=c( AM_league_lower[i,j], AM_league_lower[i,j+1], AM_league_upper[i,j+1], AM_league_upper[i,j] ),
                     col=munic_cols_trans[i], border=NA ) 
	}
}




axis(1, at=0.5*(year_seq_lower + year_seq_upper), labels=years, cex.axis=0.7)

axis(2, at=c(0.0,0.25,0.5,0.75,1), labels=c("0%", "25%", "50%", "75%", "100%"), cex.axis=0.8)

for(k in 1:length(key_index))
{
	axis(4, at=munic_at[k], labels=munic_labels[k], 
	col.axis=munic_cols[key_index][k],
	tick=FALSE, line=NA, las=2,
	cex.axis=0.5)
}


dev.off()


###############################################################
## Distribution of cases within population. This is essentially
## taking a cross-section from each year


tiff( file="/home/claudia/Documentos/PosDoc_PROCC/malaria/AnalisesRaquel/Plots_Michael/MT_Vivax_Case_Heterogeneity.tif", width=12, height=12, units="cm", res=500)

par(mfrow=c(1,1))
par(mar = c(2.5,2.5,1,1.75))
par(mgp=c(1.3,0.5,0))

year_cols <- rainbow(N_years, start=0, end=4/6)

line_seq <- c(0.2, 0.4, 0.6, 0.8)

plot(x=c(0,1), y=c(0,1), type='l', lty="dashed",
xlim=c(0,1.002), ylim=c(0,1.002),
xaxs='i', yaxs='i', xaxt='n', yaxt='n',
xlab="proportion of population", ylab="proportion of cases",
#main="Vivax cases within Acre state")
main= paste0("Vivax cases within", " ", SNAME, " ", "state"))

axis(1, at=c(0.0, 0.2, 0.4, 0.6, 0.8, 1), 
        labels=c("0%", "20%", "40%", "60%", "80%", "100%"), cex.axis=1.0 ) 

axis(2, at=c(0.0, 0.2, 0.4, 0.6, 0.8, 1), 
        labels=c("0%", "20%", "40%", "60%", "80%", "100%"), cex.axis=1.0 ) 


for(i in 1:4)
{
	points(x=c(0,1), y=rep(line_seq[i],2), type='l', col="grey", lty="dashed")
	points(x=rep(line_seq[i],2), y=c(0,1), type='l', col="grey", lty="dashed")
}


for(j in 1:N_years)
{

	AM_cases_cumsum <- cumsum(AM_cases[order(AM_API[,j], decreasing=TRUE),j])
	AM_cases_cumsum <- AM_cases_cumsum/AM_cases_cumsum[length(AM_cases_cumsum)]
	AM_cases_cumsum <- c(0, AM_cases_cumsum)

	AM_pop_cumsum <- cumsum(AM_pop[order(AM_API[,j], decreasing=TRUE),j])
	AM_pop_cumsum <- AM_pop_cumsum/AM_pop_cumsum[length(AM_pop_cumsum)]
	AM_pop_cumsum <- c(0, AM_pop_cumsum)

	points( x=AM_pop_cumsum, y=AM_cases_cumsum, 
              type='l', lwd=2, col=year_cols[j])
}

legend(x='bottomright', 
legend = years, 
fill = year_cols,
border = year_cols,
ncol=1, cex=0.75, bty="n" )

dev.off()



###############################################################
## Calculate the Gini coefficient over time, and look at the 
## association between API

Amazonas_API <- 1000*colSums(AM_cases)/colSums(AM_pop)


Gini_coef <- rep(NA, N_years)

for(j in 1:N_years)
{

	AM_cases_cumsum <- cumsum(AM_cases[order(AM_API[,j], decreasing=TRUE),j])
	AM_cases_cumsum <- AM_cases_cumsum/AM_cases_cumsum[length(AM_cases_cumsum)]
	AM_cases_cumsum <- c(0, AM_cases_cumsum)

	AM_pop_cumsum <- cumsum(AM_pop[order(AM_API[,j], decreasing=TRUE),j])
	AM_pop_cumsum <- AM_pop_cumsum/AM_pop_cumsum[length(AM_pop_cumsum)]
	AM_pop_cumsum <- c(0, AM_pop_cumsum)

	Gini_coef[j] <- sum( (AM_pop_cumsum[2:(N_munic+1)] - AM_pop_cumsum[1:N_munic])*0.5*(AM_cases_cumsum[2:(N_munic+1)] + AM_cases_cumsum[1:N_munic]) )

	Gini_coef[j] <- 2*(Gini_coef[j] - 0.5) 
}





tiff( file="/home/claudia/Documentos/PosDoc_PROCC/malaria/AnalisesRaquel/Plots_Michael/MatoGrosso_API_and_Gini.tif", width=10, height=20, units="cm", res=500)

par(mfrow=c(1,1))
par(mar = c(2.5,2.5,1,1.75))
par(mgp=c(1.3,0.5,0))


par(mfrow=c(3,1))

####################################################
## Panel 1: API in Amazonas state

plot(x=years, y=Amazonas_API, 
type='l', lwd=2,
ylim=c(0,3),
xlab="time (years)", ylab="API (cases/1000 population)",
#main="API in Acre state")
main= paste0("API in", " ", SNAME, " ", "state"))

points(x=years, y=Amazonas_API, 
pch=19, cex=2, col=year_cols)


####################################################
## Panel 2: Gini coefficient in Amazonas state municipalities

plot(x=years, y=Gini_coef, 
type='l', lwd=2,
ylim=c(0,1),
xlab="time (years)", ylab="Gini coefficient",
#main="Gini coefficient in Acre state municipalities")
main= paste0("Gini coefficient in", " ", SNAME, " ", "state municipalities"))

points(x=years, y=Gini_coef, 
pch=19, cex=2, col=year_cols)


####################################################
## Panel 3: API vs Gini coefficient
## 
## Also fit a linear model to the trend and assess
## statistical significance.

API_seq <- seq(from=0, to=60, by=0.1)

API_Gini_mod <- lm( Gini_coef ~ Amazonas_API )

API_Gini_mod_fit <- predict.lm( API_Gini_mod, data.frame(Amazonas_API=API_seq), interval="prediction", se.fit=TRUE)

API_Gini_mod_med = API_Gini_mod_fit$fit[,1]

API_Gini_mod_low = API_Gini_mod_fit$fit[,2]

API_Gini_mod_high = API_Gini_mod_fit$fit[,3]



plot(x=Amazonas_API, y=Gini_coef,
pch=19, cex=2, col=year_cols,
xlim=c(0,60), ylim=c(0,1),
xlab="API (cases/1000 population)", ylab="Gini coefficient",
main=paste("API vs Gini coefficient; P = ", round(summary(API_Gini_mod)$coef[2,4],4) ) )




points(x=API_seq, y=API_Gini_mod_med,  type='l', lwd=2 )

points(x=API_seq, y=API_Gini_mod_low,  type='l', 
lty="longdash", col="grey", lwd=2 )

points(x=API_seq, y=API_Gini_mod_high, type='l', 
lty="longdash", col="grey", lwd=2 )


dev.off()






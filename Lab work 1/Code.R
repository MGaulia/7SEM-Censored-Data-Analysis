library(tidyverse)
library(survival)
library(ggfortify)
library(muhaz)

### Duomenys

d <- read_csv("SurvivalCovid.csv") %>% 
	select("time", "sex", "ph.ecog", "status") %>% 
	drop_na()
d$time <- as.integer(d$time)
d$ph.ecog <- as.factor(d$ph.ecog)
d$sex <- as.factor(d$sex)
d$status <- as.integer(d$status)
#perkoduojame stulpeli (buvo 1-cenzuruota, dabar 0-cenzuruota)
d$status <- d$status - 1

summary(d)

NROW(d)

summary(as.factor(d$status))

summary(as.factor(d$ph.ecog))

# Sukuriame objekta
s <- Surv(d$time, d$status)

# Modelis
autoplot( survfit(s ~ 1), 
	censor.shape = '*', 
	censor.size = 10, 
	surv.colour = 'red', 
	censor.colour = 'blue')

# Modelis pagal lytį
autoplot(survfit(s ~ d$sex), 
	censor.shape = '*', 
	censor.size = 10) +
  scale_color_manual(values = c("blue","red")) + 
  scale_fill_manual(values = c("blue","red"))
  

# Sukauptos rizikos ivertis
Iverciai <- summary(survfit(s ~ 1, type="kaplan-meier"))
Lambda_hat <- (-1)*log(Iverciai$surv)
Lambda_hat <- c(Lambda_hat, tail(Lambda_hat, 1))
plot(c(Iverciai$time, 1022), Lambda_hat, 
     xlab="time",
     ylab="Lambda_hat",
     main="Cumulative hazard", 
     ylim=range(Lambda_hat),
     type="s")


print(survfit(s ~ 1), print.rmean=TRUE)

# Branduolinis rizikos ivertis
result.simple <- muhaz(d$time, 
                       d$status, 
                       max.time=900, 
                       bw.method="global", 
                       b.cor="none")
plot(result.simple)

# Isgyvenamumas pagal neigaluma
ecog <- s~d$ph.ecog
colors <- c("orange", "green", "red", "blue", "black")
plot(survfit(ecog),
     xlab="Time in days", 
     ylab="Survival probability",
     col=colors, 
     lwd =2)

legend("topright", 
       legend=c("Good-0", "1", "2", "3", "Bad-4"),
       col=colors, lwd =2)

# Homogeniskumo hipotezes
# Logranginis
survdiff(ecog, rho=0)

# Gehan-Wilcoxon kriterijaus Peto ir Peto modifikacija
survdiff(ecog, rho=1)

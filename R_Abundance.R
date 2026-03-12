setwd("C:/Users/61405/OneDrive/Desktop/Project 2 kma")

#list file names
list.files("C:/Users/61405/OneDrive/Desktop/Project 2 kma")

#import csv
d <- read.csv("forest.birds.csv", header = TRUE, stringsAsFactors = TRUE)
birds <- d

#I did not like the order of grazing intensity, here is the change
birds$grazing.intensity <- factor(birds$grazing.intensity, levels = c("light", "less than average", "average", "moderately heavy", "heavy"))
birds$grazing.intensity <- relevel(birds$grazing.intensity, ref = "light")

#############
birds$grazing.intensity <- factor(
  birds$grazing.intensity,
  levels = c("light", "less than average", "average", "moderately heavy", "heavy")
)
birds$grazing.intensity <- relevel(birds$grazing.intensity, ref = "light")

# Check
levels(birds$grazing.intensity)
##############


str(birds)
head(birds)
summary(birds)

#BOX plot 
boxplot(abundance~grazing.intensity, data = birds, main="bird numbers vs grazing parctices", xlab="")


#preliminary plot inspection 
pairs(~ abundance + patch.area + year.of.isolation + dist.nearest + dist.larger + grazing.intensity + altitude + yrs.isolation + patch.area:grazing.intensity + altitude:grazing.intensity, data = birds, panel=panel.smooth)
summary(fit)


#checking co-linearity between yrs.isolation & year.of.isolation
plot(year.of.isolation ~ yrs.isolation, data = birds, xlab = "years of isolation", ylab = "year of isolation")
cor.test(birds$year.of.isolation, birds$yrs.isolation)



#check if log(patch.area) will help 
fix1 <- lm(abundance ~ patch.area, data = birds)
plot(abundance ~ patch.area, data = birds)

fix1 <- lm(abundance ~ log(patch.area), data = birds)
plot(abundance ~ log(patch.area), data = birds)
abline(fix1, col='blue')

#should I add quadartic as it is more difficult to interpret + NO! p= .08
fix2 <- lm(abundance ~ log(patch.area) + I(log(patch.area)^2), data = birds)
plot(abundance ~ log(patch.area) + I(log(patch.area)^2), data = birds)
abline(fix2)
anova(fix1,fix2)

#check if log(dist.larger) will help 
fix3 <- lm(abundance ~ dist.larger, data = birds)
plot(abundance ~ dist.larger, data = birds)

fix3 <- lm(abundance ~ log(dist.larger), data = birds)
plot(abundance ~ log(dist.larger), data = birds)
abline(fix3, col='blue')


#check if log(dist.nearest) will help 
fix4 <- lm(abundance ~ dist.nearest, data = birds)
plot(abundance ~ dist.nearest, data = birds)

fix4 <- lm(abundance ~ log(dist.nearest), data = birds)
plot(abundance ~ log(dist.nearest), data = birds)
abline(fix4, col='blue')


fix6 <- lm(abundance ~ yrs.isolation, data = birds)
plot(abundance ~ yrs.isolation, data = birds)

fix6 <- lm(abundance ~ log(yrs.isolation), data = birds)
plot(abundance ~ log(yrs.isolation), data = birds)
abline(fix6, col='blue')

#preliminary model with logs (WINNER###############)
prelim <- lm(abundance ~ log(patch.area) + log(dist.nearest) + log(dist.larger) + altitude + grazing.intensity + log(yrs.isolation) + log(patch.area):grazing.intensity + altitude:grazing.intensity, data = birds)
summary(prelim)

opar <- par(mfrow=c(2,2))
plot(prelim)
par(opar)

#check influence measure
summary(influence.measures(fit0))

#sensitivity test (major changes without 17, report both/discuss)

birds[c(14,17, 19, 35, 52), ]
birds[order(birds$altitude), ]


birds[17, ]
fit0_no17 <- lm(abundance ~ log(patch.area) + log(dist.nearest) + log(dist.larger) + grazing.intensity + altitude + yrs.isolation + patch.area:grazing.intensity + altitude:grazing.intensity, data = birds[-c(17), ])
summary(fit0_no17)
opar <- par(mfrow=c(2,2))
plot(fit0_no17)

fit0_no14 <- lm(abundance ~ log(patch.area) + log(dist.nearest) + log(dist.larger) + grazing.intensity + altitude + yrs.isolation + patch.area:grazing.intensity + altitude:grazing.intensity, data = birds[-c(14), ])
summary(fit0_no14)
opar <- par(mfrow=c(2,2))
plot(fit0_no14)

fit0_no_14.17 <- lm(abundance ~ log(patch.area) + log(dist.nearest) + log(dist.larger) + grazing.intensity + altitude + yrs.isolation + patch.area:grazing.intensity + altitude:grazing.intensity, data = birds[-c(14, 17), ])
summary(fit0_no_14.17)
opar <- par(mfrow=c(2,2))
plot(fit0_no_14.17)

#####################Drop fit fit

d1 <- lm(abundance ~ log(patch.area) + log(dist.nearest) + log(dist.larger) + altitude + grazing.intensity + log(yrs.isolation) + log(patch.area):grazing.intensity + altitude:grazing.intensity, data = birds)
drop1(d1, .~., test = "F") 
#log(dist.larger) least helpful
summary(d1)

d2 <- lm(abundance ~ log(patch.area) + log(dist.nearest) + altitude + grazing.intensity + log(yrs.isolation) + log(patch.area):grazing.intensity + altitude:grazing.intensity, data = birds)
drop1(d2, .~., test = "F") 
#log(dist.nearer) least helpful 

d3 <- lm(abundance ~ log(patch.area) + altitude + grazing.intensity + log(yrs.isolation) + log(patch.area):grazing.intensity + altitude:grazing.intensity, data = birds)
drop1(d3, .~., test = "F") 
#log(yrs.isolation) least helpful 


d4 <- lm(abundance ~ log(patch.area) + altitude + grazing.intensity + log(patch.area):grazing.intensity + altitude:grazing.intensity, data = birds)
drop1(d4, .~., test = "F") 
#altitude least helpful 

d5 <- lm(abundance ~ log(patch.area) + grazing.intensity + log(patch.area):grazing.intensity + altitude:grazing.intensity, data = birds)
drop1(d5, .~., test = "F") 
#grazing.intensity:altitude least helpful


d6 <- lm(abundance ~ log(patch.area) + grazing.intensity + log(patch.area):grazing.intensity, data = birds)
drop1(d6, .~., test = "F") 
#log(patch.area):grazing.intensity least helpful

d7 <- lm(abundance ~ log(patch.area) + grazing.intensity, data = birds)
drop1(d7, .~., test = "F") 

df <- lm(abundance ~ log(patch.area) + grazing.intensity, data = birds)

summary(df)

opar <- par(mfrow=c(2,2))
plot(df)

#drop short 
fit_short <- lm(abundance ~ log(patch.area) + log(dist.nearest) + log(dist.larger) + altitude + grazing.intensity + log(yrs.isolation) + log(patch.area):grazing.intensity + altitude:grazing.intensity, data = birds)
drop1(fit_short, .~., test = "F") 
#log(dist.larger), log(dist.nearest), log(yrs.isolation) least helpful 

fit_short <- lm(abundance ~ log(patch.area) + altitude + grazing.intensity + log(patch.area):grazing.intensity + altitude:grazing.intensity, data = birds)
drop1(fit_short, .~., test = "F") 
#altitude not good

fit_short <- lm(abundance ~ log(patch.area) + grazing.intensity + log(patch.area):grazing.intensity + altitude:grazing.intensity, data = birds)
drop1(fit_short, .~., test = "F") 
#grazing.intensity:altitude not good

fit_short <- lm(abundance ~ log(patch.area) + grazing.intensity + log(patch.area):grazing.intensity, data = birds)
drop1(fit_short, .~., test = "F") 
#grazing.intensity:altitude not good

fit_short <- lm(abundance ~ log(patch.area) + grazing.intensity, data = birds)
drop1(fit_short, .~., test = "F") 
#grazing.intensity:altitude not good


summary(fit_short)

opar <- par(mfrow=c(2,2))
plot(fit_short)

summary(influence.measures(fit_short))

levels(birds$grazing.intensity)


#Should I fit a QUADRATIC? 
poly<- lm(abundance ~ log(patch.area) + I(log(patch.area)^2) + grazing.intensity, data = birds)

opar <- par(mfrow=c(2,2))
plot(poly)
par(opar)

summary(poly)

anova(df, poly)
anova(poly)
AIC(fit_short, poly) 

############
###GRAPHS###
############

#### raw (ha)
fit_short <- lm(abundance ~ log(patch.area), data = birds)

plot(abundance ~ patch.area, data = birds, xlab = "patch area (ha)", ylab = "Abundance", pch = 16,  xlim = c(0, 100))

xmin <- max(min(birds$patch.area, na.rm = TRUE), 1e-6)#chat
newx <- seq(xmin, 200, length.out = 400) # chat

pr.conf<- predict(fit_short, newdata = data.frame(patch.area=newx),  interval = "confidence")
matlines(newx, pr.conf, col=c("black", "red", "red"), lty=c(1))

pr.pred<- predict(fit_short, newdata = data.frame(patch.area=newx),  interval = "prediction")
matlines(newx, pr.pred, col=c("black", "red", "red"), lty=c(2))

####parallel

levels(birds$grazing.intensity)

#PARALLE GRAPH 
df <- lm(abundance ~ log(patch.area) + grazing.intensity,data = birds)
summary(df)


plot(abundance ~ jitter(log(patch.area)), pch=16, data = birds, xlab="Log(patch.area) in ha", ylab = "Abundance")

####CHAT
########
########
# Plot only LIGHT and HEAVY points
with(birds[birds$grazing.intensity %in% c("light","heavy"), ],
     plot(log(patch.area), abundance, pch = 16, 
          col = ifelse(grazing.intensity == "light", "red", "black"),
          xlab = "Log(patch.area)", ylab = "Abundance",
          ))
legend("topleft", 
       legend = c("Light grazing", "Heavy grazing"), 
       col = c("red", "black"), 
       pch = 16, lty = 1, lwd = 2, 
       bty = "n")  # bty="n" removes legend box
######
with(birds, {
  # First plot ALL points in gray, but set xlim to start at 0
  plot(log(patch.area), abundance,
       pch = 16, col = "gray",
       xlab = "log(patch.area)", ylab = "Abundance",
       xlim = c(0, max(log(patch.area), na.rm = TRUE)) )
  
  # Overlay Light grazing points in red
  points(log(patch.area)[grazing.intensity == "light"], 
         abundance[grazing.intensity == "light"], 
         pch = 16, col = "red")
  
  # Overlay Heavy grazing points in black
  points(log(patch.area)[grazing.intensity == "heavy"], 
         abundance[grazing.intensity == "heavy"], 
         pch = 16, col = "black")
})



######

#abline 'less than average'
abline(a=coef(fit_final)[1]+coef(fit_final)[6], b=coef(fit_final)[2], col="black")

#ABLINE 'light grazing'
abline(a=coef(fit_final)[1], b=coef(fit_final)[2], col="red")


######################################################################################################POLY 

#abundance vs patch graph (CI and PI) log POLY

fit.poly <- lm(abundance ~ log(patch.area)+ I(log(patch.area)^2), data = birds)

plot(abundance ~ log(patch.area), data = birds, xlab = "log(patch area)", ylab = "Abundance", pch = 16)

newx <- seq(min(log(birds$patch.area)), max(log(birds$patch.area)), length.out = 400)

pr.conf<- predict(fit.poly, newdata = data.frame(patch.area=exp(newx)),  interval = "confidence")
matlines(newx, pr.conf, col=c("black", "red", "red"), lty=c(1))

pr.pred<- predict(fit.poly, newdata = data.frame(patch.area=exp(newx)),  interval = "prediction")
matlines(newx, pr.pred, col=c("black", "red", "red"), lty=c(2))



#### raw POLY
fit.poly <- lm(abundance ~ log(patch.area)+ I(log(patch.area)^2), data = birds)

plot(abundance ~ patch.area, data = birds, xlab = "patch area (ha)", ylab = "Abundance", pch = 16,  xlim = c(0, 100))

xmin <- max(min(birds$patch.area, na.rm = TRUE), 1e-6)#chat
newx <- seq(xmin, 200, length.out = 400) # chat

pr.conf<- predict(fit.poly, newdata = data.frame(patch.area=newx),  interval = "confidence")
matlines(newx, pr.conf, col=c("black", "red", "red"), lty=c(1))

pr.pred<- predict(fit.poly, newdata = data.frame(patch.area=newx),  interval = "prediction")
matlines(newx, pr.pred, col=c("black", "red", "red"), lty=c(2))




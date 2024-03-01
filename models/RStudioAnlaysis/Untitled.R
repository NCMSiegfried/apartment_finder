attach(austin_tx_units)
par(mfrow=c(2,2),oma=c(1,1,1,1))

#convert Bath and Beds type to character
austin_tx_units$Baths = as.character(austin_tx_units$Baths)
austin_tx_units$Beds = as.character(austin_tx_units$Beds)

#mult linear regression: sq. footage, baths, beds
lm.fit = lm(MaxRent~SquareFootage+Baths+Beds)
summary(lm.fit)

#summary shows Square footage and #beds both are statistically significant bc of higher t-val and low p-val
#Square footage has lowest std. error meaning variation is lowest from predicted

plot(lm.fit)

plot(predict(lm.fit), rstudent(lm.fit))

#linear regression model: sq. footage 
lm.fit2 = lm(MaxRent~SquareFootage)
summary(lm.fit2)
plot(predict(lm.fit2), rstudent(lm.fit2))
plot(residuals(lm.fit2), pch = 10, col = "blue")
plot(MaxRent, SquareFootage, pch = 16, col = "blue")
abline(lm.fit2)
abline(lm.fit)
abline(lm.fit3)


#squared regression model: sq. footage
lm.fit3 = lm(MaxRent~(SquareFootage)^2)
summary(lm.fit3)
             
#Check colinearity and interaction between variables

lm.fit4 = lm(MaxRent~SquareFootage*Baths+ SquareFootage*Beds)
summary(lm.fit4)
plot(Beds, SquareFootage)

lm.fit5 = lm(MaxRent~Model)
summary(lm.fit5)

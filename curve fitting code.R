
#load
##################################################################
dose<- read.table("charts.csv",header=TRUE,sep=",")
head(dose)
p <- ggplot(data = dose,
            mapping = aes(
              x = X,
              y = Y))

#scatter plot
p + geom_point(size = 1, col = 'black') + 
  scale_x_continuous(limits=c(0, 100), breaks=seq(0,100,10)) +  
  scale_y_continuous(limits=c(-1, 1), breaks=seq(-1,1,0.2)) + 
  labs(
    title = "Greenness Intensity & Mental Health Response",
    x = "Eye-level Greenness (On-site)",
    y = "Mental Health Response"
  )  


#GAM
p + geom_point(size = 1, col = 'black') + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs ="cr",k=5), size = 1, col = 'blue') + 
  scale_x_continuous(limits=c(0, 100), breaks=seq(0,100,10)) +  
  scale_y_continuous(limits=c(-1, 1), breaks=seq(-1,1,0.2)) + 
  labs(
    title = "Greenness Intensity & Mental Health Response",
    x = "Eye-level Greenness (On-site)",
    y = "Mental Health Response"
  )  

fit <- gam(formula = Y ~ s(X, bs ="cr",k=5), data = dose)
summary(fit)
##################################################################



#fit & compare
##################################################################
#without log, lm
my.formula <- y ~ x
p + geom_point(size = 0.5, col = 'black') + 
  geom_smooth(method = "lm", formula = y ~ x, size = 1, col = 'blue') + 
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label..,
                                 sep = "~~~")))+
  scale_x_continuous(limits=c(0, 100), breaks=seq(0,100,10)) +  
  scale_y_continuous(limits=c(-1, 1), breaks=seq(-1, 1,0.2)) + 
  labs(
    title = "Greenness Intensity & Mental Health Response",
    x = "Eye-level Greenness (On-site)",
    y = "Mental Health Response"
  )   
fit <- lm(formula = Y ~ X, data = dose)
summary(fit)


#without log, quadratic
my.formula <- y ~ x + I(x^2)
p + geom_point(size = 0.5, col = 'black') + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, col = 'blue') + 
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label..,
                                 sep = "~~~")))+
  scale_x_continuous(limits=c(0, 100), breaks=seq(0,100,10)) +  
  scale_y_continuous(limits=c(-1, 1), breaks=seq(-1, 1,0.2)) + 
  labs(
    title = "Greenness Intensity & Mental Health Response",
    x = "Eye-level Greenness (On-site)",
    y = "Mental Health Response"
  )     
fit <- lm(formula = Y ~ X + I(X^2), data = dose)
summary(fit)



#without log, POWER
my.formula <- y ~ log(I(x^2))
p + geom_point(size = 0.5, col = 'black') + 
  geom_smooth(method = "glm", formula = y ~ log(I(x^2)), size = 1, col = 'blue') + 
  scale_x_continuous(limits=c(0, 100), breaks=seq(0,100,10)) +  
  scale_y_continuous(limits=c(-1, 1), breaks=seq(-1, 1,0.2)) + 
  labs(
    title = "Greenness Intensity & Mental Health Response",
    x = "Eye-level Greenness (On-site)",
    y = "Mental Health Response"
  )    

fit <- glm(formula = Y ~ log(I(X^2)), data = dose)
summary(fit)
rsq(fit,adj=FALSE,type=c('v','kl','sse','lr','n'))
rsq(fit,adj=TRUE,type=c('v','kl','sse','lr','n'))
##################################################################



#AIC & BIC
##################################################################
model1 <- lm(formula = Y~X, data = dose)
model2 <- lm(Y~log(I(X^2)), data = dose)
model3 <- lm(formula = Y~X + I(X^2), data = dose)
model4 <- gam(Y~s(X), data = dose)

models <- list(model1, model2, model3, model4)
mod.names <- c('linear','power','quadratic',"gam")
aictab(cand.set = models, modnames = mod.names)

AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)


BIC(model1)
BIC(model2)
BIC(model3)
BIC(model4)
##################################################################

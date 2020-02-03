import::here(grwth_county, multiplot, .from="data_cleaning.R")

train_data = grwth_county %>% dplyr::select(sex_ratio:pct_dep_class)

train_data = na.omit(train_data)
fit1 = lm(income ~ ., data=train_data)
summary(fit1)

# Alias analysis to remove linearly dependent variables
ld_vars <- attributes(alias(fit1)$Complete)$dimnames[[1]]
train_data = train_data %>% dplyr::select(-pct_occ05, -pct_cow04)

fit2 = lm(income ~ ., data=train_data)
summary(fit2)

# Vif Analysis

vif(fit2)

# removing pct_empd (2665.739821) with highest vif
train_data = train_data %>% dplyr::select(-pct_empd)
fit2 = lm(income ~ ., data=train_data)
summary(fit2)
vif(fit2)

# removing pct_occ01 (12.715096) with value greater than 10
train_data = train_data %>% dplyr::select(-pct_occ01)
fit3 = lm(income ~ ., data=train_data)
summary(fit3)

########################################################################################################
# Residual Diagnostics

#saved as Residual_1
p1 <- ggplot(mapping=aes(fit3$fitted.values,fit3$residuals)) + geom_point() + geom_smooth()
plot(p1)

#saved as Residual_2
p1 <- ggplot(mapping=aes(train_data$sex_ratio,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$pct_bpl,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2),2,2,byrow = TRUE)
multiplot(p1,p2,layout = layout)

##saved as Residual_3
p1 <- ggplot(mapping=aes(train_data$pct_occ02,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$pct_occ03,fit3$residuals)) + geom_point() + geom_smooth()
p3 <- ggplot(mapping=aes(train_data$pct_occ04,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout = layout)

#saved as Residual_4
p1 <- ggplot(mapping=aes(train_data$pct_lessthan_highgrad,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$pct_highschool_grad,fit3$residuals)) + geom_point() + geom_smooth()
p3 <- ggplot(mapping=aes(train_data$pct_bachdeg_or_highr,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout = layout)

#saved as Residual_5
p1 <- ggplot(mapping=aes(train_data$pct_cow01,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$pct_cow02,fit3$residuals)) + geom_point() + geom_smooth()
p3 <- ggplot(mapping=aes(train_data$pct_cow03,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout = layout)



#saved as Residual_6
p1 <- ggplot(mapping=aes(train_data$tax_in,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$tax_sr,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2),2,2,byrow = TRUE)
multiplot(p1,p2,layout = layout)

#saved as Residual_7
p1 <- ggplot(mapping=aes(train_data$pct_wrk_class,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$pct_dep_class,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2),2,2,byrow = TRUE)
multiplot(p1,p2,layout = layout)

###### Transformations ######
#log_income = log(train_data$income)
inc = train_data$income ** -0.5
train_data = cbind(train_data, inc)
train_data = train_data %>% dplyr::select(-income)
pct_bpl_2 = train_data$pct_bpl * train_data$pct_bpl
pct_sex_ratio_4 = train_data$sex_ratio ** 4
pct_wrk_class_3 = train_data$pct_wrk_class ** 3
pct_dep_class_2_inv = -1 * train_data$pct_dep_class ** 2
train_data = cbind(train_data, pct_bpl_2)
train_data = cbind(train_data, pct_sex_ratio_4)
train_data = cbind(train_data, pct_wrk_class_3)
train_data = cbind(train_data, pct_dep_class_2_inv)

fit4 = lm(inc ~ ., data=train_data)
summary(fit4)


qqnorm(fit4$residuals)
qqline(fit4$residuals)
res = fit4$residuals
train_data = train_data %>% cbind(res) %>% dplyr::filter(res >= -0.0007, res <= 0.0005) %>% dplyr::select(-res)
fit4 = lm(inc ~ ., data=train_data)
summary(fit4)
formula = ~ sex_ratio + pct_bpl + pct_occ02 + pct_occ03 + pct_occ04 + pct_lessthan_highgrad + pct_highschool_grad + pct_bachdeg_or_highr + pct_cow01 + pct_cow02 + pct_cow03 + tax_in + tax_sr + pct_wrk_class + pct_dep_class + pct_bpl_2 + pct_sex_ratio_4 + pct_wrk_class_3 + pct_dep_class_2_inv

############### Model selection ################
#fit0 = lm(log_income ~ 1, data=train_data)
fit0 = lm(inc ~ 1, data=train_data)
step(fit0, scope=list(lower=~1, upper=formula), k=log(nrow(train_data)))

subs = regsubsets(x=model.matrix(fit4), y=train_data$inc, nbest=1, nvmax=19)
rep = summary(subs)
model_size = seq(1:19)

p1 = ggplot(mapping=aes(model_size, rep$rsq)) + geom_line() + xlab("model size") + ylab("R^2")
p2 = ggplot(mapping=aes(model_size, rep$cp)) + geom_line() + xlab("model size") + ylab("Mallow's Cp")
p3 = ggplot(mapping=aes(model_size, rep$adjr2)) + geom_line() + xlab("model size") + ylab("Adjusted R^2")
p4 = ggplot(mapping=aes(model_size, rep$bic)) + geom_line() + xlab("model size") + ylab("BIC")

layout <- matrix(c(1,2,3,4),2,2,byrow = TRUE)
multiplot(p1,p2,p3,p4, layout = layout)

fit5 = lm(formula = inc ~ pct_bpl + pct_highschool_grad + pct_cow01 + 
            sex_ratio + pct_cow03 + pct_cow02 + pct_occ02 + pct_bachdeg_or_highr + 
            pct_wrk_class_3 + tax_in + pct_occ04 + pct_occ03 + pct_sex_ratio_4 + 
            pct_dep_class_2_inv + pct_dep_class + tax_sr, data = train_data)


############## Checking for multivariate outliers  ##############
resid = fit5$residuals
M <- model.matrix(fit5)
H <- M%*%solve(t(M)%*%M)%*%t(M)
leverage<-diag(H)
t_value<-resid*sqrt((nrow(train_data)-17-1)/ (sum(resid^2)*(1-leverage)-resid^2))
t_critical = qt(1-.10/(2*nrow(train_data)), nrow(train_data) - 17 - 1)

ggplot() + geom_point(mapping=aes(fit5$fitted.values, t_value)) + xlab("fitted values") + ylab("studentized deleted residuals")

plot(fit5$fitted.values, t_value)
lev_critical = 2*17/nrow(train_data)
df_fits = t_value * sqrt(leverage/(1 - leverage))
df_fits_critical = 2* sqrt(17/nrow(train_data))

dd = cbind(train_data, leverage)
dd = cbind(dd, df_fits)
dd = dd %>% dplyr::filter(leverage < lev_critical) %>% dplyr::select(-leverage)
dd = dd %>% dplyr::filter(df_fits < df_fits_critical) %>% dplyr::select(-df_fits)


fit6 = lm(formula = inc ~ pct_bpl + pct_highschool_grad + pct_cow01 + 
            sex_ratio + pct_cow03 + pct_cow02 + pct_occ02 + pct_bachdeg_or_highr + 
            pct_wrk_class_3 + tax_in + pct_occ04 + pct_occ03 + pct_sex_ratio_4 + 
            pct_dep_class_2_inv + pct_dep_class + tax_sr, data = dd)

summary(fit6)
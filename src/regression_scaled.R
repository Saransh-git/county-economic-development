import::here(grwth_county, multiplot, .from="data_cleaning.R")

train_data = grwth_county %>% dplyr::select(sex_ratio:pct_dep_class)

train_data = na.omit(train_data)
train_data_2 = as.tibble(scale(train_data))

train_data_2 = train_data_2 %>% dplyr::select(-pct_occ05, -pct_cow04)
fit1 = lm(income ~ . -1, data = train_data_2)

train_data_2 = train_data_2 %>% dplyr::select(-pct_empd)
train_data_2 = train_data_2 %>% dplyr::select(-pct_occ01)

pct_bpl_2 = train_data_2$pct_bpl ** 2
pct_sex_ratio_4 = train_data_2$sex_ratio ** 4
pct_wrk_class_3 = train_data_2$pct_wrk_class ** 3
pct_dep_class_2_inv = -1 * train_data_2$pct_dep_class ** 2
train_data_2 = cbind(train_data_2, pct_bpl_2)
train_data_2 = cbind(train_data_2, pct_sex_ratio_4)
train_data_2 = cbind(train_data_2, pct_wrk_class_3)
train_data_2 = cbind(train_data_2, pct_dep_class_2_inv)

fit = lm(income ~ -1 + ., data=train_data_2)
summary(fit)

qqnorm(fit$residuals)
qqline(fit$residuals)

res = fit$residuals
train_data_2 = train_data_2 %>% cbind(res) %>% dplyr::filter(res >= -0.8, res <= 0.5) %>% select(-res)
fit2 = lm(income ~ -1 + ., data=train_data_2)

formula = ~ -1 + sex_ratio + pct_bpl + pct_occ02 + pct_occ03 + pct_occ04 + pct_lessthan_highgrad + pct_highschool_grad + pct_bachdeg_or_highr + pct_cow01 + pct_cow02 + pct_cow03 + tax_in + tax_sr + pct_wrk_class + pct_dep_class + pct_bpl_2 + pct_sex_ratio_4 + pct_wrk_class_3 + pct_dep_class_2_inv

fit0 = lm(income ~ -1, data=train_data_2)
step(fit0, scope=list(lower=~-1, upper=formula), k=log(nrow(train_data_2)))

fit2 = lm(formula = income ~ pct_bpl + pct_highschool_grad + pct_cow01 + 
            pct_dep_class_2_inv + sex_ratio + pct_bachdeg_or_highr + 
            pct_occ02 + pct_cow02 + pct_bpl_2 + pct_cow03 + pct_wrk_class + 
            tax_in + pct_sex_ratio_4 + pct_occ04 + pct_occ03 + pct_wrk_class_3 - 
            1, data = train_data_2)


resid = fit2$residuals
M <- model.matrix(fit2)
H <- M%*%solve(t(M)%*%M)%*%t(M)
leverage<-diag(H)
t_value<-resid*sqrt((nrow(train_data_2)-16-1)/ (sum(resid^2)*(1-leverage)-resid^2))
t_critical = qt(1-.10/(2*nrow(train_data_2)), nrow(train_data_2) - 16 - 1)
## no outlying observations in Y found
lev_critical = 2*16/nrow(train_data_2)
df_fits = t_value * sqrt(leverage/(1 - leverage))
df_fits_critical = 2* sqrt(16/nrow(train_data_2))


dd = cbind(train_data_2, leverage)
dd = cbind(dd, df_fits)
dd = dd %>% dplyr::filter(df_fits < df_fits_critical, leverage < lev_critical) %>% select(-df_fits, -leverage)

fit3 = lm(formula = income ~ pct_bpl + pct_highschool_grad + pct_cow01 + 
            pct_dep_class_2_inv + sex_ratio + pct_bachdeg_or_highr + 
            pct_occ02 + pct_cow02 + pct_bpl_2 + pct_cow03 + pct_wrk_class + 
            tax_in + pct_sex_ratio_4 + pct_occ04 + pct_occ03 + pct_wrk_class_3 - 
            1, data = dd)
summary(fit3)

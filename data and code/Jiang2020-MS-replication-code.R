library(data.table)
library(ggplot2)
library(lfe)
library(stargazer)

load('Jiang2020_Bunching_synthetic_data.rda')

#### Table 1: Summary stats ####
data[, list(mean(highcredit), quantile(highcredit, c(0.25, 0.5, 0.75)))]
data[, list(mean(terms/12), quantile(terms/12, c(0.25, 0.5, 0.75)))]
data[, list(mean(payment), quantile(payment, c(0.25, 0.5, 0.75)))]
data[, list(mean(int), quantile(int, c(0.25, 0.5, 0.75)))]

data[, list(mean(vantage), quantile(vantage, c(0.25, 0.5, 0.75)))]
data[, list(mean(black), quantile(black, c(0.25, 0.5, 0.75)))]
data[, list(mean(hispanic), quantile(hispanic, c(0.25, 0.5, 0.75)))]

summary(data)
#### Figure 1: Freq of payment ending ####
data[, payment_level:=round((payment+0.1)/10)*10]

try = data[payment%%10==9, list(length(int)), by= payment%%100]

temp = data[payment_level%%10==0, list(length(int)), by= payment%%10][, V1:=V1/sum(V1)]
ggplot(temp) +
  geom_bar(aes(x = as.factor(payment), y = V1),
           width = 0.8, stat = 'identity')+
  scale_x_discrete(labels = paste('$',seq(0,9), sep = '')) +
  scale_y_continuous(labels = scales::percent) +
  ylab('Percentage of Loans') +
  xlab('Monthly Payment Ending Digit (over $100)') +
  theme_bw()
ggsave(file="pic/Figure1.pdf", width = 3, height = 3)




#### Figure 10: Freq of payment ending ####
data[, payment_level:=round((payment+0.1)/10)*10]

try_once = data[payment%%10==9&payment>=400&payment<=2000, list(length(int))]

try_own = data[payment%%10==9&payment>=400&payment<=2000, list(length(int)), by= payment%%100][, V1:=V1/sum(V1)]
ggplot(try_own) +
  geom_bar(aes(x = as.factor(payment), y = V1),
           width = 0.8, stat = 'identity')+
  scale_y_continuous(labels = scales::percent) +
  ylab('Percentage of Loans') +
  xlab('9-ending Payment(>400)') +
  theme_bw()
ggsave(file="pic/Figure_9ending_over400.pdf", width = 3, height = 3)




#### Figure 11: Freq of payment ending ####
data[, payment_level_new:=floor((payment+0.1)/10)*10]

try_once = data[payment_level_new%%100==00&payment>=900&payment<=2000, list(length(int)), by= payment%%100]

try_own_2 = data[payment_level_new%%100==80&payment>=000&payment<=2000, list(length(int)), by= payment%%100][, V1:=V1/sum(V1)]
ggplot(try_own_2) +
  geom_bar(aes(x = as.factor(payment), y = V1),
           width = 0.8, stat = 'identity')+
  scale_y_continuous(labels = scales::percent) +
  ylab('Percentage of Loans') +
  xlab('Monthly Payment Ending Digit') +
  theme_bw()
ggsave(file="pic/Figure_for_80interval.pdf", width = 3, height = 3)














#### Table 2: Heterogeneous payment bunching ####
data[, vantage_gp:=cut(vantage, c(seq(620, 780, by=40),850), include.lowest = T)]
data[, hispanic_gp:=cut(hispanic, c(0, 0.02, 0.05, 0.1, 0.2,Inf), include.lowest = T)]
data[, black_gp:=cut(black, c(0, 0.02, 0.05, 0.1, 0.2,Inf), include.lowest = T)]

# level of $9 bunching
data[payment_level%%100==0, sum(payment%%10==9)/sum(payment%%10==1)]
data[payment_level%%100==0, sum(payment%%10==9)/sum(payment%%10==1), by=vantage_gp][order(vantage_gp)]

data[payment_level%%100==0, sum(payment%%10==9)/sum(payment%%10==1), by=hispanic_gp][order(hispanic_gp)]
data[payment_level%%100==0, sum(payment%%10==9)/sum(payment%%10==1), by=black_gp][order(black_gp)]

# level of $0 bunching
data[payment_level%%100==0, sum(payment%%10==0)/sum(payment%%10==1), by=vantage_gp][order(vantage_gp)]
data[payment_level%%100==0, sum(payment%%10==0)/sum(payment%%10==1), by=hispanic_gp][order(hispanic_gp)]
data[payment_level%%100==0, sum(payment%%10==0)/sum(payment%%10==1), by=black_gp][order(black_gp)]

#### Table 3: Loan characteristics w/ ending digits ####
data[, list(round(mean(int)*100,3), round(mean(vantage),2), round(mean(highcredit),2), round(mean(terms),3)),
            by=list(payment%%10)][order(payment)]

#### Table 4: Interest rate regression ####
data[, type:=payment%%10]

data[, hc_gp:=cut(highcredit, c(0,seq(10.5,59.5,by=0.5),Inf), labels = F, include.lowest = T)]
data[, terms_gp:=cut(terms, seq(2,8,by=0.1), labels = F, include.lowest = T)]
data[, payment:=payment/1000]

data[, int:=int*100]

data[, `:=`(black_9=0, hispanic_9=0)]
data[type==9, black_9:=black]
data[type==9, hispanic_9:=hispanic]

# note that the regression in the actual data contains some additional variables
# which are not in this synthetic data
m1 = felm(int ~ as.factor(type) + vantage + I(vantage^2) + terms + I(terms^2) +
            highcredit + I(highcredit^2), data = data)
m2 = felm(int ~ as.factor(type)|vantage+hc_gp+terms_gp, data = data)
m3 = felm(int ~ as.factor(type) + vantage + I(vantage^2) + terms + I(terms^2) + 
            highcredit + I(highcredit^2)+black+hispanic, data = data)
m4 = felm(int ~ as.factor(type)+black+hispanic|vantage+hc_gp+terms_gp, data = data)
m5 = felm(int ~ as.factor(type) + vantage + I(vantage^2) + terms + I(terms^2) + 
            highcredit + I(highcredit^2)+ black+hispanic +black_9 + hispanic_9, data = data)

stargazer(m1,m2,m3,m4,m5, type='text', no.space=T, digits = 4, omit.stat=c('ser'),
          dep.var.labels = c('interest rate'))

#### Figure 2: Interest rate with ending digits ####
temp = cbind(0:9, c(0,m1$coefficients[2:10]))
temp = as.data.table(temp)
temp[, V2:=V2+mean(data$int) - mean(V2)]
temp[, V1:=paste('$',V1, sep = '')]
ggplot(temp) +
  geom_point(aes(x = as.factor(V1), y = V2, group = 1)) +
  geom_line(aes(x = as.factor(V1), y = V2, group = 1), linetype='dotted') +
  scale_color_discrete(guide='none') +
  scale_x_discrete(limits = paste('$',c(seq(5,9), seq(0,4)), sep = '')) +
  # scale_y_continuous(labels = scales::percent, limits = c(0.04743, 0.04837)) + 
  xlab('payment ending digit') +
  ylab('interest rate') +
  theme_bw()
ggsave(file="pic/Figure2.pdf", width = 3, height = 3)

#### Figure 3: Examples of perceived payment function ####
theta_c = 0.1
plot_data = as.data.table(cbind(p = seq(300,400)))
plot_data[, p100:=100*floor(p/100)]
plot_data[, p10:=10*floor((p-p100)/10)]
plot_data[, p1:=p-p100-p10]
plot_data[, p_c:=p100+(1-theta_c)*p10+(1-theta_c)^2*p1]

ggplot(plot_data) +
  geom_line(aes(x = p, y = p), linetype = 'dotted') +
  geom_point(aes(x = p, y = p_c), color='blue3', size = 0.5) +
  geom_line(aes(x = p, y = p_c), color='blue3') +
  ylab('perceived payment') +
  xlab('actual payment') +
  theme_bw() 
ggsave(file="pic/Figure3A.pdf", width = 3, height = 3)

theta_c = 0.15
plot_data = as.data.table(cbind(p = seq(300,400)))
plot_data[, p100:=100*floor(p/100)]
plot_data[, p10:=10*floor((p-p100)/10)]
plot_data[, p1:=p-p100-p10]
plot_data[, p_c:=p100+(1-theta_c)*p10+(1-theta_c)^2*p1]

ggplot(plot_data) +
  geom_line(aes(x = p, y = p), linetype = 'dotted') +
  geom_point(aes(x = p, y = p_c), color='blue3', size = 0.5) +
  geom_line(aes(x = p, y = p_c), color='blue3') +
  ylab('perceived payment') +
  xlab('actual payment') +
  theme_bw() 
ggsave(file="pic/Figure3B.pdf", width = 3, height = 3)

#### Figure 4: Simulated freq of payment ending digit ####
load('Jiang2020_Bunching_synthetic_data.rda')

source('Jiang2020_Bunching_functions.R')

data[, min_p:=payment - 30]
data[, max_p:=payment + 30]

X = as.matrix(data[, list(black, hispanic)])
n_X = dim(X)[2]

data = data[, list(min_p, max_p)]

a_draw = runif(dim(data)[1], -1, 1)
p_out = numeric(length = dim(data)[1])

p_consumer_bias = get_p_calc(par = c(0, 0.00015, 0, -0.0001, -1, 0, 0, 0), data = data, X = X, a_draw)
p_FM_bias = get_p_calc(par = c(0, 0, 0, 0.0001, -1, 0, 0, 0), data = data, X = X, a_draw)
p_both_bias = get_p_calc(par = c(0, 0.025, 0, 0.00001, -1, 0, 0, 0), data = data, X = X, a_draw)

data$p_consumer_bias = p_consumer_bias
data$p_FM_bias = p_FM_bias
data$p_both_bias = p_both_bias

temp = data[, list(length(min_p), mean(alpha)), by=p_consumer_bias%%10][order(p_consumer_bias)][, V1:=V1/sum(V1)]
setnames(temp, 'p_consumer_bias', 'p')
temp[p%%10==9, type:=1]
temp[p%%10==0, type:=2]
temp[is.na(type), type:=3]
temp[, bias_type:='  consumer bias only']

plot_data = temp

temp = data[, list(length(min_p), mean(alpha)), by=p_FM_bias%%10][order(p_FM_bias)][, V1:=V1/sum(V1)]
setnames(temp, 'p_FM_bias', 'p')
temp[p%%10==9, type:=1]
temp[p%%10==0, type:=2]
temp[is.na(type), type:=3]
temp[, bias_type:=' finance manager bias only']

plot_data = rbind(plot_data, temp)

temp = data[, list(length(min_p), mean(alpha)), by=p_both_bias%%10][order(p_both_bias)][, V1:=V1/sum(V1)]
setnames(temp, 'p_both_bias', 'p')
temp[p%%10==9, type:=1]
temp[p%%10==0, type:=2]
temp[is.na(type), type:=3]
temp[, bias_type:='both have biases']
plot_data = rbind(plot_data, temp)

ggplot(plot_data) +
  geom_bar(aes(x = as.factor(p), y = V1,
               fill = as.factor(type)),  width=0.8, stat = 'identity') +
  scale_fill_manual(name='', labels=c('$9-ending ','$0-ending ','others'),
                    values = c('blue3','orangered2','darkolivegreen')) +
  scale_x_discrete(breaks = seq(0,9,by=1), labels = paste('$',seq(0,9,by=1), sep='')) +
  facet_grid(.~bias_type) +
  xlab('Monthly Payment Ending Digit') +
  ylab('Percentage of Loans') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill='none') +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.05, 0.1, 0.15)) +
  coord_cartesian(ylim=c(0.05, 0.12))
ggsave(file="pic/Figure4.pdf", width = 8, height = 2.5)


#### Figure 5: Simulated bunching and bargaining power ####
load('Jiang2020_Bunching_synthetic_data.rda')

source('Jiang2020_Bunching_functions.R')

X = as.matrix(data[, list(black, hispanic)])
n_X = dim(X)[2]

data = data[, list(min_p, max_p)]

a_draw = runif(dim(data)[1], 0, 1)

## consumer bias is higher
p_both_bias = get_p_calc_simu(data, a_draw, c(0.0000, 0.02, 0, -0.0005))

data$p_both_bias = p_both_bias
data[, length(min_p), by=p_both_bias%%10][order(p_both_bias)]
hist(data$alpha)

temp = data[alpha>0.5 & alpha<0.9, list(mean(alpha)), by=list(p_both_bias%%10)][order(p_both_bias)]
setnames(temp, 'p_both_bias', 'p')
temp[p%%10==9, type:=1]
temp[p%%10==0, type:=2]
temp[is.na(type), type:=3]
temp[, p:=paste('$',p, sep = '')]

ggplot(temp) +
  geom_point(aes(x = as.factor(p), y = V1, color=as.factor(type), shape=as.factor(type)), size = 3) +
  geom_line(aes(x = as.factor(p), y = V1, group=1), linetype='dashed') +
  scale_x_discrete(limits = paste('$', c(seq(5,9),seq(0,4)), sep = '')) +
  scale_y_continuous(limits = c(0.692, 0.708)) +
  scale_color_manual(name='', labels=c('$9-ending ','$0-ending  ','others'),
                     values = c('blue3','orangered2','darkolivegreen')) +
  scale_shape_manual(name='', labels=c('$9-ending ','$0-ending  ','others'),
                     values = c(17,15,16)) +
  xlab('Monthly Payment Ending Digit') +
  ylab('Bargaining Power') +
  theme_bw() +
  theme(legend.position='bottom', 
        panel.grid.major = element_blank()) +
  guides(color = guide_legend(nrow=1))
ggsave(file="pic/Figure5A.pdf", width = 3.5, height = 3)

temp = data[alpha>0.1 & alpha<0.5, list(mean(alpha)), by=list(p_both_bias%%10)][order(p_both_bias)]
setnames(temp, 'p_both_bias', 'p')
temp[p%%10==9, type:=1]
temp[p%%10==0, type:=2]
temp[is.na(type), type:=3]
temp[, p:=paste('$',p, sep = '')]

ggplot(temp) +
  geom_point(aes(x = as.factor(p), y = V1, color=as.factor(type), shape=as.factor(type)), size = 3) +
  geom_line(aes(x = as.factor(p), y = V1, group=1), linetype='dashed') +
  scale_x_discrete(limits = paste('$', c(seq(5,9),seq(0,4)), sep = '')) +
  scale_y_continuous(limits = c(0.29, 0.305)) +
  scale_color_manual(name='', labels=c('$9-ending ','$0-ending  ','others'),
                     values = c('blue3','orangered2','darkolivegreen')) +
  scale_shape_manual(name='', labels=c('$9-ending ','$0-ending  ','others'),
                     values = c(17,15,16)) +
  xlab('Monthly Payment Ending Digit') +
  ylab('Bargaining Power') +
  theme_bw() +
  theme(legend.position='bottom', 
        panel.grid.major = element_blank()) +
  guides(color = guide_legend(nrow=1))
ggsave(file="pic/Figure5B.pdf", width = 3.5, height = 3)


## FM bias is higher
p_both_bias = get_p_calc_simu(data, a_draw, c(0.0000, 0.02, 0, 0.0005))

data$p_both_bias = p_both_bias

temp = data[alpha>0.5 & alpha<0.9, list(mean(alpha)), by=list(p_both_bias%%10)][order(p_both_bias)]
setnames(temp, 'p_both_bias', 'p')
temp[p%%10==9, type:=1]
temp[p%%10==0, type:=2]
temp[is.na(type), type:=3]
temp[, p:=paste('$',p, sep = '')]

ggplot(temp) +
  geom_point(aes(x = as.factor(p), y = V1, color=as.factor(type), shape=as.factor(type)), size = 3) +
  geom_line(aes(x = as.factor(p), y = V1, group=1), linetype='dashed') +
  scale_x_discrete(limits = paste('$', c(seq(5,9),seq(0,4)), sep = '')) +
  scale_y_continuous(limits = c(0.692, 0.708)) +
  scale_color_manual(name='', labels=c('$9-ending ','$0-ending  ','others'),
                     values = c('blue3','orangered2','darkolivegreen')) +
  scale_shape_manual(name='', labels=c('$9-ending ','$0-ending  ','others'),
                     values = c(17,15,16)) +
  xlab('Monthly Payment Ending Digit') +
  ylab('Bargaining Power') +
  theme_bw() +
  theme(legend.position='bottom', 
        panel.grid.major = element_blank()) +
  guides(color = guide_legend(nrow=1))
ggsave(file="pic/Figure5C.pdf", width = 3.5, height = 3)

temp = data[alpha>0.1 & alpha<0.5, list(mean(alpha)), by=list(p_both_bias%%10)][order(p_both_bias)]
setnames(temp, 'p_both_bias', 'p')
temp[p%%10==9, type:=1]
temp[p%%10==0, type:=2]
temp[is.na(type), type:=3]
temp[, p:=paste('$',p, sep = '')]

ggplot(temp) +
  geom_point(aes(x = as.factor(p), y = V1, color=as.factor(type), shape=as.factor(type)), size = 3) +
  geom_line(aes(x = as.factor(p), y = V1, group=1), linetype='dashed') +
  scale_x_discrete(limits = paste('$', c(seq(5,9),seq(0,4)), sep = '')) +
  scale_y_continuous(limits = c(0.29, 0.305)) +
  scale_color_manual(name='', labels=c('$9-ending ','$0-ending  ','others'),
                     values = c('blue3','orangered2','darkolivegreen')) +
  scale_shape_manual(name='', labels=c('$9-ending ','$0-ending  ','others'),
                     values = c(17,15,16)) +
  xlab('Monthly Payment Ending Digit') +
  ylab('Bargaining Power') +
  theme_bw() +
  theme(legend.position='bottom', 
        panel.grid.major = element_blank()) +
  guides(color = guide_legend(nrow=1))
ggsave(file="pic/Figure5D.pdf", width = 3.5, height = 3)


#### Table 5 & 6: Monte Carlo simulation & Estimation results ####
# The estimation procedure is the same as what's outlined in the Monte Carlo simulation
# The only difference is that I will need to calculate the reservation value
# The code below shows how I calculate the reservation value

# the code below will take a while to run
source('Jiang2020_Bunching_Estimation.R')

# reservation values are calculated using quantile regression 2.5% - 97.5%
library(quantreg)

load('Jiang2020_Bunching_synthetic_data.rda')

# Note that the actual estimation in the paper includes month FE in the regression,
# which is not included in the synthetic data set

m = rq(int ~ vantage + I(vantage^2) + terms + I(terms^2) + highcredit + I(highcredit^2) - 1,
       tau = 0.975, method = 'sfn', data = data)

m1 = rq(int ~ vantage + I(vantage^2) + terms + I(terms^2) + highcredit + I(highcredit^2)-1,
        tau = 0.025, method = 'sfn', data = data)

data$int_975 = m$fitted.values
data$int_025 = m1$fitted.values

data[, max_p_975:=round(1000*highcredit*int_975/12*(1+int_975/12)^(terms*12)/((1+int_975/12)^(terms*12)-1))]
data[, min_p_025:=round(1000*highcredit*int_025/12*(1+int_025/12)^(terms*12)/((1+int_025/12)^(terms*12)-1))]

data[, `:=`(int_975=NULL, int_025=NULL)]

data[, max_p_975:=as.integer(max_p_975)]
data[, min_p_025:=as.integer(min_p_025)]

data[, min_p:=min_p_025]
data[, max_p:=max_p_975]

data = data[payment>min_p & payment<max_p]

# Note that in the actual data, the sample size is much larger (35 million loans)
# The estimation sample is randomly sampled from the whole data
# The commented-out code below does the random sampling

# set.seed(1)
# data[, idx:=1:.N]
# data = data[idx %in% sample(seq(1, to = max(data$idx)), size = 1e6, replace = F)]

data = data[, list(payment, min_p, max_p, vantage, highcredit, terms, black, hispanic)]

#### Figure 6: Distribution of payments for actual and simulated data ####
## plot simulated payment distribution
load('par.rda')

load('Jiang2020_Bunching_synthetic_data.rda')

X = as.matrix(data[, list(black, hispanic)])
n_X = dim(X)[2]

X_bias = as.matrix(data[, list(black, hispanic)])

a_draw = runif(dim(data)[1], 0, 1)

p_calc = get_p_calc_het_bias(par, data, X, X_bias, a_draw)
data$p_calc = p_calc

temp = data[, length(min_p), by=p_calc]
temp2 = data[, length(min_p), by=payment]
library(ggplot2)
ggplot(temp[p_calc>=245 & p_calc<=535]) +
  geom_point(aes(x = p_calc, y = V1, color = 'simulated', shape = 'simulated'),size = 1.5)+
  geom_line(aes(x = p_calc, y = V1), linetype = 'dotted') +
  geom_point(data = temp2[payment>=245 & payment<=535], aes(x = payment, y = V1, color = 'actual', shape = 'actual'),size = 1.5)+
  geom_line(data = temp2[payment>=245 & payment<=535], aes(x = payment, y = V1, color = 'actual'), linetype = 'dotted')+
  scale_color_manual(name='', labels = c('True Data','Simulated Data'),
                     values = c('darkgrey','blue'))+
  scale_shape_discrete(name='', labels = c('True Data','Simulated Data')) +
  xlab('Scheduled Monthly Payment') +
  ylab('Number of Loans') +
  theme_bw() +
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank())
ggsave('pic/Figure6.pdf', width = 5, height = 4)


#### Table 7: Effect of left-digit bias on payments ####

source('Jiang2020_Bunching_CF.R')

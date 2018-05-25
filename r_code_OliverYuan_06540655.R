## Oliver Yuan
## 06540655

################ load packages #######

library(tidyverse)


################### Question 1 ########################

##### read file #####

Q1data <- read.csv("Q1data.csv")

#### check data types and missing values

print(sapply(Q1data, class))

print(is.na(Q1data))

#### Model1 ####

Q1_Model1 <- lm(sales ~ square.feet + advertising + average.income + No.of.competing.stores, data = Q1data)

summary(Q1_Model1)

#### Model2 ####

Q1_Model2 <- lm(log(sales) ~ square.feet + log(advertising) + average.income +No.of.competing.stores, data = Q1data)

summary(Q1_Model2)

#### Compute elasticity of demand w.r.t advertising #### 

sale1 <- 356.3719-4.9285*mean(Q1data$square.feet)+0.5378*mean(Q1data$advertising)+0.9732*mean(Q1data$average.income)-31.0879*mean(Q1data$No.of.competing.stores)

advertising1 <- mean(Q1data$advertising)

## elasticity = derivative(sales/advertising)*advertising/sales

E_ad_sale <- 0.5378*advertising1/sale1

print(E_ad_sale)

#### Model3 ####

## create dataset

Q1data_mod3 <- Q1data %>% 
  mutate(ad_income = log(advertising)*average.income) %>% 
  select(1,2,3,5,6)

Q1_model3 <- lm(log(sales) ~ square.feet + No.of.competing.stores + log(advertising) + ad_income, data = Q1data_mod3)

summary(Q1_model3)

## because both the right-hand side and the left-hand side are natural logarithm 
## the elasticity equals coefficient

elasticity.income.8 = 1.256228+0.001124*8
ealsticity.income.16 = 1.256228+0.001124*16

## create dataset for plot

Q1_plotdata <- Q1data_mod3 %>% 
  select(2,3,4,5)

Q1_plotdata["square.feet"] = mean(Q1_plotdata$square.feet)
Q1_plotdata["No.of.competing.stores"] = mean(Q1_plotdata$No.of.competing.stores)
Q1_plotdata["ad_income"] = mean(Q1_plotdata$ad_income)

# use model to predict sales

pred.y <- predict(Q1_model3,Q1_plotdata)

Q1_plotdata['pred.sale'] = pred.y
Q1_plotdata["advertising"] = log(Q1_plotdata$advertising)

# create plot

ggplot(Q1_plotdata) +
  geom_point(aes(x = advertising, y = pred.sale))+
  ylab("Predicted Sales") + xlab("Advertising") + 
  ggtitle("Sales and advertising are positively correlated")

################### Question 2 ########################

Q2data <- read.csv("Q2data.csv")

#### Question (a) build logit model

Q2_model1 <- glm(choice ~ price + inventory, data = Q2data, family = "binomial")

summary(Q2_model1)

#### Question(b) calculate probability

pred.data =data.frame(price = 20, inventory = mean(Q2data$inventory))
pred.prob <- predict(Q2_model1,pred.data,type = "response")

print(pred.prob)

#### Question (c)

Q2data["choice_prob"] <- ifelse(Q2data$choice == 1,
                            exp(9.86049-0.32684*Q2data$price-0.15286*Q2data$inventory)/(1+exp(9.86049-0.32684*Q2data$price-0.15286*Q2data$inventory)),
                            1/(1+exp(9.86049-0.32684*Q2data$price-0.15286*Q2data$inventory)))

Q2data["log_choice_prob"] <- log(Q2data$choice_prob)

sum(Q2data$log_choice_prob) # log-likelihood

Q2data["new_choice_prob"] <- ifelse(Q2data$choice == 1,
                                exp(9.86049-0.32684*Q2data$price-0.35286*Q2data$inventory)/(1+exp(9.86049-0.32684*Q2data$price-0.35286*Q2data$inventory)),
                                1/(1+exp(9.86049-0.32684*Q2data$price-0.35286*Q2data$inventory)))

Q2data["new_log_choice_prob"] <- log(Q2data$new_choice_prob)

sum(Q2data$new_log_choice_prob) # new log-likelihood

#### Question (d) to (g) elasticity computation

## define function to calculate elasticity

logit_elas <- function(price, inventory) {
  -0.32684*price*(1-exp(9.86049-0.32684*price-0.15286*inventory)/(1+exp(9.86049-0.32684*price-0.15286*inventory)))
  
}

## apply function to all combinations 

elas.data <- data.frame(price = c(15,15,25,25),inventory = c(50,10,50,10))
elas.data["elas"] = logit_elas(elas.data$price,elas.data$inventory)

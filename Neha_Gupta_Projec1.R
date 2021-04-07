setwd("C:/Users/Neha/Desktop/BU/bs730/project1")

## Part 1
## Ques A
proj1<- read.csv('jhst_proj1sp21(1) (1).csv', header = TRUE)


## Ques B
proj1$agecat <- cut(x = proj1$Age, breaks = c(0, 30,50, 70, Inf), labels = c(1,2,3,4))
table(proj1$agecat) #for category of age


proj1$darkgrnvegQ2 <- ifelse(proj1$Darkgrnveg >= 0.5, 1, 0)
table(proj1$darkgrnvegQ2) #for dark green vegetable category

proj1$darkgrnvegQ3 <- ifelse(proj1$Darkgrnveg >= 1.03, 1,0)
table(proj1$darkgrnvegQ3)


proj1$income2cat[proj1$Income == 1 | proj1$Income ==2] <- 1    
proj1$income2cat[proj1$Income == 3 | proj1$Income ==4] <- 0
table(proj1$income2cat) #for income category

#Dessert
proj1$desert <- ifelse(proj1$nbK3FavorFoodstore >0, 0, 1)
table(proj1$desert) #for desert category

#the don't know values as NA
proj1$Diabetes[proj1$Diabetes == 9] <- NA
proj1$Currentsmoker[proj1$Currentsmoker == 9] <- NA
proj1$Income[proj1$Income == 999] <- NA

#excluding the missing values from the table
proj1 <- proj1[(!is.na(proj1$Darkgrnveg)) & (!is.na(proj1$Income)) & (!is.na(proj1$nbmedHHincome)) & 
                 (!is.na(proj1$nbpctPoverty)) & (!is.na(proj1$nbSESanascore)) & 
                 (!is.na(proj1$nbK3FavorFoodstore)) & (!is.na(proj1$nbK3paFacilities)) & 
                 (!is.na(proj1$Activeindex)) & (!is.na(proj1$Dailydiscr)) & 
                 (!is.na(proj1$Perceivedstress)), ]


dim(proj1)##2121,27
number = table(proj1$darkgrnvegQ2) #total number
prop.table(number) #low and high level dark green vegetable number

## Part 2
#Ques A
#1st row age(years)
mean(proj1$Age)
sd(proj1$Age)
tapply(proj1$Age, proj1$darkgrnvegQ2, FUN = mean)
tapply(proj1$Age, proj1$darkgrnvegQ2, FUN = sd)
t.test(proj1$Age ~ proj1$darkgrnvegQ2, var.equal = TRUE)#aasume variance is true for all in this part


##2nd row
#table age cat wise
agecat_table <- table(proj1$agecat)
agecat_table
prop.table(agecat_table)

#table consumption wise
agecat_consumption <- table(proj1$agecat, proj1$darkgrnvegQ2)
agecat_consumption
prop.table(agecat_consumption, 2)
chisq.test(proj1$agecat , proj1$darkgrnvegQ2, correct = FALSE)


##male sex

male <- table(proj1$Sex == 'Male')
male
prop.table(male)
male_consumption <- table(proj1$Sex=='Male', proj1$darkgrnvegQ2)
male_consumption
prop.table(male_consumption,2)
t.test(proj1$Sex=='Male' ~ proj1$darkgrnvegQ2, var.equal = TRUE)


##BMI
mean(proj1$BMI)
sd(proj1$BMI)
tapply(proj1$BMI, proj1$darkgrnvegQ2, FUN = mean)
tapply(proj1$BMI, proj1$darkgrnvegQ2, FUN = sd)
t.test(proj1$BMI ~ proj1$darkgrnvegQ2, var.equal = TRUE) 

##activity index
mean(proj1$Activeindex)
sd(proj1$Activeindex)
tapply(proj1$Activeindex, proj1$darkgrnvegQ2, FUN = mean)
tapply(proj1$Activeindex, proj1$darkgrnvegQ2, FUN = sd)
t.test(proj1$Activeindex ~ proj1$darkgrnvegQ2, var.equal = TRUE)

##Daily discrimination
mean(proj1$Dailydiscr)
sd(proj1$Dailydiscr)
tapply(proj1$Dailydiscr, proj1$darkgrnvegQ2, FUN = mean)
tapply(proj1$Dailydiscr, proj1$darkgrnvegQ2, FUN = sd)
t.test(proj1$Dailydiscr ~ proj1$darkgrnvegQ2, var.equal = TRUE)


##percieved stree
mean(proj1$Perceivedstress)
sd(proj1$Perceivedstress)
tapply(proj1$Perceivedstress, proj1$darkgrnvegQ2, FUN = mean)
tapply(proj1$Perceivedstress, proj1$darkgrnvegQ2, FUN = sd)
t.test(proj1$Perceivedstress ~ proj1$darkgrnvegQ2, var.equal = TRUE)


##high income
high_income <- table(proj1$income2cat == 1)
high_income
prop.table(high_income)
highincome_cons <- table(proj1$income2cat == 1, proj1$darkgrnvegQ2)
highincome_cons
prop.table(highincome_cons, 1)
chisq.test(proj1$income2cat, proj1$darkgrnvegQ2, correct = FALSE)

##household income
mean(proj1$nbmedHHincome)
sd(proj1$nbmedHHincome)
tapply(proj1$nbmedHHincome, proj1$darkgrnvegQ2, FUN= mean)
tapply(proj1$nbmedHHincome, proj1$darkgrnvegQ2, FUN= sd)
t.test(proj1$nbmedHHincome ~ proj1$darkgrnvegQ2, var.equal = TRUE)


##%below poverty
mean(proj1$nbpctPoverty)
sd(proj1$nbpctPoverty)
tapply(proj1$nbpctPoverty, proj1$darkgrnvegQ2, FUN= mean)
tapply(proj1$nbpctPoverty, proj1$darkgrnvegQ2, FUN= sd)
t.test(proj1$nbpctPoverty ~ proj1$darkgrnvegQ2, var.equal = TRUE)
   

##SES score
mean(proj1$nbSESanascore)
sd(proj1$nbSESanascore)
tapply(proj1$nbSESanascore, proj1$darkgrnvegQ2, FUN= mean)
tapply(proj1$nbSESanascore, proj1$darkgrnvegQ2, FUN= sd)
t.test(proj1$nbSESanascore ~ proj1$darkgrnvegQ2, var.equal = TRUE)
       
  

##  Favourable food
mean(proj1$nbK3FavorFoodstore)
sd(proj1$nbK3FavorFoodstore)
tapply(proj1$nbK3FavorFoodstore, proj1$darkgrnvegQ2, FUN= mean)
tapply(proj1$nbK3FavorFoodstore, proj1$darkgrnvegQ2, FUN= sd)
t.test(proj1$nbK3FavorFoodstore ~ proj1$darkgrnvegQ2, var.equal = TRUE)

##Physical activity
mean(proj1$nbK3paFacilities)
sd(proj1$nbK3paFacilities)
tapply(proj1$nbK3paFacilities, proj1$darkgrnvegQ2, FUN= mean)
tapply(proj1$nbK3paFacilities, proj1$darkgrnvegQ2, FUN= sd)
t.test(proj1$nbK3paFacilities ~ proj1$darkgrnvegQ2, var.equal = TRUE)



## Ques C
mean(proj1$nbK3FavorFoodstore)
sd(proj1$nbK3FavorFoodstore)
tapply(proj1$nbK3FavorFoodstore, proj1$darkgrnvegQ3, FUN= mean)
tapply(proj1$nbK3FavorFoodstore, proj1$darkgrnvegQ3, FUN= sd)
var.test(proj1$nbK3FavorFoodstore ~ proj1$darkgrnvegQ3, conf.level = 0.95) # variance is equal 
t.test(proj1$nbK3FavorFoodstore ~ proj1$darkgrnvegQ3, var.equal = TRUE)


## Ques D
mean(proj1$VitaminD2, na.rm = TRUE)
sd(proj1$VitaminD2, na.rm = TRUE)
mean(proj1$VitaminD3, na.rm = TRUE)
sd(proj1$VitaminD3, na.rm = TRUE)
t.test(proj1$VitaminD2, proj1$VitaminD3, paired = TRUE, conf.level = 0.95)


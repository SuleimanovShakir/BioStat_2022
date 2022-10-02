library(readxl)

#Задача №1

hypertension <- read_excel("Desktop/Bioinformatics Institute/hypertension.xlsx")

str(hypertension)

men <- sum(hypertension[hypertension$Sex == 1, "Sex"])
women <- 100 - men
sick_men <- sum(hypertension[hypertension$Sex == 1, "Hypertension"])
sick_women <- sum(hypertension[hypertension$Sex == 0, "Hypertension"])
healthy_men <- men - sick_men
healthy_women <- women - sick_women

table <- as.data.frame(matrix(c(sick_men, healthy_men, sick_women, healthy_women), ncol = 2, nrow = 2, byrow = T))
colnames(table) <- c("outcome", "no outcome")
rownames(table) <- c("men", "women")
                       
RR <- (table[1,1]/(table[1,1] + table[1,2]))/(table[2,1]/(table[2,1] + table[2,2]))
RD <- (table[1,1]/(table[1,1] + table[1,2])) - (table[2,1]/(table[2,1] + table[2,2]))

#Решение задачи №1
cat("Risk difference:", RD, ".", "Risk ratio:", RR)

#Можно сказать, что в данной выборке риск развития артериальной гипертензии выше у мужчин
#в 2.24 раза чем у женщин

#ЗАДАЧА №2

poisoning <- read_excel("Desktop/Bioinformatics Institute/otravlenie.xlsx")

str(poisoning)

meat <- sum(poisoning[poisoning$мясо == 1, "мясо"])
fish <- sum(poisoning[poisoning$рыба == 1, "рыба"])
salad <- sum(poisoning[poisoning$салат == 1, "салат"])

#Meat measurements
meat_outcome <- sum(poisoning$мясо == 1 & poisoning$отравление == 1)
meat_no_outcome <- sum(poisoning$мясо == 1 & poisoning$отравление == 0)
no_meat_outcome <- sum(poisoning$мясо == 0 & poisoning$отравление == 1)
no_meat_no_outcome <- sum(poisoning$мясо == 0 & poisoning$отравление == 0)

#Таблица сопряженности для мяса
meat_table <- as.data.frame(matrix(c(meat_outcome, meat_no_outcome, no_meat_outcome, no_meat_no_outcome), ncol = 2, nrow = 2, byrow = T))
colnames(meat_table) <- c("outcome", "no outcome")
rownames(meat_table) <- c("meat", "no meat")

#Odds ratio для мяса
OR_meat <- (meat_table[1,1]/meat_table[1,2]) / (meat_table[2,1]/meat_table[2,2])
  
#Fish measurements
fish_outcome <- sum(poisoning$рыба == 1 & poisoning$отравление == 1)
fish_no_outcome <- sum(poisoning$рыба == 1 & poisoning$отравление == 0)
no_fish_outcome <- sum(poisoning$рыба == 0 & poisoning$отравление == 1)
no_fish_no_outcome <- sum(poisoning$рыба == 0 & poisoning$отравление == 0)

#Таблица сопряженности для рыбы
fish_table <- as.data.frame(matrix(c(fish_outcome, fish_no_outcome, no_fish_outcome, no_fish_no_outcome), ncol = 2, nrow = 2, byrow = T))
colnames(fish_table) <- c("outcome", "no outcome")
rownames(fish_table) <- c("fish", "no fish")

#Odds ratio для рыбы
OR_fish <- (fish_table[1,1]/fish_table[1,2]) / (fish_table[2,1]/fish_table[2,2])

#Salad measurements
salad_outcome <- sum(poisoning$салат == 1 & poisoning$отравление == 1)
salad_no_outcome <- sum(poisoning$салат == 1 & poisoning$отравление == 0)
no_salad_outcome <- sum(poisoning$салат == 0 & poisoning$отравление == 1)
no_salad_no_outcome <- sum(poisoning$салат == 0 & poisoning$отравление == 0)

#Таблица сопряженности для салата
salad_table <- as.data.frame(matrix(c(salad_outcome, salad_no_outcome, no_salad_outcome, no_salad_no_outcome), ncol = 2, nrow = 2, byrow = T))
colnames(salad_table) <- c("outcome", "no outcome")
rownames(salad_table) <- c("salad", "no salad")

#Odds ratio для салата
OR_salad <- (salad_table[1,1]/salad_table[1,2]) / (salad_table[2,1]/salad_table[2,2])

Results_table_2ex <- as.data.frame(matrix(c(OR_meat, OR_fish, OR_salad), ncol = 3, nrow = 1, byrow = T))
colnames(Results_table_2ex) <- c("Meat", "Fish", "Salad")
rownames(Results_table_2ex) <- "OddsRatio"

#Решение задачи №2
Results_table_2ex

#ЗАДАЧА №3

carrental <- read_excel("Desktop/Bioinformatics Institute/carrental.xlsx")

str(carrental)
carrental <- as.data.frame(carrental)

carrental$sum <- carrental$stop - carrental$start

accidents_of_good_drivers <- sum(carrental[carrental$experience == 1, "accident"])
good_drivers <- sum(carrental$experience == 1)

accidents_of_bad_drivers <- sum(carrental[carrental$experience == 0, "accident"])
bad_drivers <- sum(carrental$experience == 0)

time_for_good_drivers <- sum(carrental[carrental$experience == 1, "sum"])

time_for_bad_drivers <- sum(carrental[carrental$experience == 0, "sum"])

accident_incidence_good_drivers <- accidents_of_good_drivers / good_drivers

accident_incidence_bad_drivers <- accidents_of_bad_drivers / bad_drivers

#количество часов на человека
person_hour_good_drivers <- time_for_good_drivers / good_drivers
person_hour_bad_drivers <- time_for_bad_drivers / bad_drivers

#driver-time
driver_time_good_drivers <- accidents_of_good_drivers / time_for_good_drivers

driver_time_bad_drivers <- accidents_of_bad_drivers / time_for_bad_drivers


Results_table_3ex <- as.data.frame(matrix(c(round(driver_time_good_drivers, 6), round(driver_time_bad_drivers, 6)), ncol = 2, nrow = 1, byrow = T))
colnames(Results_table_3ex) <- c("Good drivers", "Bad drivers")
rownames(Results_table_3ex) <- "Driver-time"

#Решение задачи №3
Results_table_3ex


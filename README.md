# myrepo
testing my setup

stroke_tx <- HDSS_2 %>% 
  select(no_responden, ptm05a) %>% 
  filter(ptm05a == "ya") %>% 
  left_join(HDSS_1, by = "no_responden") 

x1 <- x %>% 
  ungroup() %>% 
  select(no_responden, asal, occup)

stroke_tx <- stroke_tx %>% 
  left_join(HDSS_nutrisi,  by = "no_responden") %>% 
  left_join(x1, by = "no_responden")


########################

stroke_tx  <- stroke_tx  %>% 
  rename(
    age = ageyr_w1.x,
    sex = art04_w1.x,
    residence = asal ,
    location = kl01.x ,
    marital = statkawin_w1.x ,
    education = art16_w1.x, 
    ethnic =  art14_w1.x, 
    occupation = occup,
    insurance =  art19_w1.x ,
    hypertension = ptm01 ,
    diabetes =   ptm03 ,
    cvd = ptm07 ,
    cancer = ptm11,
  )



stroke_tx <- na.omit(stroke_tx)

stroke_tx %>% 
  group_by(no_ruta.x) %>% 
  count()


# Factor the basic variables that
# we're interested in
stroke_tx$residence <- 
  factor(stroke_tx$residence)

table1(~ factor(sex) + age + factor(marital) + factor(education) +
         factor(insurance) +
         factor(hypertension)+
         factor(diabetes) +
         factor(cvd) +
         factor(cancer)
       | residence, data=stroke_tx, render.continuous=my.render.cont, render.categorical=my.render.cat)


## cek normality test data numerik

library("ggpubr")
ggdensity(stroke_tx$age, 
          main = "Density plot of age",
          xlab = "age")

library(ggpubr)
ggqqplot(stroke$age)

library("car")
qqPlot(stroke$age)

shapiro.test(stroke_tx$age)

## custom

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

strata_tx <- c(list(Total=stroke_tx), split(stroke_tx, stroke_tx$residence))


labels_tx <- list(
  variables=list(sex="Sex",
                 age="Age (years)",
                 education="Education",
                 occupation = "Occupation",
                 hypertension="Hypertension",
                 diabetes = "Diabetes",
                 cvd = "Cardiovascular diseases",
                 cancer = "Cancer"),
  groups=list("", "Residence"))

# Remove the word "death" from the labels, since it now appears above
levels(stroke_tx$residence) <- c("Urban, Rural")

View(stroke)
table1(strata_tx, labels_tx,groupspan=c(1, 2),extra.col=list(`P-value`=pvalue),
       render.continuous=my.render.cont, render.categorical=my.render.cat)


table1(~ sex + age + education + occupation + hypertension + diabetes + cvd + cancer | residence,
       data=stroke_tx, overall=F, extra.col=list(`P-value`=pvalue),  render.continuous=my.render.cont, render.categorical=my.render.cat)

library(dplyr)




stroke_tx$education <- dplyr::recode(stroke_tx$education, "sd/mi" = "Elementary",
                                  "sltp/mts" = "Junior high school",
                                  "slta/smk/ma" = "High school",
                                  "d2/d3" = "College or above",
                                  "d4/s1" = "College or above",
                                  "s2/s3" = "College or above",
                                  "tidak tahu" = "Unknown",
                                  "tidak/belum pernah sekolah" = "No formal education")

stroke_tx$residence <- dplyr::recode(stroke_tx$residence, "urban" = "Urban",
                                  "rural" = "Rural")
stroke_tx$hypertension <- dplyr::recode(stroke_tx$hypertension, "ya" = "Yes",
                                     "tidak" = "No")
stroke_tx$diabetes <- dplyr::recode(stroke_tx$diabetes, "ya" = "Yes",
                                 "tidak" = "No")
stroke_tx$cvd <- dplyr::recode(stroke_tx$cvd, "ya" = "Yes",
                            "tidak" = "No")

stroke_tx$cancer <- dplyr::recode(stroke_tx$cancer, "ya" = "Yes",
                               "tidak" = "No")
stroke_tx$sex <- dplyr::recode(stroke_tx$sex, "laki-laki" = "Male",
                            "perempuan" = "Female")


detach("package:car", unload=TRUE)

# stats

chisq.test(stroke$diabetes, stroke$residence, correct=FALSE)
chisq.test(stroke$cvd, stroke$residence, correct=FALSE)
chisq.test(stroke$hypertension, stroke$residence, correct=FALSE)
chisq.test(stroke$sex, stroke$residence, correct=FALSE)
t.test(stroke$age, stroke$residence)

stroke_tx %>% 
  select(residence, age) %>% 
  group_by(residence) %>% 
  summarise(
    count = n(),
    mean = mean(age, na.rm = TRUE),
    sd = sd(age, na.rm = TRUE)
  )



library("ggpubr")
ggboxplot(stroke_tx, x = "residence", y = "age", 
          color = "residence", palette = c("#00AFBB", "#E7B800"),
          ylab = "age", xlab = "age")






table1::label(stroke_tx$sex)     <- "Sex"
table1::label(stroke_tx$age)     <- "Age"
table1::label(stroke_tx$education)     <- "Education"
table1::label(stroke_tx$hypertension)     <- "Hypertension"
table1::label(stroke_tx$diabetes)     <- "Diabetes"
table1::label(stroke_tx$cvd)     <- "Cardiovascular disease"
table1::label(stroke_tx$cancer)     <- "Cancer"

stroke_tx$education <- dplyr::recode(stroke_tx$education, "sd/mi" = "Elementary",
                                  "sltp/mts" = "Junior high school",
                                  "slta/smk/ma" = "High school",
                                  "d2/d3" = "College or above",
                                  "d4/s1" = "College or above",
                                  "s2/s3" = "College or above",
                                  "tidak tahu" = "Unknown",
                                  "tidak/belum pernah sekolah" = "No formal education")





## berapa RUTA dari 505 subyek pasien yag stroke?

xruta_a <- x %>% 
  filter(ptm05a == "ya") 

stroke_tx_a <- xruta_a %>% 
  right_join(stroke_tx, by = "no_responden") %>% 
  group_by(no_ruta)


## menampilkan Ruta saja untuk pasien stroke

stroke_tx_a %>% 
  group_by(no_ruta) %>% 
  slice(1) %>% 
  ungroup() -> stroke_tx_ruta

#download dataset HDSS nutrisi llalu joint dengan data stroke untuk menambahkan kolom nutrisi
stroke <- HDSS_nutrisi %>% 
  right_join(stroke)

# menghitung kalori 
names(stroke)[...3] <- "asal"
stroke$...3 -> stroke$asal


stroke_tx_E   <- stroke_tx %>% 
  select(starts_with("ENER")) %>%
  mutate_if(is.character,as.numeric)%>%
  rowwise() %>% 
  mutate(Energi_total = sum(c_across(1:11))) %>%
  ungroup() %>%
  mutate(E_beras = (`ENERGI BERAS (KAL)`/Energi_total)*100) %>%
  mutate(E_umbi = (`ENERGI UMBI (KAL)`/Energi_total)*100) %>%
  mutate(E_jagung = (`ENERGI JAGUNG (KAL)`/Energi_total)*100) %>%
  mutate(E_mie = (`ENERGI MIE INSTAN (KAL)`/Energi_total)*100) %>%
  mutate(E_roti = (`ENERGI ROTI TAWAR (KAL)`/Energi_total)*100) %>%
  mutate(E_ayam = (`ENERGI AYAM (KAL)`/Energi_total)*100) %>%
  mutate(E_daging = (`ENERGI DAGING (KAL)`/Energi_total)*100) %>%
  mutate(E_telur = (`ENERGI TELUR (KAL)`/Energi_total)*100) %>%
  mutate(E_tahu = (`ENERGI TAHU (KAL)`/Energi_total)*100)%>%
  mutate(E_tempe = (`ENERGI TEMPE (KAL)`/Energi_total)*100)%>%
  mutate(E_gula = (`ENERGI GULA(KAL)`/Energi_total)*100) %>% 
  cbind(stroke_tx$residence)

stroke_tx_P <- stroke_tx %>% select(starts_with("PROT"))%>%
  mutate_if(is.character,as.numeric)%>%
  rowwise() %>% 
  mutate(Prot_total = sum(c_across(1:11))) %>%
  ungroup()%>%
  mutate(P_beras = (`PROTEIN BERAS (GRAM)`/Prot_total)*100) %>%
  mutate(P_umbi = (`PROTEIN UMBI (GRAM)`/Prot_total)*100) %>%
  mutate(P_jagung = (`PROTEIN JAGUNG (GRAM)`/Prot_total)*100) %>%
  mutate(P_mie = (`PROTEIN MIE INSTAN (GRAM)`/Prot_total)*100) %>%
  mutate(P_roti = (`PROTEIN ROTI TAWAR (GRAM)`/Prot_total)*100) %>%
  mutate(P_ayam = (`PROTEIN AYAM (GRAM)`/Prot_total)*100) %>%
  mutate(P_daging = (`PROTEIN DAGING (GRAM)`/Prot_total)*100) %>%
  mutate(P_telur = (`PROTEIN TELUR (GRAM)`/Prot_total)*100) %>%
  mutate(P_tahu = (`PROTEIN TAHU (GRAM)`/Prot_total)*100)%>%
  mutate(P_tempe = (`PROTEIN TEMPE (GRAM)`/Prot_total)*100)%>%
  mutate(P_gula = (`PROTEIN GULA (GRAM)`/Prot_total)*100) %>% 
  cbind(stroke_tx$residence)

stroke_tx_K <- stroke_tx %>% 
  select(starts_with("KH")) %>%
  mutate_if(is.character,as.numeric)%>%
  rowwise() %>% 
  mutate(KH_total = sum(c_across(1:11))) %>%
  ungroup() %>%
  mutate(K_beras = (`KH BERAS (GRAM)`/KH_total)*100) %>%
  mutate(K_umbi = (`KH UMBI (GRAM)`/KH_total)*100) %>%
  mutate(K_jagung = (`KH JAGUNG (GRAM)`/KH_total)*100) %>%
  mutate(K_mie = (`KH MIE INSTAN (GRAM)`/KH_total)*100) %>%
  mutate(K_roti = (`KH ROTI TAWAR (GRAM)`/KH_total)*100) %>%
  mutate(K_ayam = (`KH AYAM (GRAM)`/KH_total)*100) %>%
  mutate(K_daging = (`KH DAGING (GRAM)`/KH_total)*100) %>%
  mutate(K_telur = (`KH TELUR (GRAM)`/KH_total)*100) %>%
  mutate(K_tahu = (`KH TAHU (GRAM)`/KH_total)*100)%>%
  mutate(K_tempe = (`KH TEMPE (GRAM)`/KH_total)*100)%>%
  mutate(K_gula = (`KH GULA (GRAM)`/KH_total)*100) %>% 
  cbind(stroke_tx$residence)

stroke_tx_L
stroke_tx_L <- stroke_tx %>% select(starts_with("LEMAK"))%>%
  mutate_if(is.character,as.numeric)%>%
  rowwise() %>% 
  mutate(L_total = sum(c_across(1:11))) %>%
  ungroup() %>%
  mutate(L_beras = (`LEMAK BERAS (GRAM)`/L_total)*100) %>%
  mutate(L_umbi = (`LEMAK UMBI (GRAM)`/L_total)*100) %>%
  mutate(L_jagung = (`LEMAK JAGUNG (GRAM)`/L_total)*100) %>%
  mutate(L_mie = (`LEMAK MIE INSTAN (GRAM)`/L_total)*100) %>%
  mutate(L_roti = (`LEMAK ROTI TAWAR (GRAM)`/L_total)*100) %>%
  mutate(L_ayam = (`LEMAK AYAM (GRAM)`/L_total)*100) %>%
  mutate(L_daging = (`LEMAK DAGING (GRAM)`/L_total)*100) %>%
  mutate(L_telur = (`LEMAK TELUR (GRAM)`/L_total)*100) %>%
  mutate(L_tahu = (`LEMAK TAHU (GRAM)`/L_total)*100)%>%
  mutate(L_tempe = (`LEMAK TEMPE (GRAM)`/L_total)*100)%>%
  mutate(L_gula = (`LEMAK GULA (GRAM)`/L_total)*100) %>% 
  cbind(stroke_tx$residence)


stroke_tx_S
stroke_tx_S <- stroke_tx %>% select(starts_with("SERAT"))%>%
  mutate_if(is.character,as.numeric)%>%
  rowwise() %>% 
  mutate(S_total = sum(c_across(1:11))) %>%
  ungroup() %>%
  mutate(Serat_beras = (`SERAT BERAS (GRAM)`/S_total)*100) %>%
  mutate(Serat_umbi = (`SERAT UMBI (GRAM)`/S_total)*100) %>%
  mutate(Serat_jagung = (`SERAT JAGUNG (GRAM)`/S_total)*100) %>%
  mutate(Serat_mie = (`SERAT MIE INSTAN (GRAM)`/S_total)*100) %>%
  mutate(Serat_roti = (`SERAT ROTI TAWAR (GRAM)`/S_total)*100) %>%
  mutate(Serat_ayam = (`SERAT AYAM (GRAM)`/S_total)*100) %>%
  mutate(Serat_daging = (`SERAT DAGING (GRAM)`/S_total)*100) %>%
  mutate(Serat_telur = (`SERAT TELUR (GRAM)`/S_total)*100) %>%
  mutate(Serat_tahu = (`SERAT TAHU (GRAM)`/S_total)*100)%>%
  mutate(Serat_tempe = (`SERAT TEMPE (GRAM)`/S_total)*100)%>%
  mutate(Serat_gula = (`SERAT GULA (GRAM)`/S_total)*100) %>% 
  cbind(stroke_tx$residence) 


#persiapan lolipop
names(stroke_tx_S)[24] <- "asal"
names(stroke_tx_L)[24] <- "asal"
names(stroke_tx_E)[24] <- "asal"
names(stroke_tx_K)[24] <- "asal"
names(stroke_tx_P)[24] <- "asal"

stroke_tx_S <- stroke_tx_S  %>% 
  group_by(asal) %>% 
  summarise_if(is.numeric, mean)   %>% 
  select(14:24) %>%
  rbind(0) %>% rbind(100)

stroke_tx_S <- stroke_tx_S[c("4","3","1","2"),]

colnames(stroke_S)

stroke_tx_L
stroke_tx_L <- stroke_tx_L  %>% 
  group_by(asal) %>% 
  summarise_if(is.numeric, mean)   %>% 
  select(14:24) %>%
  rbind(0) %>% rbind(100)

stroke_tx_L <- stroke_tx_L[c("4","3","1","2"),]

stroke_tx_E 
stroke_tx_E <- stroke_tx_E  %>% 
  group_by(asal) %>% 
  summarise_if(is.numeric, mean)   %>% 
  select(14:24) %>%
  rbind(0) %>% rbind(100)

stroke_tx_E <- stroke_tx_E[c("4","3","1","2"),]

stroke_tx_P <- stroke_tx_P  %>% 
  group_by(asal) %>% 
  summarise_if(is.numeric, mean)   %>% 
  select(14:24) %>%
  rbind(0) %>% rbind(100)

stroke_tx_P <- stroke_tx_P[c("4","3","1","2"),]

stroke_tx_K <- stroke_tx_K  %>% 
  group_by(asal) %>% 
  summarise_if(is.numeric, mean)   %>% 
  select(14:24) %>%
  rbind(0) %>% rbind(100)

stroke_tx_K <- stroke_tx_K[c("4","3","1","2"),]



#lolipop


Stroke_tx_S_lol
Stroke_tx_S_lol <- t(stroke_tx_S) %>%
  as.data.frame() %>%
  rownames_to_column("Field")

colnames(Stroke_tx_S_lol) = c("Type", "Max", "Min", "Rural", "Urban")

Stroke_tx_S_lol <- Stroke_tx_S_lol %>% 
  select(Type, Rural, Urban) %>%  
  mutate(Type = fct_other(Type, keep = c("Serat_beras","Serat_umbi", "Serat_roti",
                                         "Serat_tahu", "Serat_tempe"))) %>% 
  group_by(Type = tolower(Type)) %>%
  summarise_each(funs(sum)) %>% 
  pivot_longer(
    cols = c(Rural, Urban),
    names_to = "Residence",
    values_to = "value"
  )

Stroke_tx_S_lol$Type <- recode(Stroke_tx_S_lol$Type, serat_beras = "Rice")
Stroke_tx_S_lol$Type <- recode(Stroke_tx_S_lol$Type, serat_umbi = "Tubers")
Stroke_tx_S_lol$Type <- recode(Stroke_tx_S_lol$Type, serat_tahu = "Tofu")
Stroke_tx_S_lol$Type <- recode(Stroke_tx_S_lol$Type, serat_roti = "Bread")
Stroke_tx_S_lol$Type <- recode(Stroke_tx_S_lol$Type, other = "Others")
Stroke_tx_S_lol$Type <- recode(Stroke_tx_S_lol$Type, serat_tempe = "Tempeh")
Stroke_tx_S_lol

s_lol<- ggdotchart(
  Stroke_tx_S_lol, x = "Type", y = "value", 
  group = "Residence", shape = "Residence", dot.size = 2,palette = "lancet",
  add = "segment", position = position_dodge(0.3),
  sorting = "ascending"
)
s <- s_lol +coord_flip()+ labs(title = "Fiber",
                               y = "percentage (%)") +
  theme_light()  # using a custom th
s
library(ggpubr)

# protein

Stroke_tx_P_lol <- t(stroke_tx_P) %>%
  as.data.frame() %>%
  rownames_to_column("Field")

colnames(Stroke_tx_P_lol) = c("Type", "Max", "Min", "Rural", "Urban")

Stroke_tx_P_lol <- Stroke_tx_P_lol %>% 
  select(Type, Rural, Urban) %>% 
  mutate(Type = fct_other(Type, keep = c("P_beras","P_tahu", "P_tempe",
                                         "P_ayam", "P_telur"))) %>% 
  group_by(Type = tolower(Type)) %>%
  summarise_each(funs(sum)) %>% 
  pivot_longer(
    cols = c(Rural, Urban),
    names_to = "Residence",
    values_to = "value"
  )

Stroke_tx_P_lol$Type <- recode(Stroke_tx_P_lol$Type, p_beras = "Rice")
Stroke_tx_P_lol$Type <- recode(Stroke_tx_P_lol$Type, p_ayam = "Poultry")
Stroke_tx_P_lol$Type <- recode(Stroke_tx_P_lol$Type, p_tahu = "Tofu")
Stroke_tx_P_lol$Type <- recode(Stroke_tx_P_lol$Type, p_telur = "Egg")
Stroke_tx_P_lol$Type <- recode(Stroke_tx_P_lol$Type, other = "Others")
Stroke_tx_P_lol$Type <- recode(Stroke_tx_P_lol$Type, p_tempe = "Tempeh")


p_lol<- ggdotchart(
  Stroke_tx_P_lol, x = "Type", y = "value", 
  group = "Residence", shape = "Residence", dot.size = 2,palette = "lancet",
  add = "segment", position = position_dodge(0.3),
  sorting = "ascending"
)
p <- p_lol +coord_flip()+ labs(title = "Protein",
                                       y = "percentage (%)") +
  theme_light()  # using a custom th
p


#lemak

Stroke_tx_L_lol <- t(stroke_tx_L) %>%
  as.data.frame() %>%
  rownames_to_column("Field")

colnames(Stroke_tx_L_lol) = c("Type", "Max", "Min", "Rural", "Urban")

Stroke_tx_L_lol <- Stroke_tx_L_lol %>% 
  select(Type, Rural, Urban) %>%  
  mutate(Type = fct_other(Type, keep = c("L_ayam","L_tahu", "L_tempe",
                                         "L_beras", "L_telur"))) %>% 
  group_by(Type = tolower(Type)) %>%
  summarise_each(funs(sum)) %>% 
  pivot_longer(
    cols = c(Rural, Urban),
    names_to = "Residence",
    values_to = "value"
  )

Stroke_tx_L_lol$Type <- recode(Stroke_tx_L_lol$Type, l_beras = "Rice")
Stroke_tx_L_lol$Type <- recode(Stroke_tx_L_lol$Type, l_ayam = "Poultry")
Stroke_tx_L_lol$Type <- recode(Stroke_tx_L_lol$Type, l_tahu = "Tofu")
Stroke_tx_L_lol$Type <- recode(Stroke_tx_L_lol$Type, l_telur = "Egg")
Stroke_tx_L_lol$Type <- recode(Stroke_tx_L_lol$Type, other = "Others")
Stroke_tx_L_lol$Type <- recode(Stroke_tx_L_lol$Type, l_tempe = "Tempeh")




l_lol<- ggdotchart(
  Stroke_tx_L_lol, x = "Type", y = "value", 
  group = "Residence", shape = "Residence", dot.size = 2,palette = "lancet",
  add = "segment", position = position_dodge(0.3),
  sorting = "ascending"
)
l <- l_lol +coord_flip()+ labs(title = "Fat",
                               y = "percentage (%)") +
  theme_light()  # using a custom th
l

# karbohidrat
Stroke_tx_K_lol
Stroke_tx_K_lol <- t(stroke_tx_K) %>%
  as.data.frame() %>%
  rownames_to_column("Field")

colnames(Stroke_tx_K_lol) = c("Type", "Max", "Min", "Rural", "Urban")

Stroke_tx_K_lol <- Stroke_tx_K_lol %>% 
  select(Type, Rural, Urban) %>%  
  mutate(Type = fct_other(Type, keep = c("K_beras","K_gula", "K_umbi",
                                         "K_jagung", "K_tahu"))) %>% 
  group_by(Type = tolower(Type)) %>%
  summarise_each(funs(sum)) %>% 
  pivot_longer(
    cols = c(Rural, Urban),
    names_to = "Residence",
    values_to = "value"
  )

Stroke_tx_K_lol$Type <- recode(Stroke_tx_K_lol$Type, k_beras = "Rice")
Stroke_tx_K_lol$Type <- recode(Stroke_tx_K_lol$Type, k_umbi = "Tuber")
Stroke_tx_K_lol$Type <- recode(Stroke_tx_K_lol$Type, k_gula = "Sugar")
Stroke_tx_K_lol$Type <- recode(Stroke_tx_K_lol$Type, k_jagung = "Corn")
Stroke_tx_K_lol$Type <- recode(Stroke_tx_K_lol$Type, other = "Others")
Stroke_tx_K_lol$Type <- recode(Stroke_tx_K_lol$Type, k_tahu = "Tofu")


k_lol<- ggdotchart(
  Stroke_tx_K_lol, x = "Type", y = "value", 
  group = "Residence", shape = "Residence", dot.size = 2,palette = "lancet",
  add = "segment", position = position_dodge(0.3),
  sorting = "ascending"
)
k <- k_lol +coord_flip()+ labs(title = "Carbohydrate",
                               y = "percentage (%)") +
  theme_light()  # using a custom th
k

#Energy
Stroke_tx_E_lol

Stroke_tx_E_lol <- t(stroke_tx_E) %>%
  as.data.frame() %>%
  rownames_to_column("Field")

colnames(Stroke_tx_E_lol) = c("Type", "Max", "Min", "Rural", "Urban")

Stroke_tx_E_lol <- Stroke_tx_E_lol %>% 
  select(Type, Rural, Urban) %>%  
  mutate(Type = fct_other(Type, keep = c("E_beras","E_gula", "E_ayam",
                                         "E_tahu", "E_tempe"))) %>% 
  group_by(Type = tolower(Type)) %>%
  summarise_each(funs(sum)) %>% 
  pivot_longer(
    cols = c(Rural, Urban),
    names_to = "Residence",
    values_to = "value"
  )

Stroke_tx_E_lol$Type <- recode(Stroke_tx_E_lol$Type, e_beras = "Rice")
Stroke_tx_E_lol$Type <- recode(Stroke_tx_E_lol$Type, e_ayam = "Poultry")
Stroke_tx_E_lol$Type <- recode(Stroke_tx_E_lol$Type, e_gula = "Sugar")
Stroke_tx_E_lol$Type <- recode(Stroke_tx_E_lol$Type, k_tempe = "Tempeh")
Stroke_tx_E_lol$Type <- recode(Stroke_tx_E_lol$Type, other = "Others")
Stroke_tx_E_lol$Type <- recode(Stroke_tx_E_lol$Type, k_tahu = "Tofu")


e_lol<- ggdotchart(
  Stroke_tx_E_lol, x = "Type", y = "value", 
  group = "Residence", shape = "Residence", dot.size = 2,palette = "lancet",
  add = "segment", position = position_dodge(0.3),
  sorting = "ascending"
)
e <- e_lol +coord_flip()+ labs(title = "Energy",
                               y = "percentage (%)") +
  theme_light()  # using a custom th
e

ggarrange(e,p,l,k,s,ncol = 2, nrow = 3, labels = "AUTO")


#####
stroke %>% 
  select(asal, ptm05a) %>% 
  filter(ptm05a == "ya") %>% 
  group_by(asal, ptm05a ) %>% 
  summarise(n = n())


stroke_tx_x  <- stroke_tx%>% mutate(
  occup_lump = fct_lump(occupation, n = 7)) 
  
stroke_tx_x <- stroke_tx_x %>% drop_na(occup_lump)
stroke_tx_x
  
stroke_tx_x %>% ggplot(aes(x=occup_lump)) + 
  geom_bar(color = 'black', fill = 'firebrick') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Occupation") +
  ylab("Count")






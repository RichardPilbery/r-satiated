# Analysis practice
library(tidyverse)
library(readr)
library(networkD3)
satiated_data <- read_csv("SATIATED-Results - Sheet1.csv")

# ---- Summarise the data ----

demographics <- satiated_data %>%
  group_by(randomseq) %>%
  summarise(
    total = n(),
    intubation_attempts_past_12_months = median(intattempts),
    IQR_intubation_attempts_past_12_months = paste(fivenum(intattempts)[2],fivenum(intattempts)[4],sep="-"),
    max_intubation_attempts_past_12_months = max(intattempts),
    successful_intubation_attempts_past_12_months = median(succintattempt),
    IQR_successful_intubation_attempts_past_12_months = paste(fivenum(succintattempt)[2],fivenum(succintattempt)[4],sep="-"),
    max_successful_intubation_attempts_past_12_months = max(succintattempt),
    years_as_paramedic = median(yearsqualify),
    IQR_years_as_paramedic = paste(fivenum(yearsqualify)[2],fivenum(yearsqualify)[4],sep="-"),
    familiar_SALAD_technique = sum(familiarSALAD == "yes"),
    used_SALAD_in_last_3_months = sum(usedSALAD == "yes")
  )


library(psych)

satiated_data2 <- satiated_data %>%
  mutate(
    # Truncate any overall attempt time greater than 90 seconds to 90 seconds
    attempt1intattemptTotalV = ifelse(attempt1intattemptTotalV > 90, 90, attempt1intattemptTotalV),
    attempt2intattemptTotalV = ifelse(attempt2intattemptTotalV > 90, 90, attempt2intattemptTotalV),
    attempt3intattemptTotalV = ifelse(attempt3intattemptTotalV > 90, 90, attempt3intattemptTotalV),
    # Truncate any intubation time greater than 90 seconds to 90 seconds/
    attempt1intattemptCompletedV = ifelse(attempt1intattemptCompletedV > 90, 90, attempt1intattemptCompletedV),
    attempt2intattemptCompletedV = ifelse(attempt2intattemptCompletedV > 90, 90, attempt2intattemptCompletedV),
    attempt3intattemptCompletedV = ifelse(attempt3intattemptCompletedV > 90, 90, attempt3intattemptCompletedV),
    proportion_successful_intubations = succintattempt/intattempts,
    time_diff_attempt_1_and_2 = attempt1intattemptCompletedV-attempt2intattemptCompletedV,
    time_diff_attempt_1_and_3 = attempt1intattemptCompletedV-attempt3intattemptCompletedV,
    tot_time_diff_attempt_1_and_2 = attempt1intattemptTotalV-attempt2intattemptTotalV,
    tot_time_diff_attempt_1_and_3 = attempt1intattemptTotalV-attempt3intattemptTotalV
  )

saveRDS(satiated_data2, "data.rds")

demographicsTotal <- satiated_data2 %>%
  summarise(
    total = n(),
    intubation_attempts_past_12_months = median(intattempts),
    IQR_intubation_attempts_past_12_months = paste(fivenum(intattempts)[2],fivenum(intattempts)[4],sep="-"),
    max_intubation_attempts_past_12_months = max(intattempts),
    successful_intubation_attempts_past_12_months = median(succintattempt),
    IQR_successful_intubation_attempts_past_12_months = paste(fivenum(succintattempt)[2],fivenum(succintattempt)[4],sep="-"),
    max_successful_intubation_attempts_past_12_months = max(succintattempt),
    years_as_paramedic = median(yearsqualify),
    IQR_years_as_paramedic = paste(fivenum(yearsqualify)[2],fivenum(yearsqualify)[4],sep="-"),
    familiar_SALAD_technique = sum(familiarSALAD == "yes"),
    used_SALAD_in_last_3_months = sum(usedSALAD == "yes")
  )

pairs.panels(satiated_data2[c("intattempts","proportion_successful_intubations","yearsqualify")])

# ---- Analyse the results ------

results <- satiated_data2 %>%
  group_by(randomseq) %>%
  summarise(
    n = n(),
    number_successful_1st_attempt = sum(attempt1SuccessV=="yes"),
    number_successful_2nd_attempt = sum(attempt2SuccessV=="yes"),
    number_successful_3rd_attempt = sum(attempt3SuccessV=="yes"),
    mean_diff_attempt_1_and_2 = mean(time_diff_attempt_1_and_2[attempt1SuccessV=="yes"&attempt2SuccessV=="yes"]),
    mean_diff_attempt_1_and_3 = mean(time_diff_attempt_1_and_3[attempt1SuccessV=="yes"&attempt3SuccessV=="yes"]),
    tot_mean_diff_attempt_1_and_2 = mean(tot_time_diff_attempt_1_and_2[attempt1SuccessV=="yes"&attempt2SuccessV=="yes"]),
    tot_mean_diff_attempt_1_and_3 = mean(tot_time_diff_attempt_1_and_3[attempt1SuccessV=="yes"&attempt3SuccessV=="yes"]),
    mean_attempt_time_1 = mean(attempt1intattemptCompletedV),
    mean_attempt_time_2 = mean(attempt2intattemptCompletedV),
    mean_attempt_time_3 = mean(attempt3intattemptCompletedV),
    mean_succ_attempt_time_1 = mean(attempt1intattemptCompletedV[attempt1SuccessV=="yes"]),
    mean_succ_attempt_time_2 = mean(attempt2intattemptCompletedV[attempt2SuccessV=="yes"]),
    mean_succ_attempt_time_3 = mean(attempt3intattemptCompletedV[attempt3SuccessV=="yes"])
  )

# ---- Primary Outcome ------

options(scipen = 999)
primary_outcome <- prop.test(results$number_successful_2nd_attempt, results$n ,correct = F)


# ---- Secondary Outcome -------

hist(satiated_data2$attempt3intattemptCompletedV, breaks=20, density=10)
hist(satiated_data2$attempt3intattemptTotalV[satiated_data2$attempt3SuccessV == "yes"], breaks=20, density=10) +
  abline()

ggplot(satiated_data2[satiated_data2$attempt1SuccessV=="yes",], aes(x = attempt1intattemptTotalV)) + 
  geom_histogram(aes(y = ..density..), bins = 10) + 
  geom_density()

ggplot(satiated_data2[satiated_data2$attempt2SuccessV=="yes",], aes(x = attempt2intattemptTotalV)) + 
  geom_histogram(aes(y = ..density..), bins = 10) + 
  geom_density()

ggplot(satiated_data2[satiated_data2$attempt3SuccessV=="yes",], aes(x = attempt3intattemptTotalV)) + 
  geom_histogram(aes(y = ..density..), bins = 10) + 
  geom_density()

AAB <- satiated_data2 %>%
  filter(randomseq == "AAB")

ABB <- satiated_data2 %>%
  filter(randomseq == "ABB")

### ---- Compare Means of differences -----

# Compare mean time differences of AAB and ABB attempts 1 and 2 i.e. (A01-A02) with (A11-B11)
t.test(AAB$time_diff_attempt_1_and_2[AAB$attempt1SuccessV=="yes"&AAB$attempt2SuccessV=="yes"], ABB$time_diff_attempt_1_and_2[ABB$attempt1SuccessV=="yes"&ABB$attempt2SuccessV=="yes"])

# Compare mean time difference of AAB and ABB attempts 1 and 3 (A01-B01) with (A11-B12)
t.test(AAB$time_diff_attempt_1_and_3[AAB$attempt1SuccessV=="yes"&AAB$attempt3SuccessV=="yes"], ABB$time_diff_attempt_1_and_3[ABB$attempt1SuccessV=="yes"&ABB$attempt3SuccessV=="yes"])

## TOTAL
# Compare mean time differences of AAB and ABB attempts 1 and 2 i.e. (A01-A02) with (A11-B11)
t.test(AAB$tot_time_diff_attempt_1_and_2[AAB$attempt1SuccessV=="yes"&AAB$attempt2SuccessV=="yes"], ABB$tot_time_diff_attempt_1_and_2[ABB$attempt1SuccessV=="yes"&ABB$attempt2SuccessV=="yes"])

# Compare mean time difference of AAB and ABB attempts 1 and 3 (A01-B01) with (A11-B12)
t.test(AAB$tot_time_diff_attempt_1_and_3[AAB$attempt1SuccessV=="yes"&AAB$attempt3SuccessV=="yes"], ABB$tot_time_diff_attempt_1_and_3[ABB$attempt1SuccessV=="yes"&ABB$attempt3SuccessV=="yes"])

AABprop <- results %>%
  filter(randomseq == "AAB")

ABBprop <- results %>%
  filter(randomseq == "ABB")

number_obsABB = ABBprop$n
number_obsAAB = AABprop$n

# Compare the success rates between B01 and B12 to see whether practice following training improves intubation success rate
secondary_outcome <- prop.test(c(AABprop$number_successful_3rd_attempt,ABBprop$number_successful_3rd_attempt), c(number_obsAAB,number_obsABB) ,correct = F)


prepgg1 <- satiated_data2 %>%
  select(id, randomseq, attempt1intattemptTotalV, attempt2intattemptTotalV, attempt3intattemptTotalV, attempt1intattemptCompletedV, attempt2intattemptCompletedV, attempt3intattemptCompletedV, attempt1intattemptStartV, attempt2intattemptStartV, attempt3intattemptStartV) %>%
  gather("attempt","time",-id, -randomseq) %>%
  mutate(
    metric = ifelse(grepl("total",tolower(attempt)), "3 Total attempt time", ifelse(grepl("completed",tolower(attempt)), "2 Intubation attempt time","1 Intubation start time")),
    attempt = as.factor(substr(attempt, 8, 8))
  )

ggplot(prepgg1, aes(attempt, time)) +
  geom_boxplot(aes(fill=randomseq)) +
  facet_wrap(~ metric)

prepgg2 <- satiated_data2 %>%
  select(id, randomseq, attempt1intattemptTotalV, attempt2intattemptTotalV, attempt3intattemptTotalV, attempt1intattemptCompletedV, attempt2intattemptCompletedV, attempt3intattemptCompletedV, attempt1intattemptStartV, attempt2intattemptStartV, attempt3intattemptStartV) %>%
  gather("attempt","time",-id, -randomseq) %>%
  mutate(
    metric = ifelse(grepl("total",tolower(attempt)), "3 Total attempt time", ifelse(grepl("completed",tolower(attempt)), "2 Intubation attempt time","1 Intubation start time")),
    attempt = as.factor(substr(attempt, 8, 8))
  ) %>%
  rowwise() %>%
  mutate(
    success = ifelse(attempt == "1", satiated_data2$attempt1SuccessV[satiated_data2$id == id] , ifelse(attempt == "2", satiated_data2$attempt2SuccessV[satiated_data2$id == id] , satiated_data2$attempt3SuccessV[satiated_data2$id == id] ))
  )

#http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
ggplot(prepgg2, aes(attempt, time)) +
  geom_boxplot(aes(fill=success)) +
  facet_wrap(~ metric + randomseq, nrow=3, ncol=2) +
  theme_bw() +
  scale_y_continuous("Seconds", breaks = seq(0, 90, 5)) 

ggplot(prepgg2[prepgg2$success=="yes",], aes(attempt, time, fill=randomseq)) +
  geom_violin(trim=T, width=1, position=position_dodge(1)) +
  geom_boxplot(width=0.3,
              position=position_dodge(1))+
  theme_classic() +
  scale_y_continuous("Seconds", breaks = seq(0, 90, 5)) +
  facet_wrap(~ metric)



number_obsABB = ABBprop$n
number_obsAAB = AABprop$n

secondary_outcome <- prop.test(c(AABprop$number_successful_3rd_attempt,ABBprop$number_successful_3rd_attempt), c(number_obsAAB,number_obsABB) ,correct = F)

library(ggridges)

satiated_data3 <- satiated_data2 %>%
  mutate(
    class1_2 = ifelse(attempt1SuccessV == "yes", ifelse(attempt2SuccessV=="yes","SS","SF"), ifelse(attempt2SuccessV=="yes","FS","FF")),
    class1_3 = ifelse(attempt1SuccessV == "yes", ifelse(attempt3SuccessV=="yes","SS","SF"), ifelse(attempt3SuccessV=="yes","FS","FF"))
  )


#### MEAN TABLE TO DIFFERENCES

sd3a <- satiated_data3 %>%
  group_by(class1_2) %>%
  summarise(
    n = n(),
    mean = mean(tot_time_diff_attempt_1_and_2),
    sd = sd(tot_time_diff_attempt_1_and_2),
    se = sd / sqrt(n),
    lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
    upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se
  ) %>%
  mutate_if(is.numeric, round, 1)

sd3b <- satiated_data3 %>%
  group_by(class1_3) %>%
  summarise(
    n = n(),
    mean = mean(tot_time_diff_attempt_1_and_3),
    sd = sd(tot_time_diff_attempt_1_and_3),
    se = sd / sqrt(n),
    lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
    upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se
  ) %>%
  mutate_if(is.numeric, round, 1)

ggplot(data = satiated_data3, aes(y=class1_2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  geom_density_ridges(aes(x = tot_time_diff_attempt_1_and_2, fill=randomseq),
                      alpha = .8, color = "white")

ggplot(data = satiated_data3, aes(y=class1_3)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  geom_density_ridges(aes(x = tot_time_diff_attempt_1_and_3, fill=randomseq),
                      alpha = .8, color = "white")

sd4 <- satiated_data3 %>%
  select(id, randomseq, attempt1intattemptTotalV, attempt2intattemptTotalV, attempt3intattemptTotalV) %>%
  gather(time, value, -randomseq, -id) %>%
  rowwise() %>%
  mutate(
    attempt = substr(time, 8, 8),
    success = ifelse(attempt == "1", satiated_data3$attempt1SuccessV[satiated_data3$id == id] , ifelse(attempt == "2", satiated_data3$attempt2SuccessV[satiated_data2$id == id] , satiated_data3$attempt3SuccessV[satiated_data2$id == id] ))
  ) %>%
  ungroup()

lotsmeans <- sd4 %>%
  group_by(attempt, success, randomseq) %>%
  summarise(
    n = n(),
    mean = mean(value),
    sd = sd(value)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

ggplot(data = sd4, aes(y=attempt)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  geom_density_ridges(aes(x = value, fill=randomseq),
                      alpha = .8, color = "white", from=0, to=90) +
  facet_wrap(~success)


# SANKEY

rand2attempt1 <- satiated_data3 %>%
  mutate(
    success = paste("1",attempt1SuccessV,sep="-")
  ) %>%
  select(randomseq, success) %>%
  rename(source = randomseq, target = success) %>%
  group_by(source, target) %>%
  summarise(n=n())

attempt12attempt2 <- satiated_data3 %>%
  mutate(
    attempt1 = paste("1",attempt1SuccessV,sep="-"),
    success = paste("2",attempt2SuccessV,sep="-")
  ) %>%
  select(attempt1, success) %>%
  rename(source = attempt1, target = success) %>%
  group_by(source, target) %>%
  summarise(n=n())

attempt22attempt3 <- satiated_data3 %>%
  mutate(
    attempt2 = paste("2",attempt2SuccessV,sep="-"),
    success = paste("3",attempt3SuccessV,sep="-")
  ) %>%
  select(attempt2, success) %>%
  rename(source = attempt2, target = success) %>%
  group_by(source, target) %>%
  summarise(n=n())




# https://stackoverflow.com/a/38275826/3650230
sanktify <- function(x) {
  
  # Create nodes DF with the unique sources & targets from input
  
  #  ***** changing this is the key***********************************************************
  nodes <- data.frame(unique(c(x$source,x$target)),stringsAsFactors=FALSE)
  # ************************************************************************************************
  nodes$ID <- as.numeric(rownames(nodes)) - 1 # sankeyNetwork requires IDs to be zero-indexed
  names(nodes) <- c("name", "ID")
  
  # use dplyr join over merge since much better; in this case not big enough to matter
  # Replace source & target in links DF with IDs
  links <<- inner_join(x, nodes, by = c("source"="name")) %>%
    rename(source_ID = ID) %>%
    inner_join(nodes, by = c("target"="name")) %>%
    rename(target_ID = ID) 
  
  # Create Sankey Plot
  sank <- sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source_ID",
    Target = "target_ID",
    Value = "n",
    NodeID = "name",
    fontSize = 14,
    fontFamily = "sans-serif",
    nodeWidth = 20,
    LinkGroup = "target", # For colouring the links (edges)
    nodePadding = 20,
    sinksRight = FALSE, # Prevents last node from right-justifying
    width = 1200,
    height = 500,
    iterations = 50
  )
  
  return(sank)
  
}

sanktify(data.frame(bind_rows(rand2attempt1, attempt12attempt2, attempt22attempt3)))


library(ggpubr)

ggerrorplot(sd4, x = "attempt", y = "value", 
            desc_stat = "mean_ci", 
            color = "black", palette = "jco",
            add = "jitter", add.params = list(color = "darkgray"),
            facet.by = c("randomseq","success")
)


ggviolin(sd4, x = "attempt", y = "value", fill="attempt",
            trim = TRUE,
            color = "darkgray", palette = "jco",
            add="mean_ci", add.params = list(color="black"),
            facet.by = c("randomseq","success") 
) 

gghistogram(sd4, x = "value",
            add = "mean", rug = TRUE,
            color = "randomseq", fill = "randomseq", bins = 20,
            palette = c("#00AFBB", "#E7B800"),
            facet.by = c("attempt","success"))

ggviolin(sd4, x = "attempt", y = "value", fill="attempt",
         trim = TRUE,
         color = "darkgray",
         palette = "jco",
         add = "boxplot", add.params = list(fill = "white"),
         facet.by = c("randomseq","success") 
) 

ggline(sd4, x = "attempt", y = "value", size=1,
       add = c("mean_ci", "jitter"), add.params = list(size = 0.8),
       color = "randomseq", palette = "jco",
       facet.by = c("success") 
)

sd5 <- satiated_data3 %>%
  select(id, randomseq, class1_2, class1_3, tot_time_diff_attempt_1_and_2, tot_time_diff_attempt_1_and_3) 
  
sd5a <- sd5 %>%
  filter(class1_2 != "FF") %>%
  select(-class1_2, -class1_3) %>%
  gather(time, value, -randomseq, -id)
  
sd5b <- sd5 %>%
  filter(class1_3 == "SS") %>%
  select(-class1_2, -class1_3) %>%
  gather(time, value, -randomseq, -id)

  ggline(sd5a, x = "time", y = "value", size=1,
         add = c("mean_ci", "jitter"), add.params = list(size = 0.8),
         color = "randomseq", palette = "jco")
  
  ggline(sd5b, x = "time", y = "value", size=1,
         add = c("mean_ci", "jitter"), add.params = list(size = 0.8),
         color = "randomseq", palette = "jco")
  
  ggline(sd4, x = "attempt", y = "value", size=1,
         add = c("mean_ci", "jitter"), add.params = list(size = 0.8),
         color = "randomseq", palette = c("#00AFBB", "#222222"),
         facet.by = c("success") 
  )
       
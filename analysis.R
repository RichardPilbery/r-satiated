# Analysis practice
library(tidyverse)
library(readr)
library(networkD3)
satiated_data <- read_csv("data/SATIATED-Results - Sheet1.csv")

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

saveRDS(satiated_data2, "data/data.rds")

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


demographicsTotal <- satiated_data2 %>%
  summarise(
    randomseq = "Total",
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

  demofinal <- t(rbind(demographics, demographicsTotal, make.row.names=F, stringsAsFactors=F))
  demofinal2 <- data.frame(demofinal, stringsAsFactors = F)
  colnames(demofinal2) <- c("AAB", "ABB", "Total")
  demofinal <- demofinal2
  
  finaldemodf <- data.frame(
    Measure = c("n", "Median intubation attempts in past 12 months (IQR)", "Median number of successful intubation attempts in past 12 months (IQR)", "Median years as paramedic (IQR)", "Familiar with SALAD technique"),
    AAB = c(demofinal$AAB[2], paste0(demofinal$AAB[3]," (",demofinal$AAB[4],")"), paste0(demofinal$AAB[6]," (",demofinal$AAB[7],")"), paste0(demofinal$AAB[9]," (",demofinal$AAB[10],")"), demofinal$AAB[11]),
    ABB = c(demofinal$ABB[2], paste0(demofinal$ABB[3]," (",demofinal$ABB[4],")"), paste0(demofinal$ABB[6]," (",demofinal$ABB[7],")"), paste0(demofinal$ABB[9]," (",demofinal$ABB[10],")"), demofinal$ABB[11]),
    Total = c(demofinal$Total[2], paste0(demofinal$Total[3]," (",demofinal$Total[4],")"), paste0(demofinal$Total[6]," (",demofinal$Total[7],")"), paste0(demofinal$Total[9]," (",demofinal$Total[10],")"), demofinal$Total[11]), stringsAsFactors = F
  )
  

  
  
### ------- Testing from here --------

library(psych)



pairs.panels(satiated_data2[c("intattempts","proportion_successful_intubations","yearsqualify")])

### ---- Analyse the results ------

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

### ---- Primary Outcome ------

options(scipen = 999)
primary_outcome <- tidy(prop.test(results$number_successful_2nd_attempt, results$n ,correct = F))

### ---- Secondary Outcome -------

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

AAB <- satiated_data2 %>%
  filter(randomseq == "AAB")

ABB <- satiated_data2 %>%
  filter(randomseq == "ABB")

AABprop <- results %>%
  filter(randomseq == "AAB")

ABBprop <- results %>%
  filter(randomseq == "ABB")

number_obsABB = ABBprop$n
number_obsAAB = AABprop$n

tot_meanDiff1and2 <- tidy(t.test(AAB$tot_time_diff_attempt_1_and_2[AAB$attempt1SuccessV=="yes"&AAB$attempt2SuccessV=="yes"], ABB$tot_time_diff_attempt_1_and_2[ABB$attempt1SuccessV=="yes"&ABB$attempt2SuccessV=="yes"]))

meanDiff1and2 <- tidy(t.test(AAB$time_diff_attempt_1_and_2[AAB$attempt1SuccessV=="yes"&AAB$attempt2SuccessV=="yes"], ABB$time_diff_attempt_1_and_2[ABB$attempt1SuccessV=="yes"&ABB$attempt2SuccessV=="yes"]))

sdDiff1and2x <- sd(AAB$tot_time_diff_attempt_1_and_2[AAB$attempt1SuccessV=="yes"&AAB$attempt2SuccessV=="yes"])
sdDiff1and2y <- sd(ABB$tot_time_diff_attempt_1_and_2[ABB$attempt1SuccessV=="yes"&ABB$attempt2SuccessV=="yes"])

# Compare mean time difference of AAB and ABB attempts 1 and 3 (A01-B01) with (A11-B12)
tot_meanDiff1and3 <- tidy(t.test(AAB$tot_time_diff_attempt_1_and_3[AAB$attempt1SuccessV=="yes"&AAB$attempt3SuccessV=="yes"], ABB$tot_time_diff_attempt_1_and_3[ABB$attempt1SuccessV=="yes"&ABB$attempt3SuccessV=="yes"]))

meanDiff1and3 <- tidy(t.test(AAB$time_diff_attempt_1_and_3[AAB$attempt1SuccessV=="yes"&AAB$attempt3SuccessV=="yes"], ABB$time_diff_attempt_1_and_3[ABB$attempt1SuccessV=="yes"&ABB$attempt3SuccessV=="yes"]))

sdDiff1and3x <- sd(AAB$tot_time_diff_attempt_1_and_3[AAB$attempt1SuccessV=="yes"&AAB$attempt3SuccessV=="yes"])
sdDiff1and3y <- sd(ABB$tot_time_diff_attempt_1_and_3[ABB$attempt1SuccessV=="yes"&ABB$attempt3SuccessV=="yes"])

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


secondary_outcome2 <- tidy(prop.test(results$number_successful_3rd_attempt, results$n ,correct = F))

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


### ------ SANKEY ------

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
       
  
### ------ Time plots ------
  
df <- readRDS('data/data.rds')  
  
timedf <- satiated_data2 %>%
  select(randomseq, attempt1intattemptStartV, attempt2intattemptStartV, attempt3intattemptStartV, attempt1intattemptCompletedV, attempt1intattemptTotalV, attempt2intattemptCompletedV, attempt2intattemptTotalV, attempt3intattemptCompletedV, attempt3intattemptTotalV, attempt1SuccessV, attempt2SuccessV, attempt3SuccessV, bougie1, bougie2, bougie3) %>%
  mutate(
    id = row_number()
  ) %>%
  gather(key, value, -randomseq, -id) %>%
  mutate(
    attempt = as.factor(substr(key,8,8))
  ) %>%
  group_by(id, randomseq, attempt) %>%
  summarise(
    success = ifelse(value[grepl("Success",key)] == "yes", 1, 0),
    time_to_int = as.integer(value[grepl("Start", key)]),
    tot_time = as.integer(value[grepl("Total",key)]),
    int_time = as.integer(value[grepl("Completed",key)]),
    bougie = ifelse(is.na(value[grep('bougie',key)]), 1, 0)
  )

timedf2 <- timedf %>%
  mutate(
    success = ifelse(success == 1, "Successful Intubation", "Failure Intubation")
  )

ggplot(timedf2, aes(x = attempt, y = tot_time, fill=randomseq)) +
  geom_boxplot() +
  facet_wrap(~success) +
  theme_minimal() +
  scale_fill_brewer(name="Randomisation\nsequence") +
  scale_y_continuous(name="Time (seconds)", breaks = seq(0,90,10), limits=c(20,90)) +
  scale_x_discrete(name="Attempt number")

ggplot(timedf2, aes(x = attempt, y = int_time, fill=randomseq)) +
  geom_boxplot() +
  facet_wrap(~success) +
  theme_minimal() +
  theme(legend.position="top") +
  scale_fill_brewer(name="Randomisation sequence") +
  scale_y_continuous(name="Time (seconds)", breaks = seq(0,90,10), limits=c(20,90)) +
  scale_x_discrete(name="Attempt number")


#### ----- Summary table for results ------

timedf3 <- timedf %>%
  filter(success==1) %>%
  group_by(attempt, randomseq) %>%
  summarise(
    nsuccess = n(),
    pcsuccess = nsuccess/82,
    medt2int = median(time_to_int),
    medint = median(int_time),
    medtotal = median(tot_time),
    lqtrt2int = fivenum(time_to_int)[2],
    lqtrint = fivenum(int_time)[2],
    lqtrtotal = fivenum(tot_time)[2],
    uqtrt2int = fivenum(time_to_int)[4],
    uqtrint = fivenum(int_time)[4],
    uqtrtotal = fivenum(tot_time)[4]
  )


#### ------ Plot Mean differences ------

attemptDiff <- satiated_data %>%
  mutate(
    class1_2 = ifelse(attempt1SuccessV == "yes", ifelse(attempt2SuccessV=="yes","SS","SF"), ifelse(attempt2SuccessV=="yes","FS","FF")),
    class1_3 = ifelse(attempt1SuccessV == "yes", ifelse(attempt3SuccessV=="yes","SS","SF"), ifelse(attempt3SuccessV=="yes","FS","FF"))
  ) %>%
  select(randomseq, class1_2, class1_3, time_diff_attempt_1_and_2, time_diff_attempt_1_and_3) %>%
  filter(class1_2 == "SS" | class1_3 == "SS") %>%
  gather(key, value, -randomseq, -class1_2, -class1_3) %>%
  mutate(
    one2two = ifelse(class1_2 == "SS" & key == "time_diff_attempt_1_and_2", value, NA),
    one2three = ifelse(class1_3 == "SS" & key == "time_diff_attempt_1_and_3" , value, NA)
  ) %>%
  select(randomseq, one2two, one2three) %>%
  gather(key, value, -randomseq) %>%
  na.omit()

lotsmeans <- attemptDiff %>%
  group_by(key, randomseq) %>%
  summarise(
    n = n(),
    mean = mean(value),
    sd = sd(value)
  ) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
  mutate_if(is.numeric, round, 1)
  

library(ggpubr)
library(viridis)
ggline(attemptDiff, x= "key", y = "value", size=1,
       add = c("mean_ci"), add.params = list(size = 0.8),
       palette = c("#00AFBB", "#222222"), color = "randomseq"
)


summaryExtras <- satiated_data2 %>%
  group_by(randomseq) %>%
  summarise(
    bougie1 = sum(!is.na(bougie1)),
    bougie2 = sum(!is.na(bougie2)),
    bougie3 = sum(!is.na(bougie3)),
    assthold1 = sum(!is.na(assthold1)),
    assthold2 = sum(!is.na(assthold2)),
    assthold3 = sum(!is.na(assthold3)),
    sucinsitu1 = sum(!is.na(sucinsitu1)),
    sucinsitu2 = sum(!is.na(sucinsitu2)),
    sucinsitu3 = sum(!is.na(sucinsitu3)),
    nohole1 = sum(!is.na(Nohole1)),
    nohole2 = sum(!is.na(Nohole2)),
    nohole3 = sum(!is.na(Nohole3))
  )

summaryExtrasSF <- satiated_data2 %>%
  group_by(randomseq) %>%
  summarise(
    bougie1S = sum(!is.na(bougie1) & attempt1SuccessV == "yes"),
    bougie2S = sum(!is.na(bougie2) & attempt2SuccessV == "yes"),
    bougie3S = sum(!is.na(bougie3) & attempt3SuccessV == "yes"),
    assthold1S = sum(!is.na(assthold1) & attempt1SuccessV == "yes"),
    assthold2S = sum(!is.na(assthold2) & attempt2SuccessV == "yes"),
    assthold3S = sum(!is.na(assthold3) & attempt3SuccessV == "yes"),
    sucinsitu1S = sum(!is.na(sucinsitu1) & attempt1SuccessV == "yes"),
    sucinsitu2S = sum(!is.na(sucinsitu2) & attempt2SuccessV == "yes"),
    sucinsitu3S = sum(!is.na(sucinsitu3) & attempt3SuccessV == "yes"),
    nohole1S = sum(!is.na(Nohole1) & attempt1SuccessV == "yes"),
    nohole2S = sum(!is.na(Nohole2) & attempt2SuccessV == "yes"),
    nohole3S = sum(!is.na(Nohole3) & attempt3SuccessV == "yes"),
    bougie1F = sum(!is.na(bougie1) & attempt1SuccessV == "no"),
    bougie2F = sum(!is.na(bougie2) & attempt2SuccessV == "no"),
    bougie3F = sum(!is.na(bougie3) & attempt3SuccessV == "no"),
    assthold1F = sum(!is.na(assthold1) & attempt1SuccessV == "no"),
    assthold2F = sum(!is.na(assthold2) & attempt2SuccessV == "no"),
    assthold3F = sum(!is.na(assthold3) & attempt3SuccessV == "no"),
    sucinsitu1F = sum(!is.na(sucinsitu1) & attempt1SuccessV == "no"),
    sucinsitu2F = sum(!is.na(sucinsitu2) & attempt2SuccessV == "no"),
    sucinsitu3F = sum(!is.na(sucinsitu3) & attempt3SuccessV == "no"),
    nohole1F = sum(!is.na(Nohole1) & attempt1SuccessV == "no"),
    nohole2F = sum(!is.na(Nohole2) & attempt2SuccessV == "no"),
    nohole3F = sum(!is.na(Nohole3) & attempt3SuccessV == "no")
  )

summaryExtras2 <- summaryExtras %>%
  gather(key, value, -randomseq) %>%
  mutate(
    attempt = str_sub(key, -1, -1),
    new_key = str_sub(key, 1, -2)
  ) %>%
  select(randomseq, new_key, value, attempt) %>%
  mutate(
    attempt = paste("Attempt ", attempt, sep=" ")
  )

summaryExtras3 <- summaryExtrasSF %>%
  gather(key, value, -randomseq) %>%
  mutate(
    attempt = str_sub(key, -2, -2),
    new_key = str_sub(key, 1, -3),
    success = ifelse(str_sub(key, -1, -1) == "S", "Successful Attempt", "Failed Attempt")
  ) %>%
  select(randomseq, new_key, value, attempt, success) %>%
  mutate(
    attempt = paste("Attempt ", attempt, sep=" ")
  )


ggplot(summaryExtras2, aes(x = new_key, y = value, fill = randomseq)) +
  geom_bar(stat = "identity", position="dodge", colour="black") +
  facet_wrap(~attempt) +
  theme_minimal() +
  theme(legend.position="top") +
  scale_fill_brewer(name="Randomisation sequence") +
  scale_y_continuous(name="Number", breaks = seq(0,15,1), limits=c(0,15)) +
  scale_x_discrete(name="Characteristic", labels=c("Assistant\nheld\nsuction", "No bougie\nused", "Suction\nhole not\noccluded", "Suction left\nin situ"))

ggplot(summaryExtras3, aes(x = new_key, y = value, fill = randomseq)) +
  geom_bar(stat = "identity", position="dodge", colour="black") +
  facet_wrap(~attempt + success, dir="v", nrow=2, ncol=3) +
  theme_minimal() +
  theme(legend.position="top") +
  scale_fill_brewer(name="Randomisation sequence") +
  scale_y_continuous(name="Number", breaks = seq(0,10,1), limits=c(0,10)) +
  scale_x_discrete(name="Characteristic", labels=c("Assistant\nheld\nsuction", "No bougie\nused", "Suction\nhole not\noccluded", "Suction left\nin situ")) 

ggplot(summaryExtras2, aes(x = new_key, y = value, fill = attempt)) +
  geom_bar(stat = "identity", position="dodge") +
  facet_wrap(~randomseq) +
  theme_minimal() +
  theme(legend.position="top") +
  scale_fill_brewer(name="Randomisation sequence") +
  scale_y_continuous(name="Number", breaks = seq(0,15,1), limits=c(0,15)) +
  scale_x_discrete(name="Charateristic", labels=c("Assistant held suction", "No bougie used", "Suction hole not occluded", "Suction left in situ"))



bougieProbe <- satiated_data2 %>%
  select(bougie1, bougie2, bougie3, randomseq, attempt1SuccessV, attempt2SuccessV, attempt3SuccessV) %>%
  mutate(
    id = row_number()
  ) %>%
  gather(key, value, -randomseq, -id) %>%
  mutate(
    attempt = as.factor(ifelse(str_sub(key, 1, 6) == "bougie", str_sub(key,7,7) , substr(key,8,8)))
  ) %>%
  group_by(id, randomseq, attempt) %>%
  summarise(
    success = ifelse(value[grepl("Success",key)] == "yes", 1, 0),
    bougie = ifelse(is.na(value[grepl("bougie", key)]), 1, 0)
  ) %>%
  ungroup() %>%
  select(success, bougie) %>%
  count(success, bougie)



ggplot(summaryExtras2, aes(x = new_key, y = value, fill = randomseq)) +
  geom_bar(stat = "identity", position="dodge", colour="black") +
  facet_wrap(~attempt) +
  theme_minimal() +
  theme(legend.position="top", text=element_text(family="Arial"), axis.title.y = element_text(size=16,margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.title.x = element_text(size=16,margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  scale_fill_brewer(name="Randomisation group") +
  scale_y_continuous(name="Number of occurences", breaks = seq(0,15,1), limits=c(0,15)) +
  scale_x_discrete(name="Technique adaptions/omissions", labels=c("Assistant\nheld\nsuction", "No bougie\nused", "Suction\nhole not\noccluded", "Suction left\nin situ")) +
  facet_wrap(~success)

timedf <- satiated_data2 %>%
  select(randomseq, attempt1intattemptStartV, attempt2intattemptStartV, attempt3intattemptStartV, attempt1intattemptCompletedV, attempt1intattemptTotalV, attempt2intattemptCompletedV, attempt2intattemptTotalV, attempt3intattemptCompletedV, attempt3intattemptTotalV, attempt1SuccessV, attempt2SuccessV, attempt3SuccessV) %>%
  mutate(
    id = row_number()
  ) %>%
  gather(key, value, -randomseq, -id) %>%
  mutate(
    attempt = as.factor(substr(key,8,8))
  ) %>%
  group_by(id, randomseq, attempt) %>%
  summarise(
    success = ifelse(value[grepl("Success",key)] == "yes", 1, 0),
    time_to_int = as.integer(value[grepl("Start", key)]),
    tot_time = as.integer(value[grepl("Total",key)]),
    int_time = as.integer(value[grepl("Completed",key)])
  )

timedf2 <- timedf %>%
  mutate(
    success = ifelse(success == 1, "Successful Intubation", "Failed Intubation")
  ) %>%
  filter(success == "Successful Intubation")

give.n <- function(x){
  # https://stackoverflow.com/questions/3483203/create-a-boxplot-in-r-that-labels-a-box-with-the-sample-size-n
  # Used data.frame instead of c so I could add 'n=' to label
  #return(data.frame(y = (fivenum(x)[1])-2, label = paste0("n=",length(x))  ))
  return(data.frame(y = 19, label = paste0("n=",length(x))  ))
}

ggplot(timedf2, aes(x = attempt, y = int_time, fill=randomseq)) +
  geom_boxplot() +
  #facet_wrap(~success) +
  theme_minimal() +
  theme(legend.position="top", text=element_text(size=16, family="Arial"), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  scale_fill_brewer(name="Randomisation group") +
  #scale_fill_manual(name="Randomisation group", values=c("#edf3f5","#0069b4")) +
  scale_y_continuous(name="Intubation attempt time (seconds)", breaks = seq(0,90,10), limits=c(15,90)) +
  scale_x_discrete(name="Attempt number") +
  stat_summary(fun.data = give.n, geom = "text", position = position_dodge(width = 0.75), size=5)

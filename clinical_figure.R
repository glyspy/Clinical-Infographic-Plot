#create infographic for the clinical data of the cohorts

library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(scales)
library(patchwork)

# run this in every cancer project and then import the .csv files
# CHOL_clin <-data.frame(row.names = colnames(tcga_CHOLprim),
#                        pathologic_stage=as.vector(tcga_CHOLprim$ajcc_pathologic_stage),
#                        age_diagnosis=as.vector(tcga_CHOLprim$age_at_diagnosis),
#                        days_to_last_follow_up=as.vector(tcga_CHOLprim$days_to_last_follow_up),
#                        vital_status=as.vector(tcga_CHOLprim$vital_status),
#                        days_to_death=as.vector(tcga_CHOLprim$days_to_death),
#                        prior_malignancy=as.vector(tcga_CHOLprim$prior_malignancy),
#                        histologic_subtype=as.vector(tcga_CHOLprim$`paper_histologic subtype`),
#                        site_of_resection_or_biopsy=as.vector(tcga_CHOLprim$site_of_resection_or_biopsy),
#                        gender=as.vector(tcga_CHOLprim$gender),
#                        ethnicity=as.vector(tcga_CHOLprim$ethnicity),
#                        project="CHOL"
# )
# 
# write.csv(x = CHOL_clin, file = "~/Documents/GitHub/Infographic/CHOL_clin.csv")

# length(tcga_GBM$sample_type)
# length(tcga_GBM$ajcc_pathologic_stage)
# length(tcga_GBM$age_at_diagnosis)
# length(tcga_GBM$days_to_last_follow_up)
# length(tcga_GBM$vital_status)
# length(tcga_GBM$days_to_death)
# length(tcga_GBM$prior_malignancy)
# length(tcga_GBM$`paper_histologic_subtype`)
# length(tcga_GBM$site_of_resection_or_biopsy)
# length(tcga_GBM$gender)
# length(tcga_GBM$ethnicity)


# IMPORT DFs
CHOL_clin <- read.csv("CHOL_clin.csv")
KICH_clin <- read.csv("KICH_clin.csv")
PCPG_clin <- read.csv("PCPG_clin.csv")
SARC_clin <- read.csv("SARC_clin.csv")
THYM_clin <- read.csv("THYM_clin.csv")
GBM_clin <- read.csv("GBM_clin.csv")

# select only primary tumor samples
CHOL_clin <- CHOL_clin[CHOL_clin$sample_type=="Primary Tumor",]
KICH_clin <- KICH_clin[KICH_clin$sample_type=="Primary Tumor",]
PCPG_clin <- PCPG_clin[PCPG_clin$sample_type=="Primary Tumor",]
SARC_clin <- SARC_clin[SARC_clin$sample_type=="Primary Tumor",]
THYM_clin <- THYM_clin[THYM_clin$sample_type=="Primary Tumor",]
GBM_clin <- GBM_clin[GBM_clin$sample_type=="Primary Tumor",]

colnames(CHOL_clin)[11] <- "sex"
colnames(KICH_clin)[11] <- "sex"
colnames(PCPG_clin)[11] <- "sex"
colnames(SARC_clin)[11] <- "sex"
colnames(THYM_clin)[11] <- "sex"
colnames(GBM_clin)[11] <- "sex"

# merge all cancers
cancers_clin <- rbind(CHOL_clin, KICH_clin, PCPG_clin, SARC_clin, THYM_clin, GBM_clin)

# change not reported and NA values
cancers_clin[is.na(cancers_clin)] <- "Not Reported"
cancers_clin[cancers_clin=="not reported"] <- "Not Reported"

# convert days to months
cancers_clin$days_to_last_follow_up <- as.numeric(cancers_clin$days_to_last_follow_up)
cancers_clin$months_to_last_follow_up <- cancers_clin$days_to_last_follow_up / 30

# convert days to years
cancers_clin$age_diagnosis <- as.numeric(cancers_clin$age_diagnosis)
cancers_clin$age_diagnosis <- floor(cancers_clin$age_diagnosis / 365)


write.csv(x = cancers_clin, file = "cancers_infographic_data.csv")


# Create individual plots per clinical attribute
# SEX
cancers_sex <- cancers_clin %>%
  group_by(project, sex) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(project) %>%
  mutate(project_total = sum(count)) %>%
  mutate(percentage = 100 * count / project_total) %>%
  ungroup()

sex_plots <- ggplot(cancers_sex, aes(x = project, y = percentage, fill = sex)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#F8AFA8", "#46ACC8", "#D9D0D3"),
                    labels = c("Female", "Male", "Not Reported"),
                    "Sex") +
  ggtitle("Sex") + labs(y="Percentage", x="") + 
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = c(0.25, 0.5, 0.75))
  
  
  # ETHNICITY
  cancers_ethnicity <- cancers_clin %>%
  group_by(project, ethnicity) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(project) %>%
  mutate(project_total = sum(count)) %>%
  mutate(percentage = 100 * count / project_total) %>%
  ungroup()

ethnicity_plots <- ggplot(cancers_ethnicity, aes(x = project, y = percentage, fill = ethnicity)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#90D4CC", "#FDD262", "#D9D0D3"),
                    labels = c("Hispanic or Latino", "Not Hispanic or Latino", "Not Reported"),
                    "Ethnicity") +
  ggtitle("Ethnicity") +
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = c(0.25, 0.5, 0.75))


# VITAL STATUS
cancers_vital <- cancers_clin %>%
  group_by(project, vital_status) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(project) %>%
  mutate(project_total = sum(count)) %>%
  mutate(percentage = 100 * count / project_total) %>%
  ungroup()

vital_plots <- ggplot(cancers_vital, aes(x = project, y = percentage, fill = vital_status)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#7fc9a9","#0B775E", "#D9D0D3"),
                    labels = c("Alive", "Dead", "Not reported"),
                    "Vital Status") +
  ggtitle("Vital Status") +
  #scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "", y="Percentage") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = c(0.25, 0.5, 0.75))


# PRIOR MALIGNANCY STATUS
cancers_mal <- cancers_clin %>%
  group_by(project, prior_malignancy) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(project) %>%
  mutate(project_total = sum(count)) %>%
  mutate(percentage = 100 * count / project_total) %>%
  ungroup()

mal_plots <- ggplot(cancers_mal, aes(x = project, y = percentage, fill = prior_malignancy)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#899DA4","#C93312", "#D9D0D3"),
                    labels = c("Yes", "No", "Not reported"),
                    "Prior Malignancy") +
  ggtitle("Prior Malignancy") +
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  labs(x = "", y ="Percentage") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = c(0.25, 0.5, 0.75))


# AGE AT DIAGNOSIS BOXPLOTS
age_diagn_plots <- ggplot(cancers_clin, aes(x = project, y = age_diagnosis)) +
  geom_boxplot(color = "#514ead", fill = "#b2b5d6") +
  coord_flip() +
  ggtitle("Age at Diagnosis") +
  labs(y = "Years") +
  facet_grid(scales = "fixed", space = "free") +
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "") +
  scale_y_continuous(limits = c(0, 100))


# DAYS TO LAST FOLLOW-UP
months_folowup_plots <- ggplot(cancers_clin, aes(x = project, y = months_to_last_follow_up)) +
  geom_boxplot(color = "#F98400", fill = "#FDD262") +
  coord_flip() +
  ggtitle("Last Follow-up") +
  labs(y = "Months") +
  facet_grid(scales = "fixed", space = "free") +
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "") +
  scale_y_continuous(limits = c(0, 230),
                     breaks = c(0,100,200)) 




# Combine the plots using patchwork and adjust the layout
combined_plots <- 
  sex_plots + theme_pubclean() + labs_pubr() + theme(legend.position = "right",
                                                     plot.title = element_text(hjust = 0.5),
                                                     legend.key.size = unit(0.5, "cm")) +
  font("title", 19) +
  font("ylab", size = 15) +
  font("xlab", size = 17) +
  font("y.text", size = 20) +
  font("x.text", size = 15) +
  font("legend.title", size = 14) +
  font("legend.text", size = 12) +
  ethnicity_plots + theme_pubclean() + labs_pubr() + theme(legend.position = "right",
                                                           plot.title = element_text(hjust = 0.5),
                                                           legend.key.size = unit(0.5, "cm")) +
  font("title", 19) +
  font("ylab", size = 15) +
  font("xlab", size = 17) +
  font("y.text", size = 20) +
  font("x.text", size = 15) +
  font("legend.title", size = 14) +
  font("legend.text", size = 12) +
  age_diagn_plots + theme_pubclean() + labs_pubr() + theme(legend.position = "right",
                                                           plot.title = element_text(hjust = 0.5),
                                                           legend.key.size = unit(0.5, "cm")) +
  font("title", 19) +
  font("ylab", size = 15) +
  font("xlab", size = 17) +
  font("y.text", size = 20) +
  font("x.text", size = 15) +
  font("legend.title", size = 14) +
  font("legend.text", size = 12) +
  vital_plots + theme_pubclean() + labs_pubr() + theme(legend.position = "right",
                                                       plot.title = element_text(hjust = 0.5),
                                                       legend.key.size = unit(0.5, "cm")) +
  font("title", 19) +
  font("ylab", size = 15) +
  font("xlab", size = 17) +
  font("y.text", size = 20) +
  font("x.text", size = 15) +
  font("legend.title", size = 14) +
  font("legend.text", size = 12) +
  mal_plots + theme_pubclean() + labs_pubr() + theme(legend.position = "right",
                                                     plot.title = element_text(hjust = 0.5),
                                                     legend.key.size = unit(0.5, "cm")) +
  font("title", 19) +
  font("ylab", size = 15) +
  font("xlab", size = 17) +
  font("y.text", size = 20) +
  font("x.text", size = 15) +
  font("legend.title", size = 14) +
  font("legend.text", size = 12) +
  months_folowup_plots + theme_pubclean() +labs_pubr() + theme(legend.position = "right",
                                                               plot.title = element_text(hjust = 0.5),
                                                               legend.key.size = unit(0.5, "cm")) +
  font("title", 19) +
  font("ylab", size = 15) +
  font("xlab", size = 17) +
  font("y.text", size = 20) +
  font("x.text", size = 15) +
  font("legend.title", size = 14) +
  font("legend.text", size = 12) +
  plot_layout(guides = "collect",
              ncol = 3,
              nrow = 2
              # widths = c(5,5,5,5,5,5),
              # heights = c(25),
              # align = "center"
  )


ggsave(filename = "clinical_info.pdf",
       plot = combined_plots,
       width = 12,
       height = 6)


print(combined_plots)

dev.off()

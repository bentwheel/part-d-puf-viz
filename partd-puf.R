library(tidyverse)
library(lubridate)
library(viridis)
library(shiny)
library(INLA)
library(here)

# Download Part D PUF to recreate analysis
# 2016

# Open this URL in your browser to download data, then upload to RStudio Directory
# blob:https://data.cms.gov/ff5d1501-4e46-4e3c-a924-0a86e50e06ca

# Name of 2016 Part D Prescriber Summary Table file
puf_2016_pst_csv <- "MUP_DPR_RY21_P04_V10_DY16_NPI.csv"

# Read CSV file into in-memory dataset
puf_2016_pst <- read_csv(puf_2016_pst_csv)

# Download and install INLA per instructions here:
# https://www.r-inla.org/download-install

# Run once
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# commentary on how hard it was to get these packages up and running - in many cases RStudio dependencies are difficult to install in
# a locked down environment due to actuaries typically having access to PHI

# How many zip codes are represented? Sum up Brand and Generic dispenses at the total claims level
# Remove any records where Brand or Generic counts are suppressed

pst_by_zip5 <- puf_2016_pst %>%
  filter(!is.na(Brnd_Tot_Clms)) %>% 
  filter(!is.na(Gnrc_Tot_Clms)) %>% 
  filter(!is.na(Tot_Benes)) %>% 
  mutate(GDR = Gnrc_Tot_Clms/Tot_Clms,
         BDR = Brnd_Tot_Clms/Tot_Clms,
         TotalCheck = Gnrc_Tot_Clms + Brnd_Tot_Clms + Othr_Tot_Clms - Tot_Clms,
         Prscrbr_zip3 = str_sub(Prscrbr_zip5, 1, 3),
         Bene_Agg_Sum_Risk_Scre = Tot_Benes * Bene_Avg_Risk_Scre,
         Bene_Agg_Sum_Age = Tot_Benes * Bene_Avg_Age) %>% 
  select(PRSCRBR_NPI, Prscrbr_Type, Prscrbr_RUCA, Prscrbr_RUCA_Desc, Prscrbr_zip5, Prscrbr_zip3, Prscrbr_State_Abrvtn, Tot_Benes, Bene_Avg_Risk_Scre, Bene_Agg_Sum_Risk_Scre, Bene_Avg_Age, Bene_Agg_Sum_Age, Brnd_Tot_Clms, Gnrc_Tot_Clms, Tot_Clms, BDR, GDR, TotalCheck)

# Aggregation by zip3 - will we have ~530 triangles?
# Looks like we have about 919, all said and done - still a reasonable sample,
# talk about political/legal/socioeconomic effects of this 
# - Generic Substitution Laws
# - Duals - which states have more dual eligibles due to expansion of Medicaid?
# - name of 3rd party payer might also be important, due to the fact that different plans have PA/UM requirements

pst_by_zip3 <- pst_by_zip5 %>%
  group_by(Prscrbr_zip3) %>% 
  summarize(Tot_Benes = sum(Tot_Benes),
            Bene_Agg_Sum_Risk_Scre = sum(Bene_Agg_Sum_Risk_Scre),
            Bene_Agg_Sum_Age = sum(Bene_Agg_Sum_Age),
            Brnd_Tot_Clms = sum(Brnd_Tot_Clms),
            Gnrc_Tot_Clms = sum(Gnrc_Tot_Clms),
            Tot_Clms = sum(Tot_Clms)) %>% 
  ungroup() %>% 
  mutate(Bene_Avg_Risk_Scre = Bene_Agg_Sum_Risk_Scre / Tot_Benes,
         Bene_Avg_Age = Bene_Agg_Sum_Age / Tot_Benes,
         BDR = Brnd_Tot_Clms / Tot_Clms,
         GDR = Gnrc_Tot_Clms / Tot_Clms)

pst_by_state <- pst_by_zip5 %>%
  group_by(Prscrbr_State_Abrvtn) %>% 
  summarize(Tot_Benes = sum(Tot_Benes),
            Bene_Agg_Sum_Risk_Scre = sum(Bene_Agg_Sum_Risk_Scre),
            Bene_Agg_Sum_Age = sum(Bene_Agg_Sum_Age),
            Brnd_Tot_Clms = sum(Brnd_Tot_Clms),
            Gnrc_Tot_Clms = sum(Gnrc_Tot_Clms),
            Tot_Clms = sum(Tot_Clms)) %>% 
  ungroup() %>% 
  mutate(Bene_Avg_Risk_Scre = Bene_Agg_Sum_Risk_Scre / Tot_Benes,
         Bene_Avg_Age = Bene_Agg_Sum_Age / Tot_Benes,
         BDR = Brnd_Tot_Clms / Tot_Clms,
         GDR = Gnrc_Tot_Clms / Tot_Clms)

# Now recreate INLA analysis? Seems too much to do, and hard to re-create, very arbitrary.

# Step 1 - noncensus package
# devtools::install_github('ramhiser/noncensus')
library(noncensus)
data(zip_codes)
data(states)




# More on this later...



# Now let's look at summary statistics table and histogram as presented on the top of p. 208.

# GDR by state, with notes on Medicaid Expansion states as of 2016, as well as states with mandatory Generic Substitution
# Mandatory substitution: https://www.ncsl.org/portals/1/documents/health/Generic_Drug_Substitution_Laws_32193.pdf
# Medicaid Expansion: https://www.kff.org/medicaid/issue-brief/status-of-state-medicaid-expansion-decisions-interactive-map/
# Also should control for Provider specialty

# Get total avg stats

pst_by_ALL <- pst_by_zip5 %>%
  summarize(Tot_Benes = sum(Tot_Benes),
            Bene_Agg_Sum_Risk_Scre = sum(Bene_Agg_Sum_Risk_Scre),
            Bene_Agg_Sum_Age = sum(Bene_Agg_Sum_Age),
            Brnd_Tot_Clms = sum(Brnd_Tot_Clms),
            Gnrc_Tot_Clms = sum(Gnrc_Tot_Clms),
            Tot_Clms = sum(Tot_Clms)) %>% 
  ungroup() %>% 
  mutate(Bene_Avg_Risk_Scre = Bene_Agg_Sum_Risk_Scre / Tot_Benes,
         Bene_Avg_Age = Bene_Agg_Sum_Age / Tot_Benes,
         BDR = Brnd_Tot_Clms / Tot_Clms,
         GDR = Gnrc_Tot_Clms / Tot_Clms)

nationwide_avg_bdr <- pst_by_ALL$BDR
nationwide_avg_bdr

# Join to state names and to Medicaid Expansion Status and Generic Subst. Law status

# Download expansion states data
library(curl)
med_exp_states_csv <- curl_download(url = "https://www.kff.org/wp-content/uploads/2021/08/expansion-status-interactive-map_9.8.2021.csv",
                                    destfile = here::here("med_exp_states.csv"))


med_exp_states <- read_csv(med_exp_states_csv) %>% 
  filter(`Expansion Status`=="Adopted and Implemented",
         str_sub(Description, 1, 25) == "Implemented expansion on ") %>% 
  mutate(effdate = mdy(map_chr(Description, function(x) str_split(str_sub(x, 26), pattern=' ', simplify=T)[1]))) %>% 
  filter(effdate <= lubridate::ymd(20160101)) %>% 
  transmute(name = State) %>% 
  mutate(Bool_Expansion = T)

# States with Mandatory Generic substition laws
# https://www.ncsl.org/portals/1/documents/health/Generic_Drug_Substitution_Laws_32193.pdf

# "To increase access to generic drugs, some state lawmakers have pursued allowing a pharmacist to make a generic drug
# substitution. Twelve states and one territory require a pharmacist to replace a brand-name drug with a generic if all
# other prescribing requirements are met: Florida, Kansas, Kentucky, Massachusetts, Minnesota, Nevada, New Jersey,
# New York, Pennsylvania, Puerto Rico, Vermont and Washington. "

gen_subst_states <- read_csv(med_exp_states_csv) %>% 
  transmute(name = State) %>% 
  filter(name == "Florida" | 
           name == "Kansas" |
           name == "Kentucky" |
           name == "Massachusetts" |
           name == "Minnesota" |
           name == "Nevada" |
           name == "New Jersey" |
           name == "New York" |
           name == "Pennsylvania" |
           name == "Puerto Rico" |
           name == "Vermont" |
           name == "Washington") %>% 
  mutate(Bool_GenSubst = T)


pst_by_state_viz <- pst_by_state %>% 
  inner_join(states, by=c("Prscrbr_State_Abrvtn"="state")) %>% 
  left_join(med_exp_states) %>% 
  left_join(gen_subst_states) %>% 
  mutate(Medicaid_Expansion = if_else(!is.na(Bool_Expansion), "Adopted before 1/1/2016", "Not adopted before 1/1/2016")) %>% 
  filter(!is.na(region)) %>% 
  mutate(Generic_Substitution_Law = if_else(!is.na(Bool_GenSubst), "Legally mandated", "Not legally mandated"))

# Medicare Expansion Color View
pst_by_state_gg_1 <- ggplot(data=pst_by_state_viz,
                          mapping = aes(x = BDR,
                                        y = reorder(name, BDR),
                                        size = Tot_Benes,
                                        color=Medicaid_Expansion)) + 
  guides(size = "none") +
  scale_color_viridis(discrete=T) +
  theme_bw() +
  geom_vline(xintercept = nationwide_avg_bdr, color="gray30") +
  labs(title="Brand Drug Dispense Rate by State by Medicaid Expansion Status",
       subtitle = "Medicare Part D Claims",
       x = "Brand Dispense Rate (BDR)",
       y = "State",
       color = "Medicaid Expansion",
       caption = "Based on Calendar Year 2016 Part D Prescriber Public Use File (PUF) located at
                  https://www.hhs.gov/guidance/document/medicare-provider-utilization-and-payment-data-part-d-prescriber-0") + 
  geom_point() +
  scale_x_continuous(breaks = c(0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.24, 0.25),
                     labels = c("14%", "15%", "16%", "17%", "18%", "19%", "20%", "21%", "22%", "23%", "24%", "25%")) +
  facet_wrap(~ region, ncol = 2, scales="free_y")

pst_by_state_gg_1

# Mandatory Generic Substitution View
pst_by_state_gg_2 <- ggplot(data=pst_by_state_viz,
                            mapping = aes(x = BDR,
                                          y = reorder(name, BDR),
                                          size = Tot_Benes,
                                          color=Generic_Substitution_Law)) + 
  guides(size = "none") +
  scale_color_viridis(discrete=T) +
  theme_bw() +
  geom_vline(xintercept = nationwide_avg_bdr, color="gray30") +
  labs(title="Brand Drug Dispense Rate by State by Generic Substitution Mandate Status",
       subtitle = "Medicare Part D Claims",
       x = "Brand Dispense Rate (BDR)",
       y = "State",
       color = "Generic Substitution for\nMore Expensive Brand Drugs",
       caption = "Based on Part D Prescriber Public Use File (PUF) located at
                  https://www.hhs.gov/guidance/document/medicare-provider-utilization-and-payment-data-part-d-prescriber-0") + 
  geom_point() +
  scale_x_continuous(breaks = c(0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.23, 0.24, 0.25),
                     labels = c("14%", "15%", "16%", "17%", "18%", "19%", "20%", "21%", "22%", "23%", "24%", "25%")) +
  facet_wrap(~ region, ncol = 2, scales="free_y")

pst_by_state_gg_2

# BDR by Provider Specialty 
pst_by_type <- pst_by_zip5 %>%
  inner_join(states, by=c("Prscrbr_State_Abrvtn"="state")) %>% 
  group_by(Prscrbr_Type, division) %>% 
  summarize(Tot_Benes = sum(Tot_Benes),
            Bene_Agg_Sum_Risk_Scre = sum(Bene_Agg_Sum_Risk_Scre),
            Bene_Agg_Sum_Age = sum(Bene_Agg_Sum_Age),
            Brnd_Tot_Clms = sum(Brnd_Tot_Clms),
            Gnrc_Tot_Clms = sum(Gnrc_Tot_Clms),
            Tot_Clms = sum(Tot_Clms)) %>% 
  ungroup() %>% 
  mutate(Bene_Avg_Risk_Scre = Bene_Agg_Sum_Risk_Scre / Tot_Benes,
         Bene_Avg_Age = Bene_Agg_Sum_Age / Tot_Benes,
         BDR = Brnd_Tot_Clms / Tot_Clms,
         GDR = Gnrc_Tot_Clms / Tot_Clms) %>% 
  filter(!is.na(division))
  
top_40_spec <-pst_by_type %>% 
  group_by(Prscrbr_Type) %>% 
  summarize(Sum_Clms_AllRegions = sum(Tot_Clms)) %>% 
  ungroup() %>% 
  top_n(n=40, Sum_Clms_AllRegions) %>% 
  select(Prscrbr_Type)

top_10_spec <-pst_by_type %>% 
  group_by(Prscrbr_Type) %>% 
  summarize(Sum_Clms_AllRegions = sum(Tot_Clms)) %>% 
  ungroup() %>% 
  top_n(n=10, Sum_Clms_AllRegions) %>% 
  select(Prscrbr_Type)

pst_by_spec_gg <- pst_by_type %>% 
  inner_join(top_40_spec) %>% 
  ggplot(mapping = aes(x = BDR,
                       y = reorder(Prscrbr_Type, BDR),
                       size = Tot_Benes,
                       color = division)) +
  guides(size = "none") +
  scale_color_viridis(discrete=T) +
  theme_bw() +
  geom_vline(xintercept = nationwide_avg_bdr, color="gray30") +
  scale_x_continuous(labels = scales::percent_format(accuracy = )) + 
  labs(title="Brand Drug Dispense Rate by Top 40 Provider Specialties",
       subtitle = "Medicare Part D Claims",
       x = "Brand Dispense Rate (BDR)",
       y = "Provider Specialty",
       color = "Census Division",
       caption = "Based on Part D Prescriber Public Use File (PUF) located at
                  https://www.hhs.gov/guidance/document/medicare-provider-utilization-and-payment-data-part-d-prescriber-0") + 
  geom_point(alpha = 0.5)

pst_by_spec_gg

ruca_simplified_cw <- pst_by_zip5 %>% 
  filter(!is.na(Prscrbr_RUCA_Desc) & str_trim(Prscrbr_RUCA_Desc) != "Unknown") %>% 
  distinct(Prscrbr_RUCA, Prscrbr_RUCA_Desc) %>% 
  mutate(Adj_RUCA = as.integer(Prscrbr_RUCA)) %>% 
  group_by(Adj_RUCA) %>% 
  arrange(Adj_RUCA) %>% 
  mutate(Adj_RUCA_Desc = first(Prscrbr_RUCA_Desc)) %>% 
  ungroup() %>% 
  mutate(RUCA_Desc_Short = map_chr(Adj_RUCA_Desc, function(x) str_split(x, pattern=":", simplify=T)[1])) %>% 
  distinct(Prscrbr_RUCA, RUCA_Desc_Short) %>% 
  arrange(Prscrbr_RUCA) %>% 
  mutate(RUCA_Desc_Short_Factor = fct_inorder(RUCA_Desc_Short)) %>% 
  select(-RUCA_Desc_Short)

## By Provider location RUCA and Type
pst_by_type_ruca <- pst_by_zip5 %>%
  inner_join(states, by=c("Prscrbr_State_Abrvtn"="state")) %>% 
  inner_join(ruca_simplified_cw) %>% 
  group_by(Prscrbr_Type, RUCA_Desc_Short_Factor) %>% 
  summarize(Tot_Benes = sum(Tot_Benes),
            Bene_Agg_Sum_Risk_Scre = sum(Bene_Agg_Sum_Risk_Scre),
            Bene_Agg_Sum_Age = sum(Bene_Agg_Sum_Age),
            Brnd_Tot_Clms = sum(Brnd_Tot_Clms),
            Gnrc_Tot_Clms = sum(Gnrc_Tot_Clms),
            Tot_Clms = sum(Tot_Clms)) %>% 
  ungroup() %>% 
  mutate(Bene_Avg_Risk_Scre = Bene_Agg_Sum_Risk_Scre / Tot_Benes,
         Bene_Avg_Age = Bene_Agg_Sum_Age / Tot_Benes,
         BDR = Brnd_Tot_Clms / Tot_Clms,
         GDR = Gnrc_Tot_Clms / Tot_Clms)

pst_by_type_ruca_gg <- pst_by_type_ruca %>% 
  inner_join(top_10_spec) %>% 
  ggplot(mapping = aes(x = BDR,
                       y = reorder(Prscrbr_Type, BDR),
                       size = Tot_Benes)) +
  guides(size = "none") +
  scale_color_viridis(discrete=T) +
  theme_bw() +
  geom_vline(xintercept = nationwide_avg_bdr, color="gray30") +
  scale_x_continuous(labels = scales::percent_format(accuracy = )) + 
  labs(title="Brand Drug Dispense Rate by Top 10 Provider Specialties",
       subtitle = "Medicare Part D Claims, Faceted by RUCA (Rural/Urban Commuting Area) Codes",
       x = "Brand Dispense Rate (BDR)",
       y = "Provider Specialty",
       caption = "Based on Part D Prescriber Public Use File (PUF) located at
                  https://www.hhs.gov/guidance/document/medicare-provider-utilization-and-payment-data-part-d-prescriber-0") + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ RUCA_Desc_Short_Factor, ncol = 3)

pst_by_type_ruca_gg

## Now add two points - one for states with and without Med Exp or Gen Subst to answer 

## By Provider location RUCA and Type
pst_by_type_ruca_medexp <- pst_by_zip5 %>%
  inner_join(states, by=c("Prscrbr_State_Abrvtn"="state")) %>% 
  inner_join(ruca_simplified_cw) %>% 
  left_join(med_exp_states) %>% 
  left_join(gen_subst_states) %>% 
  mutate(Medicaid_Expansion = if_else(!is.na(Bool_Expansion), "Adopted before 1/1/2016", "Not adopted before 1/1/2016")) %>% 
  filter(!is.na(region)) %>% 
  mutate(Generic_Substitution_Law = if_else(!is.na(Bool_GenSubst), "Legally mandated", "Not legally mandated")) %>% 
  group_by(Prscrbr_Type, RUCA_Desc_Short_Factor, Medicaid_Expansion, Generic_Substitution_Law) %>% 
  summarize(Tot_Benes = sum(Tot_Benes),
            Bene_Agg_Sum_Risk_Scre = sum(Bene_Agg_Sum_Risk_Scre),
            Bene_Agg_Sum_Age = sum(Bene_Agg_Sum_Age),
            Brnd_Tot_Clms = sum(Brnd_Tot_Clms),
            Gnrc_Tot_Clms = sum(Gnrc_Tot_Clms),
            Tot_Clms = sum(Tot_Clms)) %>% 
  ungroup() %>% 
  mutate(Bene_Avg_Risk_Scre = Bene_Agg_Sum_Risk_Scre / Tot_Benes,
         Bene_Avg_Age = Bene_Agg_Sum_Age / Tot_Benes,
         BDR = Brnd_Tot_Clms / Tot_Clms,
         GDR = Gnrc_Tot_Clms / Tot_Clms)



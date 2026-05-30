
library(tidyr); library(dplyr); library(readxl)
# https://datacatalog.urban.org/dataset/changing-medical-debt-landscape-united-states
file_name <- 'changing_med_debt_landscape_state.xlsx'
link <- 'https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2024/07/02/changing_med_debt_landscape_state.xlsx'
if (!file.exists(file_name))  download.file(url = link, destfile = file_name, mode = "wb")
debt <- read_excel(file_name)
table(debt$Year); colnames(debt)
colnames(debt) = c('year', 'fips', 'state', 'share', 'median',
                   'share_white', 'median_white', 'share_nw', 'median_nw',
                   'hospital', 'closure', 'uninsured', 'elderly', 'income'); head(debt)
debt = debt %>% mutate(across(-state, ~ as.numeric(as.character(.)))) %>% 
  mutate(state_name = state, state = fips) %>% select(-fips)
# https://cpr.uky.edu/resources/national-welfare-data
file_name <- 'ukcpr_national_welfare_data_1980_2024_jan26update.xlsx'
link <- 'https://ukcpr.uky.edu/sites/default/files/2026-02/ukcpr_national_welfare_data_1980_2024_jan26update.xlsx'
if (!file.exists(file_name))  download.file(url = link, destfile = file_name, mode = "wb")
state <- read_excel(file_name, sheet = 2)
colnames(state)
state = state %>% select(-state) %>% 
  rename('medicaid' = "Medicaid beneficiaries",  #'fips' = 'state_fips',
         'state_unemp' = "Unemployment rate", 'state_income' = "Personal income",
         'state_poverty' = "Poverty Rate", 'pop' = 'Population', 'gdp' = "Gross State Product",
         'state_governor' ="Governor is Democrat (1=Yes)") %>% 
  select(state_name, year, pop, medicaid, contains('state_')) %>%
  filter(year %in% 2011:2023) %>% mutate(state_governor = as.numeric(state_governor),
                                         log_income = log(state_income), log_pop  = log(pop)); head(state)
# pop 
# https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-detail.html
# https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2010-2020-state-detail.html
# 1. Define filenames and source links
file1 <- 'sc-est2020int-alldata6.csv'
link1 <- 'https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/intercensal/state/asrh/sc-est2020int-alldata6.csv'
file2 <- 'sc-est2024-alldata6.csv'
link2 <- 'https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/state/asrh/sc-est2024-alldata6.csv'
if (!file.exists(file1)) download.file(url = link1, destfile = file1, mode = "wb")
if (!file.exists(file2))download.file(url = link2, destfile = file2, mode = "wb")
pop1 <- read.csv(file1); head(pop1)
pop2 <- read.csv(file2); head(pop2)
(mk = intersect(colnames(pop1), colnames(pop2)))
pop = pop1 %>% left_join(pop2, by = mk)
pop = pop %>% rename_with(tolower) %>% mutate(fips = state) %>% 
  select(-sumlev, -region, -division, -name, -estimatesbase2010,  -estimatesbase2020, -census2020pop); head(pop)
pop = pop %>% pivot_longer(cols = starts_with("popestimate"), names_to = "year", values_to = "pop") %>% 
  mutate(year = gsub('popestimate', '', year), year = as.numeric(year)); head(pop)
total_pop = pop %>% filter(sex == 0 & origin==0) %>% 
  group_by(state, year) %>% summarise(total_pop = sum(pop)) %>% ungroup()
aggregate(total_pop ~ year, total_pop, sum); total_pop %>% filter(state==5)
treated = pop %>% filter(age >= 30 & age < 50 & sex == 0 & origin==0) %>%
  group_by(state, year) %>% summarise(pop = sum(pop)) %>% ungroup() %>% rename('age3049' = 'pop'); aggregate(age3049 ~ year, treated, sum)
young = pop %>% filter(age<=18 & sex == 0 & origin==0) %>% group_by(state, year) %>%
  summarise(pop = sum(pop)) %>% ungroup() %>% rename('young' = 'pop')
old = pop %>% filter(age>=65 & sex == 0 & origin==0) %>% group_by(state, year) %>%
  summarise(pop = sum(pop)) %>% ungroup() %>% rename('old' = 'pop')
white = pop %>% filter(sex == 0 & origin==1 & race == 1) %>% group_by(state, year) %>%
  summarise(pop = sum(pop)) %>% ungroup() %>% rename('white' = 'pop')
black = pop %>% filter(sex == 0 & origin==1 & race == 2) %>% group_by(state, year) %>%
  summarise(pop = sum(pop)) %>% ungroup() %>% rename('black' = 'pop')
hispanic = pop %>% filter(sex == 0 & origin==2) %>% group_by(state, year) %>%
  summarise(pop = sum(pop)) %>% ungroup() %>% rename('hispanic' = 'pop')
POP = total_pop %>% 
  left_join(treated, by = c('state', 'year')) %>% 
  left_join(young, by = c('state', 'year')) %>% 
  left_join(old, by = c('state', 'year')) %>% 
  left_join(white, by = c('state', 'year')) %>% 
  left_join(black, by = c('state', 'year')) %>% 
  left_join(hispanic, by = c('state', 'year')); head(POP)
POP = POP %>% mutate(white = white/total_pop, black = black/total_pop, hispanic = hispanic/total_pop,
                     pop_share = age3049/total_pop, young = young/total_pop, old = old/total_pop); head(POP)
subset(POP, state==5)
data = debt %>% left_join(state, by = c('state_name', 'year')) %>%
  left_join(POP, by = c('state', 'year')) # %>%  left_join(cc, by = c('state_name', 'year'))
save(data, file = 'data.RDa'); summary(data)
############################################
library(ggplot2)
load('data.RDa'); head(data)
file_name <- 'changing_med_debt_landscape_national.xlsx'
link <- 'https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2024/07/02/changing_med_debt_landscape_national.xlsx'
if (!file.exists(file_name)) download.file(url = link, destfile = file_name, mode = "wb")
us <- read_excel(file_name); head(us)
colnames(us) = colnames(debt) = c('year', 'state', 'share', 'median',
                                  'share_white', 'median_white', 'share_nw', 'median_nw',
                                  'hospital', 'closure', 'uninsured', 'elderly', 'income'); head(us)
us = us %>% mutate(across(-state, ~ as.numeric(as.character(.)))) %>% filter(year<2023)
(ar = data %>% filter(state_name=='AR' & year < 2023) %>% mutate(medicaid = medicaid/pop) %>% 
    select(year, share, median, medicaid))
ggplot() + 
  geom_rect(aes(xmin = 2018, xmax = 2019, ymin = -Inf, ymax = Inf), 
            fill = "grey90", alpha = 0.5) +
  geom_text(aes(x = 2018.7, y = max(ar$share, na.rm = TRUE) * 0.95, 
                label = "Medicaid Work Requirement\nPeriod (June 2018 - March 2019)"),
            color = "black", size = 3.5, hjust = -0.1) +
  geom_line(data = ar, aes(x = year, y = share), size = 1.1, color = 'black') + 
  geom_line(data = us, aes(x = year, y = share), size = 1.1, color = 'darkgrey') +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "grey50") + 
  geom_vline(xintercept = 2019, linetype = "dashed", color = "grey50") +
  geom_text(data = subset(ar, year == max(year)), 
            aes(x = year, y = share, label = "Arkansas"), 
            hjust = -0.15, vjust = 0.5, color = "black") +
  geom_text(data = subset(us, year == max(year)), 
            aes(x = year, y = share, label = "US Average"), 
            hjust = -0.15, vjust = 0.5, color = "black") +
  scale_x_continuous(breaks = min(ar$year):max(ar$year),
                     labels = min(ar$year):max(ar$year),
                     limits = c(min(ar$year), max(ar$year) + 2)) + 
  labs(x = "Year", y = "Population share with medical debt in collections") +
  theme_classic(); ggsave('plot_debt_share.png', height = 4, width = 7)
ggplot() + 
  geom_rect(aes(xmin = 2018, xmax = 2019, ymin = -Inf, ymax = Inf), 
            fill = "grey90", alpha = 0.5) +
  geom_text(aes(x = 2018, y = max(ar$median, na.rm = TRUE) * 0.95, 
                label = "Medicaid Work Requirement\nPeriod (June 2018 - March 2019)"),
            color = "black", size = 3.5, hjust = 1.1) +
  geom_line(data = ar, aes(x = year, y = median), size = 1.1, color = 'black') + 
  geom_line(data = us, aes(x = year, y = median), size = 1.1, color = 'darkgrey') +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "grey50") + 
  geom_vline(xintercept = 2019, linetype = "dashed", color = "grey50") +
  geom_text(data = subset(ar, year == max(year)), 
            aes(x = year, y = median, label = "Arkansas"), 
            hjust = -0.15, vjust = 0.5, color = "black") +
  geom_text(data = subset(us, year == max(year)), 
            aes(x = year, y = median, label = "US Average"), 
            hjust = -0.15, vjust = 0.5, color = "black") +
  scale_x_continuous(breaks = min(ar$year):max(ar$year),
                     labels = min(ar$year):max(ar$year),
                     limits = c(min(ar$year), max(ar$year) + 2)) + 
  labs(x = "Year", y = "Median medical debt in collections (in $2023)") +
  theme_classic(); ggsave('plot_debt_median.png', height = 4, width = 7)
#
(usm = data %>% filter(year<2023) %>% group_by(year) %>% #%>% filter(state_name!='AR') 
  summarize(medicaid = sum(medicaid), pop = sum(pop), medicaid = medicaid/pop) %>% 
  ungroup())
ggplot() + 
  geom_rect(aes(xmin = 2018, xmax = 2019, ymin = -Inf, ymax = Inf), 
            fill = "grey90", alpha = 0.5) +
  geom_text(aes(x = 2018, y = max(ar$medicaid, na.rm = TRUE) * 0.95, 
                label = "Medicaid Work Requirement\nPeriod (June 2018 - March 2019)"),
            color = "black", size = 3.5, hjust = 1.1) +
  geom_line(data = ar, aes(x = year, y = medicaid), size = 1.1, color = 'black') + 
  geom_line(data = usm, aes(x = year, y = medicaid), size = 1.1, color = 'darkgrey') +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "grey50") + 
  geom_vline(xintercept = 2019, linetype = "dashed", color = "grey50") +
  geom_text(data = subset(ar, year == max(year)), 
            aes(x = year, y = medicaid, label = "Arkansas"), 
            hjust = -0.15, vjust = 0.5, color = "black") +
  geom_text(data = subset(usm, year == max(year)), 
            aes(x = year, y = medicaid, label = "US Average"), 
            hjust = -0.15, vjust = 0.5, color = "black") +
  scale_x_continuous(breaks = min(ar$year):max(ar$year),
                     labels = min(ar$year):max(ar$year),
                     limits = c(min(ar$year), max(ar$year) + 2)) + 
  labs(x = "Year", y = "Medicaid population share") +
  theme_classic(); ggsave('plot_medicaid.png', height = 4, width = 7)

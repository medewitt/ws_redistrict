# Purpose: Get Results



# library -----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)


# import 2016 data --------------------------------------------------------


my_file <- fs::dir_ls(here::here("data"), regex = "20161108")[[2]]

dat_2016 <- read_csv( my_file)

dat_2018 <- read_tsv(here::here("data", "ncvhis34.txt"))

proposed_wards <- tribble(
  ~"new_ward", ~"pct_label", ~"current_ward",
  "Ward 1", "032", "",
  "Ward 1", "033", "",
  "Ward 1", "092", "",
  "Ward 1", "101", "",
  "Ward 1", "131", "",
  "Ward 1", "132", "",
  "Ward 1", "201", "",
  "Ward 1", "203", "",
  "Ward 1", "204", "",
  "Ward 1", "205", "",
  "Ward 1", "206", "",
  "Ward 1", "207", "",
  "Ward 1", "801", "",
  "Ward 1", "802", "",
  "Ward 1", "903", "",
  "Ward 1", "904", "",
  "Ward 1", "905", "",
  "Ward 1", "906", "",
  "Ward 1", "907", "",
  "Ward 1", "908", "",
  "Ward 1", "909", "",
  
  "Ward 2", "031", "",
  "Ward 2", "032", "",
  "Ward 2", "033", "",
  "Ward 2", "034", "",
  "Ward 2", "081", "",
  "Ward 2", "082", "",
  "Ward 2", "083", "",
  "Ward 2", "111", "",
  "Ward 2", "201", "",
  "Ward 2", "203", "",
  "Ward 2", "204", "",
  "Ward 2", "206", "",
  "Ward 2", "207", "",
  "Ward 2", "301", "",
  "Ward 2", "302", "",
  "Ward 2", "303", "",
  "Ward 2", "304", "",
  "Ward 2", "305", "",
  "Ward 2", "306", "",
  "Ward 2", "401", "",
  "Ward 2", "402", "",
  "Ward 2", "403", "",
  "Ward 2", "404", "",
  
  "Ward 3", "16", "",
  "Ward 3", "011", "",
  "Ward 3", "012", "",
  "Ward 3", "013", "",
  "Ward 3", "015", "",
  "Ward 3", "042", "",
  "Ward 3", "043", "",
  "Ward 3", "063", "",
  "Ward 3", "082", "",
  "Ward 3", "401", "",
  "Ward 3", "405", "",
  "Ward 3", "501", "",
  "Ward 3", "502", "",
  "Ward 3", "503", "",
  "Ward 3", "504", "",
  "Ward 3", "505", "",
  "Ward 3", "507", "",
  
  "Ward 4", "042", "",
  "Ward 4", "122", "",
  "Ward 4", "506", "",
  "Ward 4", "601", "",
  "Ward 4", "602", "",
  "Ward 4", "603", "",
  "Ward 4", "604", "",
  "Ward 4", "605", "",
  "Ward 4", "606", "",
  "Ward 4", "607", "",
  "Ward 4", "702", "",
  "Ward 4", "703", "",
  "Ward 4", "704", "",
  "Ward 4", "705", "",
  "Ward 4", "709", "",
  
  "Ward 5", "071", "",
  "Ward 5", "072", "",
  "Ward 5", "074", "",
  "Ward 5", "123", "",
  "Ward 5", "131", "",
  "Ward 5", "133", "",
  "Ward 5", "701", "",
  "Ward 5", "705", "",
  "Ward 5", "706", "",
  "Ward 5", "707", "",
  "Ward 5", "708", "",
  "Ward 5", "803", "",
  "Ward 5", "804", "",
  "Ward 5", "805", "",
  "Ward 5", "806", "",
  "Ward 5", "807", "",
  "Ward 5", "808", "",
  "Ward 5", "809", "",
  "Ward 5", "901", "",
  "Ward 5", "902", "",
)

combined <- proposed_wards %>% 
  left_join(dat_2018) %>% 
  filter(grepl(pattern = "GENERAL", election_desc)) %>% 
  mutate(my_date = lubridate::mdy(election_lbl),
         election = lubridate::year(my_date))

voter_history <- combined %>% 
  group_by(election, new_ward, voted_party_desc) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n/sum(n)) %>% 
  ungroup() %>% 
  group_by(new_ward, voted_party_desc) %>% 
  summarise(avg_vote_share = mean(perc))

my_files <- list.files(here("data", "results"), full.names = T)


results_2010 <- read_csv(my_files[[1]])%>% 
  janitor::clean_names() %>% 
  rename(contest_name = contest, choice_party = party) %>% 
  select(county, precinct, contest_name, contest_type, 
         total_votes, choice, choice_party)
results_2011 <- read_csv(my_files[[2]])%>% 
  janitor::clean_names() %>% 
  rename(contest_name = contest, choice_party = party) %>% 
  select(county, precinct, contest_name, contest_type, 
         total_votes, choice, choice_party)
results_2012 <- read_csv(my_files[[3]])%>% 
  janitor::clean_names() %>% 
  rename(contest_name = contest, choice_party = party) %>% 
  select(county, precinct, contest_name, contest_type, 
         total_votes, choice, choice_party)

results_2014 <- read_tsv(my_files[[4]])%>% janitor::clean_names()
results_2015 <- read_tsv(my_files[[5]])%>% janitor::clean_names()
results_2016 <- read_tsv(my_files[[6]])%>% janitor::clean_names()
results_2017 <- read_tsv(my_files[[7]])%>% janitor::clean_names()
results_2018 <- read_tsv(my_files[[8]])%>% janitor::clean_names()

all_results <-bind_rows(results_2010, results_2011, results_2012, results_2014,
          results_2015, results_2016, results_2017, results_2018)

forsyth_results <- all_results %>% 
  filter(grepl("FORSYTH",county)) %>% 
  mutate(precinct = str_extract(precinct, "\\d{2,}")) %>% 
  left_join(proposed_wards, by = c("precinct" = "pct_label")) %>% 
  mutate(my_date = lubridate::mdy(election_date),
         election = lubridate::year(my_date))


city_consel_elections <- forsyth_results %>% 
  filter(grepl("WARD", contest_name)) %>% 
  filter(!grepl("HIGH POINT", contest_name))
  
city_consel_elections %>% 
  group_by(election, contest_name, choice_party) %>% 
  summarise(total_vote = sum(total_votes, na.rm = T)) %>% 
  spread(choice_party, total_vote, 0) %>% 
  mutate(DEM_vote_share = DEM / (DEM + REP + `<NA>`))

city_consel_elections %>% 
  group_by(choice_party) %>% 
  summarise(total = sum(total_votes))

city_consel_elections %>% 
  group_by(election, new_ward, choice_party) %>% 
  summarise(total_vote = sum(total_votes)) %>% 
  spread(choice_party, total_vote) %>% 
  mutate(DEM_vote_share = DEM/sum(DEM+REP+`<NA>`))

(out <-forsyth_results %>% 
  filter(!is.na(new_ward)) %>% 
  filter(grepl(x = contest_name, "PRESIDENT")) %>% 
  mutate(choice_party = ifelse(choice_party %in% c("DEM", "REP"), choice_party,
                               "other")) %>% 
  group_by(election, new_ward, choice_party) %>% 
  summarise(total = sum(total_votes)) %>% 
  ungroup() %>% 
  spread(choice_party, total) %>% 
  mutate(election = ifelse(is.na(election), 2012, election)) %>% 
  mutate(DEM_share = DEM/ (DEM+other+REP)) %>% 
  mutate(winning = (DEM+other+REP)/2+1) %>% 
  rowwise() %>% 
  mutate(DEM_wasted_vote = ifelse(DEM - winning<0,DEM,DEM - winning),
         REP_wasted_vote = ifelse(REP - winning<0,REP,REP - winning)) %>% 
    ungroup())

out %>% 
  summarise(total_voter= sum(REP+other +DEM),
            wasted_dem = sum(DEM_wasted_vote),
            wasted_rep = sum(REP_wasted_vote))->efficiency

(efficiency$wasted_rep-efficiency$wasted_dem)/efficiency$total_voter


# ccurrent efficiency gap -------------------------------------------------

city_consel_elections %>% 
  mutate(choice_party = ifelse(choice_party %in% c("DEM", "REP"), choice_party,
                               "other")) %>% 
  group_by(election, contest_name, choice_party) %>% 
  summarise(total_vote = sum(total_votes, na.rm = T)) %>% 
  spread(choice_party, total_vote, 0) %>% 
  mutate(winning = (DEM+other+REP)/2+1) %>% 
  rowwise() %>% 
  mutate(DEM_wasted_vote = ifelse(DEM - winning<0,DEM,DEM - winning),
         REP_wasted_vote = ifelse(REP - winning<0,REP,REP - winning)) %>% 
  ungroup()-> current_eff

current_eff <- current_eff %>% 
  summarise(total_voter= sum(REP+other +DEM),
            wasted_dem = sum(DEM_wasted_vote),
            wasted_rep = sum(REP_wasted_vote))

(current_eff$wasted_rep-current_eff$wasted_dem)/current_eff$total_voter

# next --------------------------------------------------------------------


forsyth_results %>% 
  filter(!is.na(new_ward)) %>% 
  filter(grepl(x = contest_name, "PRESIDENT")) %>% 
  mutate(choice_party = ifelse(choice_party %in% c("DEM", "REP"), choice_party,
                               "other")) %>% 
  group_by(election, choice_party) %>% 
  summarise(total = sum(total_votes)) %>% 
  ungroup() %>% 
  spread(choice_party, total) %>% 
  mutate(election = ifelse(is.na(election), 2012, election)) %>% 
  mutate(DEM_share = DEM/ (DEM+other+REP))

current_eff %>% 
summarise(total_voter= sum(REP+other +DEM),
          wasted_dem = sum(DEM_wasted_vote),
          wasted_rep = sum(REP_wasted_vote))

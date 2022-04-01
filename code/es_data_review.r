# ==========================================================================
# Identify the number of evictions that we have across different data sources
# Tim Thomas - timthomas@berkeley.edu
# 2022.03.27
# ==========================================================================

# ==========================================================================
# Packages and options
# ==========================================================================

pacman::p_load(qs, data.table, tidyverse, sf, googledrive, lubridate, bit64)
options(scipen=10, width=system("tput cols", intern=TRUE), tigris_use_cache = TRUE) # avoid scientific notation

# ==========================================================================
# pull in data
# ==========================================================================

#
# Washington State
# --------------------------------------------------------------------------

wa_df <- fread("/Volumes/GoogleDrive/My Drive/data/evictions/evictions_study/results/evictions_race_sex_geo_2020-01-13.csv.bz2")

wa_df_def <-
	wa_df %>%
	filter(grepl("^DEF", Participant)) %>%
	select(MainID, County, Year, Key, CaseNumber = Number, Name, FirstName, LastName, Company, PCount) %>%
	glimpse()

glimpse(wa_df_def)

wa_kc_cn <- wa_df_def %>% filter(County == "King") %>% pull(CaseNumber)

king <-
	fread("/Volumes/GoogleDrive/Shared drives/udpdata/projects/evictions-study/washington/king/King_2022/king_co_unlawful_detainers_2016_Jan_2022_021422.csv") %>%
	mutate(County = "King", Year = year(FileDate)) %>%
	filter(partyType == "DEF") %>%
	select(County, Year, CaseNumber, Name = Party) %>%
	filter(!CaseNumber %in% wa_kc_cn) %>%
	glimpse()

# ==========================================================================
# Clean for demographic estimation
# ==========================================================================
pattern <-
  c(' [A-Z]. | [A-Z] |JR.|JR|.SR|SR|DOE.*|,$|, $| \"[^>]+\" |\\s*\\([^\\)]+\\)|\\s\\s| AKA.*| AND.*| III| II| I|ESTATE|DEL |OCCUPANT|ABU')
corp <- c('CAFE|LAB|SECURITY|SERVICE|PHARMACY|COMMERCIAL|LLC|PRINTING|VENTURE|CARRY|CARRYOUT| INC|FOOD|GROUP|SALOON|COMMUNITY|KINGDOM|COVER ME|TATTOO|ROOM|BOAT|TACKLE')

king_cn <-
  king %>%
  distinct() %>%
  mutate(
    clean_name = toupper(Name),
    clean_name = str_replace_all(clean_name, 'ET AL', ' '),
    clean_name = str_replace_all(clean_name, pattern, ' '),
    clean_name = str_replace_all(clean_name, c('\\.|\\;'), ','),
    clean_name = str_replace_all(clean_name, pattern, ' '),
    clean_name = str_trim(clean_name),
    clean_name = case_when(clean_name != '' ~ clean_name),
    Company = case_when(grepl(corp, clean_name) == TRUE ~ TRUE, TRUE ~ FALSE),
    space = str_count(clean_name, ' '),
    FirstName = word(clean_name, 1),
    LastName =
      case_when(
        space <= 2 ~ word(clean_name, -1),
        space > 2 ~ word(clean_name, 2)
        )
  ) %>%
  select(-space, -clean_name) %>%
  group_by(CaseNumber) %>%
  mutate(PCount = row_number())

glimpse(king_cn)


#
# Addresses
# --------------------------------------------------------------------------

add <-
	fread("/Volumes/GoogleDrive/My Drive/data/evictions/evictions_study/results/all_addresses_2019-12-06.csv") %>%
	select(MainID = key, Address = StAddr, City, ZipCode = Postal)

wa_df_def_add <- left_join(wa_df_def, add)

kc_add <-
	readxl::read_xlsx("/Volumes/GoogleDrive/Shared drives/udpdata/projects/evictions-study/washington/king/King_2022/KC Case numbers and addresses.xlsx") %>%
	rename(CaseNumber = `Case Number`, ZipCode = `Zip Code`)

king_add <- left_join(king_cn, kc_add)

final_wa <-
	bind_rows(wa_df_def_add, king_add) %>%
	distinct()

glimpse(final_wa)
tail(final_wa)

#
# report
# --------------------------------------------------------------------------

r_wa <-
	final_wa %>%
	group_by(Year, County) %>%
	summarize(cases = n(), addresses = sum(!is.na(ZipCode)))

cases <-
	r_wa %>%
	select(-addresses) %>%
	ungroup() %>%
	spread(Year,cases)

addresses <-
	r_wa %>%
	select(-cases) %>%
	ungroup() %>%
	spread(Year,addresses)

fwrite(final_wa, "~/Downloads/wa_evictions_2004_2022.csv")
fwrite(cases, "~/Downloads/wa_cases_2004_2022.csv")
fwrite(addresses, "~/Downloads/wa_addresses_2004_2022.csv")

kc_2016 <-
	left_join(king, kc_add_cn)

kc_2016 group_by(year)

#
# Illinois
# --------------------------------------------------------------------------
cook <- readRDS('/Volumes/GoogleDrive/Shared drives/udpdata/projects/evictions-study/illinois/chicago_df.rds')

cook <- fread('/Volumes/GoogleDrive/Shared drives/udpdata/projects/evictions-study/illinois/chicago_race_sex_estimation.csv')

cook_18 <-
	fread('/Volumes/GoogleDrive/Shared drives/udpdata/raw/evictions/illinois/cook/Chicago_2010_2018_final.csv') %>%
	mutate(Year = year(File_Date), CaseNumber = CaseNum, Street = FullStreet, City = DefCity, ZipCode = DefZipCode, Name = DefFirm) %>%
	select(Year:Name) %>% distinct()

cook_21 <- fread('/Volumes/GoogleDrive/Shared drives/udpdata/raw/evictions/illinois/cook/Chicago_2019_2021_final.csv') %>%
	mutate(Year = year(File_Date), CaseNumber = CaseNum, Street = FullStreet, City = DefCity, ZipCode = DefZipCode, Name = DefFirm) %>%
	select(Year:Name) %>% distinct()

cook_df <- bind_rows(cook_18, cook_21) %>%
distinct() %>%
  mutate(
    clean_name = toupper(Name),
    clean_name = str_replace_all(clean_name, 'ET AL', ' '),
    clean_name = str_replace_all(clean_name, pattern, ' '),
    clean_name = str_replace_all(clean_name, c('\\.|\\;'), ','),
    clean_name = str_replace_all(clean_name, pattern, ' '),
    clean_name = str_replace_all(clean_name, pattern, ' '),
    clean_name = str_trim(clean_name),
    clean_name = case_when(clean_name != '' ~ clean_name),
    Company = case_when(grepl(corp, clean_name) == TRUE ~ TRUE, TRUE ~ FALSE),
    space = str_count(clean_name, ' '),
    FirstName = word(clean_name, 1),
    LastName =
      case_when(
        space <= 2 ~ word(clean_name, -1),
        space > 2 ~ word(clean_name, 2)
        )
  ) %>%
  select(-space, -clean_name) %>%
  group_by(CaseNumber) %>%
  mutate(PCount = row_number())

r_cook <-
	cook_df %>%
	group_by(Year) %>%
	summarize(cases = n(), addresses = sum(!is.na(ZipCode))) %>%
	mutate(County = "Cook")

fwrite(cook_df, '~/Downloads/il_evictions_2010_2021.csv')
fwrite(r_cook, '~/Downloads/il_cases_2004_2021.csv')

#
# California
# --------------------------------------------------------------------------
  df_geo_sex_race <-
  	fread('~/data/eviction/california/bay_ev_clean_complete_2017-2021.csv') %>%
  	mutate(month = month(Date), Year = year(Date))

r_ca <-
	df_geo_sex_race %>%
	group_by(Year, County) %>%
	summarize(cases = n(), addresses = sum(!is.na(addr1)))

cases <-
	r_ca %>%
	select(-addresses) %>%
	ungroup() %>%
	spread(Year,cases)

addresses <-
	r_ca %>%
	select(-cases) %>%
	ungroup() %>%
	spread(Year,addresses)

fwrite(df_geo_sex_race, "~/Downloads/ca_evictions_2017_2021.csv")
fwrite(cases, "~/Downloads/ca_cases_2017_2021.csv")
fwrite(addresses, "~/Downloads/ca_addresses_2017_2021.csv")


#
# Baltimore
# --------------------------------------------------------------------------


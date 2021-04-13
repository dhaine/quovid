## Load data
## cases
inspq <- read.csv("https://msss.gouv.qc.ca/professionnels/statistiques/documents/covid19/COVID19_Qc_RapportINSPQ_HistoVigie.csv")
inspq <- inspq %>%
    filter(Date != "Date inconnue") %>%
    mutate(Date = lubridate::date(Date),
           rolling_avg = zoo::rollmean(Nb_Nvx_Cas, k = 7, fill = NA, align = "right")) %>%
    filter(Date > ymd("2020-02-24"))

#res <- httr::GET("https://www.inspq.qc.ca/sites/default/files/covid/donnees/covid19-hist.csv?randNum=26969598",
#                 add_headers("X-Requested-With" = "XMLHttpRequest"))
#res_parsed <- content(res, "parsed")

#res2 <- httr::GET("https://www.inspq.qc.ca/sites/default/files/covid/donnees/regions.csv?randNum=26969598",
#                 add_headers("X-Requested-With" = "XMLHttpRequest"))
#res2_parsed <- content(res2, "parsed")

#res3 <- httr::GET("https://www.inspq.qc.ca/sites/default/files/covid/donnees/manual-data.csv?randNum=26969598",
#                 add_headers("X-Requested-With" = "XMLHttpRequest"))
#res3_parsed <- content(res3, "parsed")

#res4 <- httr::GET("https://www.inspq.qc.ca/sites/default/files/covid/donnees/comorbidite.csv?randNum=26969598",
#                 add_headers("X-Requested-With" = "XMLHttpRequest"))
#res4_parsed <- content(res4, "parsed")

#res5 <- httr::GET("https://www.inspq.qc.ca/sites/default/files/covid/donnees/tableau-rpa-new.csv?randNum=26969598",
#                 add_headers("X-Requested-With" = "XMLHttpRequest"))
#res5_parsed <- content(res5, "parsed")

cases <- read.csv("R/data/Graphique 1.2 - page_principal.csv")
colnames(cases) <- c("Date", "epi_link", "lab_link", "moving_avg")
cases <- cases %>% mutate(Date = lubridate::date(Date),
                          Nb_Nvx_Cas = epi_link + lab_link) %>%
    select(Date, Nb_Nvx_Cas) %>%
    filter(Date > max(inspq$Date))
inspq <- dplyr::bind_rows(inspq, cases) %>%
    mutate(rolling_avg = zoo::rollmean(Nb_Nvx_Cas, k = 7, fill = NA, align = "right"))
inspq$cumsum <- cumsum(inspq$Nb_Nvx_Cas)
inspq$death_cumsum <- cumsum(inspq$Nb_Nvx_Deces_Total)

## cases opencovid
opencovid_api <- "https://api.opencovid.ca"
ts <- stringr::str_c(opencovid_api, "timeseries", sep = "/")
ts_cases <- stringr::str_c(ts, "?stat=cases")
ts_cases_abitibi <- stringr::str_c(ts_cases,
                                   ##"loc=QC",
                                   "loc=2408",
                                   "after=2020-02-25",
                                   "ymd=true",
                                   "missing=na",
                                   "version=true",
                                   sep = "&")
#ts_cases_abitibi
ts_abitibi_GET <- httr::GET(url = ts_cases_abitibi)
#ts_abitibi_GET
#httr::http_status(ts_abitibi_GET)  # check status of API request
##httr::content(ts_abitibi_GET, as = "text")  # double-check response
ts_abitibi <- jsonlite::fromJSON(ts_cases_abitibi)
#ts_abitibi %>% str()
abitibi <- tibble::as_tibble(ts_abitibi$cases)

ts_cases_basstlaur <- stringr::str_c(ts_cases,
                                     ##"loc=QC",
                                     "loc=2401",
                                     "after=2020-02-25",
                                     "ymd=true",
                                     "missing=na",
                                     "version=true",
                                     sep = "&")
#ts_cases_basstlaur
ts_basstlaur_GET <- httr::GET(url = ts_cases_basstlaur)
#ts_abitibi_GET
#httr::http_status(ts_abitibi_GET)  # check status of API request
##httr::content(ts_abitibi_GET, as = "text")  # double-check response
ts_basstlaur <- jsonlite::fromJSON(ts_cases_basstlaur)
#ts_basstlaur %>% str()
basstlaur <- tibble::as_tibble(ts_basstlaur$cases)

ts_cases_capnat <- stringr::str_c(ts_cases,
                                  ##"loc=QC",
                                  "loc=2403",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_capnat
ts_capnat_GET <- httr::GET(url = ts_cases_capnat)
#ts_capnat_GET
#httr::http_status(ts_capnat_GET)  # check status of API request
ts_capnat <- jsonlite::fromJSON(ts_cases_capnat)
#ts_capnat %>% str()
capnat <- tibble::as_tibble(ts_capnat$cases)

ts_cases_chaudapp <- stringr::str_c(ts_cases,
                                  "loc=2412",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_chaudapp
ts_chaudapp_GET <- httr::GET(url = ts_cases_chaudapp)
#ts_chaudapp_GET
#httr::http_status(ts_chaudapp_GET)  # check status of API request
ts_chaudapp <- jsonlite::fromJSON(ts_cases_chaudapp)
#ts_chaudapp %>% str()
chaudapp <- tibble::as_tibble(ts_chaudapp$cases)

ts_cases_cotenord <- stringr::str_c(ts_cases,
                                  "loc=2409",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_cotenord
ts_cotenord_GET <- httr::GET(url = ts_cases_cotenord)
#ts_cotenord_GET
#httr::http_status(ts_cotenord_GET)  # check status of API request
ts_cotenord <- jsonlite::fromJSON(ts_cases_cotenord)
#ts_cotenord %>% str()
cotenord <- tibble::as_tibble(ts_cotenord$cases)

ts_cases_estrie <- stringr::str_c(ts_cases,
                                  "loc=2405",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_estrie
ts_estrie_GET <- httr::GET(url = ts_cases_estrie)
#ts_estrie_GET
#httr::http_status(ts_estrie_GET)  # check status of API request
ts_estrie <- jsonlite::fromJSON(ts_cases_estrie)
#ts_estrie %>% str()
estrie <- tibble::as_tibble(ts_estrie$cases)

ts_cases_gasp <- stringr::str_c(ts_cases,
                                  "loc=2411",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_gasp
ts_gasp_GET <- httr::GET(url = ts_cases_gasp)
#ts_gasp_GET
#httr::http_status(ts_gasp_GET)  # check status of API request
ts_gasp <- jsonlite::fromJSON(ts_cases_gasp)
#ts_gasp %>% str()
gasp <- tibble::as_tibble(ts_gasp$cases)

ts_cases_lanaud <- stringr::str_c(ts_cases,
                                  "loc=2414",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_lanaud
ts_lanaud_GET <- httr::GET(url = ts_cases_lanaud)
#ts_lanaud_GET
#httr::http_status(ts_lanaud_GET)  # check status of API request
ts_lanaud <- jsonlite::fromJSON(ts_cases_lanaud)
#ts_lanaud %>% str()
lanaud <- tibble::as_tibble(ts_lanaud$cases)

ts_cases_laurent <- stringr::str_c(ts_cases,
                                  "loc=2415",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_laurent
ts_laurent_GET <- httr::GET(url = ts_cases_laurent)
#ts_laurent_GET
#httr::http_status(ts_laurent_GET)  # check status of API request
ts_laurent <- jsonlite::fromJSON(ts_cases_laurent)
#ts_laurent %>% str()
laurent <- tibble::as_tibble(ts_laurent$cases)

ts_cases_laval <- stringr::str_c(ts_cases,
                                  "loc=2413",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_laval
ts_laval_GET <- httr::GET(url = ts_cases_laval)
#ts_laval_GET
#httr::http_status(ts_laval_GET)  # check status of API request
ts_laval <- jsonlite::fromJSON(ts_cases_laval)
#ts_laval %>% str()
laval <- tibble::as_tibble(ts_laval$cases)

ts_cases_mauric <- stringr::str_c(ts_cases,
                                  "loc=2404",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_mauric
ts_mauric_GET <- httr::GET(url = ts_cases_mauric)
#ts_mauric_GET
#httr::http_status(ts_mauric_GET)  # check status of API request
ts_mauric <- jsonlite::fromJSON(ts_cases_mauric)
#ts_mauric %>% str()
mauric <- tibble::as_tibble(ts_mauric$cases)

ts_cases_monter <- stringr::str_c(ts_cases,
                                  "loc=2416",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_monter
ts_monter_GET <- httr::GET(url = ts_cases_monter)
#ts_monter_GET
#httr::http_status(ts_monter_GET)  # check status of API request
ts_monter <- jsonlite::fromJSON(ts_cases_monter)
#ts_monter %>% str()
monter <- tibble::as_tibble(ts_monter$cases)

ts_cases_mtl <- stringr::str_c(ts_cases,
                                  "loc=2406",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_mtl
ts_mtl_GET <- httr::GET(url = ts_cases_mtl)
#ts_mtl_GET
#httr::http_status(ts_mtl_GET)  # check status of API request
ts_mtl <- jsonlite::fromJSON(ts_cases_mtl)
#ts_mtl %>% str()
mtl <- tibble::as_tibble(ts_mtl$cases)

ts_cases_nord <- stringr::str_c(ts_cases,
                                  "loc=2410",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_nord
ts_nord_GET <- httr::GET(url = ts_cases_nord)
#ts_nord_GET
#httr::http_status(ts_nord_GET)  # check status of API request
ts_nord <- jsonlite::fromJSON(ts_cases_nord)
#ts_nord %>% str()
nord <- tibble::as_tibble(ts_nord$cases)

ts_cases_nunavik <- stringr::str_c(ts_cases,
                                  "loc=2417",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_nunavik
ts_nunavik_GET <- httr::GET(url = ts_cases_nunavik)
#ts_nunavik_GET
#httr::http_status(ts_nunavik_GET)  # check status of API request
ts_nunavik <- jsonlite::fromJSON(ts_cases_nunavik)
#ts_nunavik %>% str()
nunavik <- tibble::as_tibble(ts_nunavik$cases)

ts_cases_outaouais <- stringr::str_c(ts_cases,
                                  "loc=2407",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_outaouais
ts_outaouais_GET <- httr::GET(url = ts_cases_outaouais)
#ts_outaouais_GET
#httr::http_status(ts_outaouais_GET)  # check status of API request
ts_outaouais <- jsonlite::fromJSON(ts_cases_outaouais)
#ts_outaouais %>% str()
outaouais <- tibble::as_tibble(ts_outaouais$cases)

ts_cases_saguenay <- stringr::str_c(ts_cases,
                                  "loc=2402",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_saguenay
ts_saguenay_GET <- httr::GET(url = ts_cases_saguenay)
#ts_saguenay_GET
#httr::http_status(ts_saguenay_GET)  # check status of API request
ts_saguenay <- jsonlite::fromJSON(ts_cases_saguenay)
#ts_saguenay %>% str()
saguenay <- tibble::as_tibble(ts_saguenay$cases)

ts_cases_cri <- stringr::str_c(ts_cases,
                                  "loc=2418",
                                  "after=2020-02-25",
                                  "ymd=true",
                                  "missing=na",
                                  "version=true",
                                  sep = "&")
#ts_cases_cri
ts_cri_GET <- httr::GET(url = ts_cases_cri)
#ts_cri_GET
#httr::http_status(ts_cri_GET)  # check status of API request
ts_cri <- jsonlite::fromJSON(ts_cases_cri)
#ts_cri %>% str()
cri <- tibble::as_tibble(ts_cri$cases)

opencovid <- rbind(abitibi, basstlaur, capnat, chaudapp, cotenord,
                   estrie, gasp, lanaud, laurent, laval, mauric,
                   monter, mtl, nord, nunavik, outaouais, saguenay,
                   cri)

##--------------------------------------------------------------------
## load data incidence hospitalisation
## https://www.inspq.qc.ca/covid-19/donnees
hospi_i <- read.csv("R/data/Graphique 3.2 - page_principal.csv")
colnames(hospi_i) <- c("date", "hors_si", "si", "moving_inspq")
hospi_i <- hospi_i %>%
    mutate(date = lubridate::date(date),
           hospi = rowSums(hospi_i[, c(2, 3)], na.rm = TRUE),
           roll_hospi = zoo::rollmean(hospi, k = 7, fill = NA, align = "right"),
           roll_hsi = zoo::rollmean(hors_si, k = 7, fill = NA, align = "right"),
           roll_si = zoo::rollmean(si, k = 7, fill = NA, align = "right"))
## load data prévalence hospitalisation
hospi_p <- read.csv("R/data/Graphique 3.1 - page_principal.csv")
colnames(hospi_p) <- c("date", "hors_si", "si")
hospi_p <- hospi_p %>%
    mutate(date = lubridate::date(date),
           hospi = rowSums(hospi_p[, c(2:3)], na.rm = TRUE),
           roll_hospi = zoo::rollmean(hospi, k = 7, fill = NA, align = "right"),
           roll_hsi = zoo::rollmean(hors_si, k = 7, fill = NA, align = "right"),
           roll_si = zoo::rollmean(si, k = 7, fill = NA, align = "right"))
hospi_pl <- hospi_p %>%
    select(date, hors_si, si) %>%
    pivot_longer(!date, names_to = "si_ornot", values_to = "nbr") %>%
    mutate(si_ornot = case_when(si_ornot == "hors_si" ~ "Hospitalisations",
                                si_ornot == "si" ~ "Dont soins intensifs"))
hospi_pl$si_ornot <- factor(hospi_pl$si_ornot, levels = c("Hospitalisations",
                                                          "Dont soins intensifs"))
## load data âge
## https://www.inspq.qc.ca/covid-19/donnees/age-sexe
hospi_age <- read.csv("R/data/Graphique 3.7 - page age-sexe.csv")
colnames(hospi_age) <- c("date", "zero", "ten", "twenty", "thirty", "forty",
                         "fifty", "sixty", "seventy", "eighty", "ninety")
hospi_age <- hospi_age %>% mutate(date = lubridate::date(date))
hospi_al <- hospi_age %>%
    pivot_longer(!date, names_to = "age", values_to = "nbr") %>%
    mutate(age = case_when(age == "zero" ~ "0-9 ans",
                           age == "ten" ~ "10-19 ans",
                           age == "twenty" ~ "20-29 ans",
                           age == "thirty" ~ "30-39 ans",
                           age == "forty" ~ "40-49 ans",
                           age == "fifty" ~ "50-59 ans",
                           age == "sixty" ~ "60-69 ans",
                           age == "seventy" ~ "70-79 ans",
                           age == "eighty" ~ "80-89 ans",
                           age == "ninety" ~ ">= 90 ans"),
           si_ornot = "Hospitalisations")
hospi_siage <- read.csv("R/data/Graphique 3.9 - page age-sexe.csv")
colnames(hospi_siage) <- c("date", "zero", "ten", "twenty", "thirty", "forty",
                           "fifty", "sixty", "seventy", "eighty", "ninety")
hospi_siage <- hospi_siage %>% mutate(date = lubridate::date(date))
hospi_sial <- hospi_siage %>%
    pivot_longer(!date, names_to = "age", values_to = "nbr") %>%
    mutate(age = case_when(age == "zero" ~ "0-9 ans",
                           age == "ten" ~ "10-19 ans",
                           age == "twenty" ~ "20-29 ans",
                           age == "thirty" ~ "30-39 ans",
                           age == "forty" ~ "40-49 ans",
                           age == "fifty" ~ "50-59 ans",
                           age == "sixty" ~ "60-69 ans",
                           age == "seventy" ~ "70-79 ans",
                           age == "eighty" ~ "80-89 ans",
                           age == "ninety" ~ ">= 90 ans"),
           si_ornot = "Dont soins intensifs",
           nbr_si = nbr)
if (dim(hospi_al)[1] != dim(hospi_sial)[1]) stop("Cannot merge!")
hospi_al <- left_join(hospi_al, hospi_sial %>% select(-nbr, -si_ornot),
                      by = c("date", "age"))
hospi_al <- hospi_al %>% mutate(nbr = rowSums(hospi_al[, c(3, 5)], na.rm = TRUE))
hospi_al <- rbind(hospi_al %>% select(-nbr_si), hospi_sial %>% select(-nbr_si))
hospi_al$age <- factor(hospi_al$age, levels = c("0-9 ans", "10-19 ans", "20-29 ans",
                                                "30-39 ans", "40-49 ans", "50-59 ans",
                                                "60-69 ans", "70-79 ans", "80-89 ans",
                                                ">= 90 ans"))
hospi_al$si_ornot <- factor(hospi_al$si_ornot,
                            levels = c("Hospitalisations", "Dont soins intensifs"))

##---------------------------------------------------------------------
## Population (dénominateurs des taux) https://publications.msss.gouv.qc.ca/msss/document-001617/ Institut de la statistique du Québec et MSSS
#pop <- read.xlsx("R/data/EstimationProjectionComparable_1996_2041_20200424.xlsx",
#                 sheet = 3, startRow = 5)
#pop <- pop %>%
#    filter(Année == 2020 & Niveau.géographique == "RSS") %>%
#    mutate(Territoire = stringr::str_sub(Territoire, 4),
#           Territoire = case_when(Territoire == "Saguenay - Lac-Saint-Jean" ~ "Saguenay",
#                                  Territoire == "Mauricie et Centre-du-Québec" ~ "Mauricie",
#                                  Territoire == "Gaspésie - Îles-de-la-Madeleine" ~ "Gaspésie-Îles-de-la-Madeleine", TRUE ~ Territoire))
#saveRDS(pop, "R/data/pop.rds")
pop <- readRDS("R/data/pop.rds")

#pop_age <- read.xlsx("R/data/EstimationProjectionComparable_1996_2041_20200424.xlsx",
#                     sheet = 3, startRow = 5)
#pop_age <- pop_age  %>%
#    filter(Année == 2020 & Niveau.géographique == "Québec" & Sexe == "Total") %>%
#    mutate(neuf = rowSums(test[, c(9:18)], na.rm = TRUE),
#           dixneuf = rowSums(test[, c(19:28)], na.rm = TRUE),
#           vingtneuf = rowSums(test[, c(29:38)], na.rm = TRUE),
#           trenteneuf = rowSums(test[, c(39:48)], na.rm = TRUE),
#           quaranteneuf = rowSums(test[, c(49:58)], na.rm = TRUE),
#           cinquanteneuf = rowSums(test[, c(59:68)], na.rm = TRUE),
#           soixanteneuf = rowSums(test[, c(69:78)], na.rm = TRUE),
#           septanteneuf = rowSums(test[, c(79:88)], na.rm = TRUE),
#           quatrevingtneuf = rowSums(test[, c(89:98)], na.rm = TRUE)) %>%
#    rename(nonante = "90.ans.ou.plus") %>%
#    select("Tous.les.âges", nonante:quatrevingtneuf)
#saveRDS(pop_age, "R/data/pop_age.rds")
pop_age <- readRDS("R/data/pop_age.rds")
pop_agel <- pivot_longer(pop_age, "Tous.les.âges":quatrevingtneuf,
                         names_to = "age", values_to = "pop") %>%
    mutate(age = case_when(age == "neuf" ~ "0-9",
                           age == "dixneuf" ~ "10-19",
                           age == "vingtneuf" ~ "20-29",
                           age == "trenteneuf" ~ "30-39",
                           age == "quaranteneuf" ~ "40-49",
                           age == "cinquanteneuf" ~ "50-59",
                           age == "soixanteneuf" ~ "60-69",
                           age == "septanteneuf" ~ "70-79",
                           age == "quatrevingtneuf" ~ "80-89",
                           age == "nonante" ~ "90+",
                           age == "Tous.les.âges" ~ "Tous âges"))

##---------------------------------------------------------------------------
## age and cases
## https://www.inspq.qc.ca/covid-19/donnees
age <- read.csv("R/data/Graphique 1.6 - page age-sexe.csv")
colnames(age) <- c("date", "case_9", "case_19", "case_29", "case_39",
                   "case_49", "case_59", "case_69", "case_79", "case_89",
                   "case_90")
age <- age %>%
    mutate(date = lubridate::date(date),
           total = rowSums(age[, c(2:10)], na.rm = TRUE))
age_l <- age %>%
    pivot_longer(!date, names_to = "age", values_to = "nbr") %>%
    mutate(age = case_when(age == "case_9" ~ "0-9",
                           age == "case_19" ~ "10-19",
                           age == "case_29" ~ "20-29",
                           age == "case_39" ~ "30-39",
                           age == "case_49" ~ "40-49",
                           age == "case_59" ~ "50-59",
                           age == "case_69" ~ "60-69",
                           age == "case_79" ~ "70-79",
                           age == "case_89" ~ "80-89",
                           age == "case_90" ~ "90+",
                           age == "total" ~ "Tous âges"))

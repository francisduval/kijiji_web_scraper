library(tidyverse)
library(rvest)
library(glue)
library(lubridate)
library(mailR)
library(htmlTable)


# Inputs ------------------------------------------------------------------------------------------------------------------------
path <- "/Users/francisduval/Documents/Hiver 2020/kijiji_web_scraper/" # Path to the R project
sender <- "xxx@gmail.com" # Must be a Gmail email
email_password <- "yourPasswordHere"
recipients <- c("recipient1@xxx.com", "recipient2@xxx.com")


# Create strings for files ------------------------------------------------------------------------------------------------------
file_ads_dataset_RDS <- glue("{path}ads_dataset.RDS")
file_ads_dataset_csv <- glue("{path}ads_dataset.csv")
file_URLs <- glue("{path}URLs.txt")
file_new_ads <- glue("{path}new_ads.csv")


# Load ads database -------------------------------------------------------------------------------------------------------------
if (file.exists(file_ads_dataset_RDS)) {
  all_ads <- readRDS(file_ads_dataset_RDS)
}


# Import and parse URLs ---------------------------------------------------------------------------------------------------------
urls <- read_delim(file_URLs, delim = "\n", col_names = F)
parsed_url <- map(urls[[1]], read_html)


# Functions ---------------------------------------------------------------------------------------------------------------------
get_prices <- function(object) {
  object %>%
    html_nodes(".price") %>%
    html_text() %>% 
    str_trim() %>%
    str_remove_all(",(.*)") %>%
    str_remove_all("\\s") %>% 
    as.numeric()
}

# ----------
get_links <- function(object) {
  links <- object %>%
    html_nodes(".title") %>%
    html_attr("href") %>% 
    na.omit()
  
  glue("https://www.kijiji.ca{links}")
}

# ----------
get_titles <- function(object) {
  res <- object %>% 
    html_nodes(".title") %>%
    html_text() %>% 
    str_trim()
  
  res[seq_along(res) %% 2 == 0]
}

# ----------
get_descriptions <- function(object) {
  object %>% 
    html_nodes(".description") %>%
    html_text() %>% 
    str_trim()
}

# ----------
get_rooms <- function(object) {
  object %>% 
    html_nodes(".details") %>%
    html_text() %>% 
    str_remove_all("Pièces: ") %>% 
    str_trim()
}

# ----------
get_dates_texte <- function(object) {
  object %>% 
    html_nodes(".date-posted") %>% 
    html_text()
}

# ----------
get_dates <- function(object) {
  dates_texte <- object %>% 
    html_nodes(".date-posted") %>% 
    html_text()
  
  vec_dates <- rep(NA, length(dates_texte))
  ind <- seq_along(dates_texte)
  ind_ajd <- str_which(dates_texte, "Il y a")
  ind_hier <- str_which(dates_texte, "hier")
  ind_autres <- str_which(dates_texte, "-")
  
  nb_heures <- as.numeric(str_extract(dates_texte[ind_ajd], pattern = "[0-9]+"))
  
  dates_autres <- dates_texte[ind_autres] %>%
    parse_date("%d-%B-%y", locale = locale("fr")) %>%
    as_date()
  
  vec_dates[ind_autres] <- dates_autres
  vec_dates[ind_ajd] <- as_date(Sys.time() - 3600 * nb_heures)
  vec_dates[ind_hier] <- today() - 1
  
  return(vec_dates)
}


# Create data frame of ads ------------------------------------------------------------------------------------------------------
ads <- tibble(
  titre = unlist(map(parsed_url, get_titles)),
  prix = unlist(map(parsed_url, get_prices)),
  date = as_date(unlist(map(parsed_url, get_dates))),
  lien = unlist(map(parsed_url, get_links))
) %>% 
  distinct(titre, .keep_all = T) %>% 
  filter(!str_detect(titre, "[34].*(1/2)")) %>% 
  arrange(desc(date))


# Determine which ads are not already in the ads dataset ------------------------------------------------------------------------
if (exists("all_ads")) {
  new_ads <- anti_join(ads, all_ads, by = "titre")
} else {
  new_ads <- ads
}


# Augment ads dataset with new ads found ----------------------------------------------------------------------------------------
if (exists("all_ads")) {
  all_ads_update <- bind_rows(all_ads, new_ads) %>%
    select(titre, prix, date, lien) %>%
    arrange(desc(date))
} else {
  all_ads_update <- new_ads %>%
    arrange(desc(date))
}


# Overwrite ads_dataset.RDS and ads_dataset.csv ---------------------------------------------------------------------------------
saveRDS(all_ads_update, file = file_ads_dataset_RDS)
write_csv(all_ads_update, path = file_ads_dataset_csv)
write_csv(new_ads, path = file_new_ads)


# Send email --------------------------------------------------------------------------------------------------------------------
if (nrow(new_ads) > 0) {
  table_html <- htmlTable(new_ads, rnames = FALSE)
  html_body <- glue("<p> Voici les nouvelles annonces trouvées </p> {table_html}")

  send.mail(
    from = sender,
    to = recipients,
    subject = "Nouvelle(s) annonce(s) trouvée(s)!",
    body = html_body,
    html = T,
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 465,
      user.name = sender,
      passwd = email_password,
      ssl = T
    ),
    authenticate = T,
    send = T
  )
}

Sys.time()
cat(nrow(new_ads), " nouvelles annonces")

library(robotstxt)
library(rvest)
library(dplyr)

name_atm_full <- character()

address_atm_full <- character()

paths_allowed(paths = "https://www.techcombank.com.vn/mang-luoi-dia-diem-atm/danh-sach-chi-nhanh-phong-giao-dich-va-atm?page=1")

for (i in 1:5){
  url <- paste0("https://www.techcombank.com.vn/mang-luoi-dia-diem-atm/danh-sach-chi-nhanh-phong-giao-dich-va-atm?page=",i)
  
  webpage <- read_html(url, encoding = "UTF-8")
  name_atm <- webpage %>% 
    html_nodes(".title-entries a") %>% 
    html_text() %>% 
    as.character()
  address_atm <- webpage %>% 
    html_nodes(".address") %>% 
    html_text() %>% 
    as.character()
  name_atm_full <- c(name_atm_full, name_atm)
  address_atm_full <- c(address_atm_full, address_atm)
}

atm_techcom <- data.frame(Name_ATM = name_atm_full, Address_ATM = address_atm_full)
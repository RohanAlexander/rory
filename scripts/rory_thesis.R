#### Preamble ####
# Purpose: To download, prepare, and clean data from Hansard
# Author: Rohan Alexander
# Date: 25 October 2020
# Contact: rohan.alexander@utoronto.ca


#### Workspace set up ####
library(rvest)
library(tidyverse)


#### Gather the data ####
# Get the XML links from here: https://www.aph.gov.au/Parliamentary_Business/Hansard/Hansreps_2011
# Put them into a csv where one column is the url and the other is somewhere to save them
# Function that visits the URL and downloads it
go_get_em <- function(a_url, a_save_location_and_name){
  # Get the xml
  the_xml <- read_html(a_url)
  # Save it locally
  write_html(the_xml, a_save_location_and_name, encoding = "UTF-8")
  # Print to the console so the user knows what's happening
  message <- paste0("Done with ", a_save_location_and_name, " at ", Sys.time())
  print(message)
  rm(the_xml)
  # Pause to be polite
  Sys.sleep(sample(x = c(45:60), size = 1))
}

# Apply the function to the data that we want
get_these <- read_csv("inputs/addresses.csv")
# get_these <- get_these[45:67,]
# This will take a while
# Commented to prevent accidently running
# walk2(get_these$address, get_these$save_name, go_get_em)



#### Prepare the data ####
# For every file we want to read it in, parse it into a tibble and then save that tibble.
go_parse_em <- function(file_to_input, location_to_save_to){
  full_hansard <- read_html(file_to_input, encoding = "UTF-8")
  
  # We want to get rid of some nodes that we don't need
  # Based on https://stackoverflow.com/questions/50768364/how-to-filter-out-nodes-with-rvest
  debateinfo_nodes <- full_hansard %>%
    html_nodes("debateinfo")
  talker_nodes <- full_hansard %>%
    html_nodes("talker")
  subdebateinfo_nodes <- full_hansard %>%
    html_nodes("subdebateinfo")
  pageno_nodes <- full_hansard %>%
    html_nodes("page\\.no")
  subdebatetext_nodes <- full_hansard %>%
    html_nodes("subdebate\\.text")
  spanclasshpsdebate_nodes <- full_hansard %>%
    html_nodes(".HPS-Debate")
  
  xml_remove(debateinfo_nodes)
  xml_remove(talker_nodes)
  xml_remove(subdebateinfo_nodes)
  xml_remove(pageno_nodes)
  xml_remove(subdebatetext_nodes)
  xml_remove(spanclasshpsdebate_nodes)
  
  # Convert to tibble and save
  raw_data <- tibble(all = full_hansard %>% html_text())  
  
  write_csv(raw_data, location_to_save_to)
  
  # Print to the console so the user knows what's happening
  message <- paste0("Done with ", file_to_input, " at ", Sys.time())
  print(message)
}

# Get a list of all the files
all_xml_files <- list.files(path = "inputs/data",
                            full.names = TRUE)

# just try the later ones
all_xml_files <- all_xml_files[35:67]

# Make a list of savenames
csv_save_locations <- str_replace(all_xml_files, "inputs", "outputs")
csv_save_locations <- str_replace(csv_save_locations, ".html", ".csv")

# Apply the function to each
# Will take a few minutes, but just local so doesn't matter.
walk2(all_xml_files, csv_save_locations, go_parse_em)


#### Clean the data ####
# We want a CSV where one column is a uniqueID and then another column is the text
# For every file we want to split the observations every time we identify a line break
go_clean_em <- function(csv_save_locations, location_to_save_to){
  the_days_data <- read_csv(csv_save_locations)
  
  # Looking for spots that have line breaks
  # We take advantage of patterns in those line breaks
  the_days_data <- 
    the_days_data %>% 
    mutate(all = str_replace_all(all, "\\r\\n", "HAHAHAHA"),
           all = str_squish(all)) %>% 
    mutate(all = str_split(all, pattern = "HAHAHAHA HAHAHAHA HAHAHAHA")) %>% 
    unnest(col = all) %>% 
    mutate(all = str_replace_all(all, "HAHAHAHA", "")) %>% 
    filter(!all == " ")
  
  # Try to identify which lines are the start of someone speaking
  # TODO Come back and improve this
  the_days_data <- 
    the_days_data %>% 
    mutate(
      all = str_squish(all),
      is_a_start = str_detect(all, "\\):"),
      
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^A government member: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^An honourable member interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^An opposition member: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr Chalmers interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr CHALMERS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr Emerson: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr GILLESPIE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr JENSEN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr Leigh interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr LEIGH: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr Leigh: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr Mike Kelly: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr MIKE KELLY: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr Southcott: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr SOUTHCOTT: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Dr STONE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Government members interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Government members interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Honourable members interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Honourable members interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Honourable members: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr [:alpha:]{2,20} interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr ABBOTT: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Abbott: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Adams: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr ALBANESE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Albanese: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr ANDREWS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Baldwin: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr BALDWIN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr BANDT: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Bandt: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Billson: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr BILLSON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr BOWEN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Bowen: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr BRADBURY: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Bradbury: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Brendan O'Connor interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr BRENDAN O'CONNOR: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Brendan O'Connor: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Briggs: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr BROAD: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr BROADBENT: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Broadbent: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Brough: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr BRUCE SCOTT: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr BURKE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Burke: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr BUTLER: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Champion interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr CHAMPION: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Champion: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr CHESTER: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Chester: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Christensen: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr CHRISTENSEN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Ciobo: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr CIOBO: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr CLARE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Clare: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr COLEMAN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Combet: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr COMBET: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr CONROY: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr CRAIG KELLY: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr CRAIG THOMSON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr CREAN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Crean: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr CROOK: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Danby: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr DANBY: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr DREYFUS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Dutton interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr DUTTON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Dutton: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Entsch interjecting —")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Ewen Jones interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr EWEN JONES: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Ewen Jones: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr FITZGIBBON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Fitzgibbon: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr FRYDENBERG: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Frydenberg: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr GARRETT: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Garrett: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr GEORGANAS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Georganas: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Giles: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr GOODENOUGH: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Gray: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr GRAY: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr GRIFFIN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Haase: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Hartsuyker: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr HAWKE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Hawke: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr HAYES: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr HOCKEY: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Hockey: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr HOGAN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Hunt: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr HUNT: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr HUSIC: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Husic: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr HUTCHINSON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Hutchinson: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr IAN MACFARLANE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Ian Macfarlane: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr IRONS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Irons: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr JOHN COBB: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr JOYCE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Joyce: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Katter: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr KEENAN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Keenan: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr KELVIN THOMSON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr LAMING: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr LAURIE FERGUSON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr MARTIN FERGUSON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr McCLELLAND: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr McCORMACK: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr McCormack: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Melham: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Mitchell interjecting —")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr MITCHELL: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Mitchell: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr MORRISON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Morrison: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr MURPHY: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Neumann: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr NEUMANN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr NEVILLE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Nikolic interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Nikolic: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr OAKESHOTT: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Pasin: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Perrett: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr PERRETT: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr PORTER: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr PYNE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Pyne: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr RAMSEY: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Ramsey: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr RANDALL: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr ROBB: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr RUDD: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Rudd: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr RUDDOCK: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Ruddock: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Schultz: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr SECKER: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Secker: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr SHORTEN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Shorten: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr SHORTEN:")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr SIDEBOTTOM: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr SIMPKINS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Simpkins: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr SNOWDON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Stephen Jones interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Stephen Jones: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr STEPHEN JONES: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr STEPHEN SMITH: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Stephen Smith: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr SWAN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr SWAN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Swan: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr TAYLOR: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr TEHAN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Tehan: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Tony Smith interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr TONY SMITH: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Tony Smith: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr TUDGE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Tudge: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr TURNBULL: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Turnbull: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr VAN MANEN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Watts: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr WHITELEY: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr WILKIE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr WILLIAMS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr WILSON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Windsor: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Wyatt: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr WYATT: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr ZAPPIA: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mr Zimmerman: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs ANDREWS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs Bronwyn Bishop interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs BRONWYN BISHOP: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs Bronwyn Bishop: ")),    
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs Gash: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs Griggs interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs GRIGGS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs MARKUS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs Markus: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs McNAMARA: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs Mirabella: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs MIRABELLA: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Mrs MOYLAN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms [:alpha:]{2,20} interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms BIRD: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Bird: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms BRODTMANN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Brodtmann: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Butler: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms CHESTERS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Claydon: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms CLAYDON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms COLLINS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Collins: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms GAMBARO: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms GAMBARO:")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms GILLARD: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Gillard: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms HALL: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Hall: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Henderson: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms HENDERSON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Julie Bishop interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms JULIE BISHOP: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Julie Bishop: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Kate Ellis interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms KATE ELLIS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Kate Ellis: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms KING: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms King: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms LEY: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Ley: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms LIVERMORE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Macklin: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms MACKLIN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms MacTIERNAN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms MARINO: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms McGOWAN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms McGowan: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms O'DWYER: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms O'Neil interjecting—")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms O'Neill: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Owens interjecting —")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Owens: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms OWENS: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms PARKE: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms PLIBERSEK: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Plibersek: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Price: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Rishworth: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms RISHWORTH: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Rowland: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms ROXON: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms RYAN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms SAFFIN: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Smyth: ")),    
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms VAMVAKINOU: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Ms Vamvakinou: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Opposition members interjecting")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Opposition members: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^The DEPUTY SPEAKER: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^The SPEAKER: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^Unknown member: ")),
      is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(all, "^WYATT ROY: ")),
      
    ) %>% 
    mutate(is_a_start = if_else(is_a_start == TRUE, 1, 0)) %>% 
    mutate(cumulative_starts = cumsum(is_a_start))
  
  # From: https://stackoverflow.com/questions/15933958/collapse-concatenate-aggregate-a-column-to-a-single-comma-separated-string-w
  # Finally concatenate the lines
  the_days_data <- 
    the_days_data %>% 
    group_by(cumulative_starts) %>% 
    summarize(text = str_c(all, collapse = "\n"))
  
  write_csv(the_days_data, location_to_save_to)
  
  # Print to the console so the user knows what's happening
  message <- paste0("Done with ", location_to_save_to, " at ", Sys.time())
  print(message)
  
}

location_to_save_to <- str_replace(csv_save_locations, "data/", "cleaned/")

# Apply the function to each
# Will take a few minutes, but just local so doesn't matter.
walk2(csv_save_locations, location_to_save_to, go_clean_em)



# Now we need to separate the names from the text
go_fix_the_names <- function(location_to_read_and_save_to){
  the_days_data <- read_csv(location_to_read_and_save_to)
  
  the_days_data <- 
    the_days_data %>% 
    separate(text, into = c("name", "text"), sep = ": ", extra = "merge", fill = "left")

  # Now we need to clean the names
  the_days_data <- 
    the_days_data %>% 
    mutate(name = str_remove(name, "\\([:digit:]{2}:[:digit:]{2}\\)")) %>% 
    separate(name, into = c("name", "titles_etc"), sep = "\\(", extra = "merge", fill = "right") %>% 
    select(-titles_etc, -cumulative_starts)
  
  write_csv(the_days_data, location_to_read_and_save_to)
  
  # Print to the console so the user knows what's happening
  message <- paste0("Done with ", location_to_read_and_save_to, " at ", Sys.time())
  print(message)
  
}

walk(location_to_save_to, go_fix_the_names)


#### End matter ####
# It may be useful to have everything in one dataframe
# This will be quite slow if you have a lot of days, but will be fine for a year or two

all_data <- map_dfr(location_to_save_to, read_csv)

head(all_data)

write_csv(all_data, "have_a_look.csv")




























#### Working

hansard <- read_html("https://parlinfo.aph.gov.au/parlInfo/download/chamber/hansardr/17ce9e95-ddb6-408d-8d19-954ef4b886b1/toc_unixml/House%20of%20Representatives_2011_05_31_70_Official.xml;fileType=text%2Fxml")

# hansard <- read_html("https://parlinfo.aph.gov.au/parlInfo/download/chamber/hansardr/7bce5c16-c1b0-43c9-9392-8ff6f8f1352b/toc_unixml/House%20of%20Representatives_2011_05_10_10_Official.xml;fileType=text%2Fxml")
hansard <- read_html("inputs/data/2011-03-24.html", encoding = "UTF-8")

# otherhansard <- read_html("inputs/data/2010-09-30.html", encoding = "UTF-8")

test <- hansard

#### That ####
# https://stackoverflow.com/questions/50768364/how-to-filter-out-nodes-with-rvest
debateinfo_nodes <- test %>%
  html_nodes("debateinfo")

talker_nodes <- test %>%
  html_nodes("talker")

subdebateinfo_nodes <- test %>%
  html_nodes("subdebateinfo")

pageno_nodes <- test %>%
  html_nodes("page\\.no")

subdebatetext_nodes <- test %>%
  html_nodes("subdebate\\.text")

spanclasshpsdebate_nodes <- test %>%
  html_nodes(".HPS-Debate")

xml_remove(debateinfo_nodes)
xml_remove(talker_nodes)
xml_remove(subdebateinfo_nodes)
xml_remove(pageno_nodes)
xml_remove(subdebatetext_nodes)
xml_remove(spanclasshpsdebate_nodes)


raw_data <- tibble(some_data = test %>% html_text())


#HERE

write_csv(raw_data, "atea.csv")

asdfraw_data <- 
  raw_data %>% 
  mutate(some_data = str_replace_all(some_data, "\\r\\n", "HAHAHAHA"),
         some_data = str_squish(some_data)) %>% 
  mutate(some_data = str_split(some_data, pattern = "HAHAHAHA HAHAHAHA HAHAHAHA")) %>% 
  unnest(col = some_data) %>% 
  mutate(some_data = str_replace_all(some_data, "HAHAHAHA", "")) %>% 
  filter(!some_data == " ")

# Add flag 
asdfraw_data <- 
  asdfraw_data %>% 
  mutate(
    some_data = str_squish(some_data),
    is_a_start = str_detect(some_data, "\\):"),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^The SPEAKER: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr ABBOTT: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr SWAN: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Ms GILLARD: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr BANDT: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr BOWEN: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr BRADBURY: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr CHAMPION: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr DREYFUS: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr EWEN JONES: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr FRYDENBERG: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr HUSIC: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr IAN MACFARLANE: ")),
    
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr LAMING: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr McCLELLAND: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr McCORMACK: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr NEUMANN: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr NEVILLE: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr STEPHEN JONES: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr SWAN: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr TEHAN: ")),
    
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr TUDGE: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr WILKIE: ")),

    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Opposition members interjecting")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Honourable members interjecting—")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Honourable members: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr [:alpha:]{2,20} interjecting—")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Ms [:alpha:]{2,20} interjecting—")),
    
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Abbott: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Albanese: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Hockey: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Hunt: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr McCormack: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Mitchell: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Morrison: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Perrett: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Pyne: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Ruddock: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Schultz: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Tehan: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mr Tudge: ")),
    
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Mrs Markus: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Ms Gillard: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Ms Julie Bishop: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Ms Kate Ellis: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Ms Ley: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Ms LIVERMORE: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Ms Macklin: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Ms Owens: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Ms OWENS: ")),
    is_a_start = if_else(is_a_start == TRUE, is_a_start, str_detect(some_data, "^Ms Smyth: ")),    
    
  ) %>% 
  mutate(is_a_start = if_else(is_a_start == TRUE, 1, 0)) %>% 
  mutate(cumulative_starts = cumsum(is_a_start))
  

# From: https://stackoverflow.com/questions/15933958/collapse-concatenate-aggregate-a-column-to-a-single-comma-separated-string-w

asdfraw_data_concat <- 
  asdfraw_data %>% 
  group_by(cumulative_starts) %>% 
  summarize(text = str_c(some_data, collapse = "\n"))

# Now we need to separate the names from the text
asdfraw_data_concat <- 
  asdfraw_data_concat %>% 
  separate(text, into = c("name", "text"), sep = ": ", extra = "merge", fill = "left")

# Now we need to clean the names

asdfraw_data_concat <- 
  asdfraw_data_concat %>% 
  mutate(name = str_remove(name, "\\([:digit:]{2}:[:digit:]{2}\\)")) %>% 
  separate(name, into = c("name", "titles_etc"), sep = "\\(", extra = "merge", fill = "right") %>% 
  select(-titles_etc, -cumulative_starts)



  
  





# We only need Gillard or Abbott
only_gillard_and_abbott <- 
  all %>% 
  mutate(is_gillard_or_abbott = str_detect(string = name, pattern = "GILLARD|ABBOTT")) %>% 
  filter(is_gillard_or_abbott == TRUE)

names <- all$name %>% count()


rm()
only_gillard_and_abbott$text[10]










#### Parse the data ####
# hansard %>%
#   html_nodes("#documentContent") %>%
#   html_text()

# First we use the xml tags to identify the content that we want
all_speechs <- xml_find_all(hansard, "//talk.text") %>% xml_text()

# Now push it into a tibble
all <- tibble(raw = all_speechs)



#### Clean the data ####
# First we need to get rid of the random spaces and regex formatting 
all <- 
  all %>% 
  mutate(clean = str_remove_all(raw, "\\r\\n"),
         clean = str_squish(clean)
         ) %>% 
  select(clean) %>% 
  filter(clean != "")


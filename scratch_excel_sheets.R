library(readxl)



# read_excel("~/schedule_sheet_CBC0.xlsx")


# Pulls every excel file that has CBC in the name then places it in a vector.
Expeditions <- vector()
dir5 <- list.files("~", full.names = T, recursive = F, pattern = "*CBC*")
Expeditions <- append(dir5, Expeditions)
Expeditions



# One way I attempted to do this was through using map_df and then creating my own function to pull excactly what I needed
# This gives an error of Sheet "Schedule" not found even though each excel document 100% has Schedule as a sheet name
data_xlsx_df <- map_df(set_names(Expeditions), function(file) {
  file %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map_df(
      ~ read_xlsx(path = file, sheet = "Schedule", cell_cols("W:Z")),
      .id = "sheet")
}, .id = "file")

# Columns W:Z with added 'file' column specifying the excel file
data_xlsx_df2 <- map_df(set_names(Expeditions), function(file) {
  read_xlsx(path = file, sheet = "Schedule", cell_cols("W:Z"))},
  .id = "file")

# Same thing but file names are shortened
data_xlsx_df2 <- map_df(Expeditions, function(file) {
  dat = read_xlsx(path = file, sheet = "Schedule", cell_cols("W:Z")) %>%
  mutate(file = basename(file)) %>%
  select(file, everything())})

# Reads in data from columns W:Z for each sheet, per each excel file
# w/ 'file' column and 'sheet' column
data_xlsx_df2 <- map_df(set_names(Expeditions), function(file) {
  file %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(function(x) {
      # browser()
      read_xlsx(path = file, sheet = x, cell_cols("W:Z"))
    }, .id = "sheet")
}, .id = "file")
    


dat <- map_df(set_names(Expeditions), function(file) {
  Sheet = file %>%
    excel_sheets()})
data_xlsx_df2 <- map_df(dat, function(x) {
  read_xlsx(path = names(dat), sheet)
})


set_names(Expeditions)[1] %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(
    ~ read_xlsx(path = "C:/Users/brega/Documents/schedule_sheet_CBC0.xlsx",
                sheet = "Schedule", cell_cols("W:Z")),
    .id = "sheet")

read_xlsx(path = "C:/Users/brega/Documents/schedule_sheet_CBC0.xlsx",
          sheet = "Schedule", cell_cols("W:Z"))








# In theory this should using the sheet option pull the Schedule sheet at the W through Z columns
# Returns error Sheet "Schedule" not found

for (i in Expeditions) {
  read_xlsx(i, sheet = "Schedule", cell_cols("W:Z"))
}



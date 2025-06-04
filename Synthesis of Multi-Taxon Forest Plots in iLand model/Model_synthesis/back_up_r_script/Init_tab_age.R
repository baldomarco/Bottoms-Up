# Carica le librerie necessarie
library(dplyr)
library(readr)

# Definisci i percorsi delle cartelle
old_folder <- "C:/iland/20230901_Bottoms_Up/plot_init/gis/init/init_with_ages_old_KDE"
new_folder <- "C:/iland/20230901_Bottoms_Up/plot_init/gis/init/init_CORR_GAM"
output_folder <- "C:/iland/20230901_Bottoms_Up/plot_init/gis/init/init_good"

# Ottieni i nomi dei file da entrambe le cartelle
old_files <- list.files(old_folder, full.names = FALSE, pattern = "\\.txt$")
new_files <- list.files(new_folder, full.names = FALSE, pattern = "\\.txt$")

# Assicurati che i nomi dei file corrispondano
file_names <- intersect(basename(old_files), basename(new_files))

# Inizializza un log per tracciare i file senza corrispondenze
no_match_log <- character()

# Elabora ogni file
for (file_name in file_names) {
  # Leggi i dati dai file
  old_data <- read_delim(file.path(old_folder, file_name), delim = " ", col_types = cols())
  new_data <- read_delim(file.path(new_folder, file_name), delim = " ", col_types = cols())
  
  # Debug: stampare dimensioni dei dati
  cat("File: ", file_name, "\n")
  cat("Dimensioni old_data: ", nrow(old_data), " righe\n")
  cat("Dimensioni new_data: ", nrow(new_data), " righe\n")
  
  # Assicurati che le colonne di join siano dello stesso tipo
  old_data <- old_data %>%
    mutate(dbh = as.numeric(dbh), height = as.numeric(height))  # Assicurati che dbh e height siano numerici
  
  new_data <- new_data %>%
    mutate(dbh = as.numeric(dbh), height = as.numeric(height))  # Assicurati che dbh e height siano numerici
  
  # Gestione della colonna 'age' (conversione a numerico)
  old_data <- old_data %>%
    mutate(age = as.numeric(age))  # Assicurati che age sia numerico (se possibile)
  
  new_data <- new_data %>%
    mutate(age = as.numeric(age))  # Assicurati che age sia numerico (se possibile)
  
  # Unisci i dati per le righe corrispondenti
  updated_data <- old_data %>%
    left_join(new_data, by = c("x", "y", "species", "dbh", "height"), suffix = c(".old", ".new")) %>%
    mutate(age = if_else(!is.na(age.new), age.new, age.old)) %>%
    select(-age.old, -age.new)  # Rimuovi le vecchie colonne age
  
  # Debug: numero di righe dopo unione
  cat("Numero di righe in updated_data (dopo unione): ", nrow(updated_data), "\n")
  
  # Scrivi il file aggiornato nella cartella di output
  write_delim(updated_data, file.path(output_folder, file_name), delim = " ", na = "NA")
}

# Salva il log dei file senza corrispondenze
if (length(no_match_log) > 0) {
  writeLines(no_match_log, file.path(output_folder, "no_matches_log.txt"))
}

# Stampa il messaggio di completamento
cat("Elaborazione completata. Controlla la cartella 'init_good' e 'no_matches_log.txt' per i risultati.\n")

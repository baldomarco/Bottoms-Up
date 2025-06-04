# Carichiamo i pacchetti necessari
library(flextable)  # Per creare tabelle formattate
library(officer)    # Per esportare tabelle in Word

# Creiamo la tabella con i taxa nei tre linguaggi
taxa_table <- data.frame(
  Italiano = c("Briofite", "Licheni", "Coleotteri", "Macrofunghi", "Falene"),
  Inglese = c("Bryophytes", "Lichens", "Beetles", "Macrofungi", "Moths"),
  Latino = c("Bryophyta (phylum)",  # Briofite
             "Lichenes (gruppo informale)",  # Licheni
             "Coleoptera (ordine)",  # Coleotteri
             "Fungi (regno) o Basidiomycota/Ascomycota (phyla principali)",  # Macrofunghi
             "Lepidoptera (ordine)")  # Falene
)

# Convertiamo il data frame in una flextable per una formattazione migliore
ft <- flextable(taxa_table)

# Creiamo un nuovo documento Word e aggiungiamo la tabella
doc <- read_docx() %>%
  body_add_flextable(ft)

# Per generare in automatico un path di salvataggio
setwd("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/")

# Salviamo il documento Word con il nome "Taxa_Tabella.docx"
print(doc, target = "Taxa_Tabella.docx")

# Ora puoi trovare il file "Taxa_Tabella.docx" nella tua directory di lavoro e aprirlo in Word!

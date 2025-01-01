setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/")

library(tidyverse)
library(dplyr)
library(sf)
library(flextable)
library(stringr)
library(stringi)
library(lubridate)
library(readxl)
library(ggplot2)

A<- read_csv("C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/AFRO_LQAS_data.csv")

LQAS_rep<-A |>
  filter(start_date >  as_date(2019-10-1)) |>
  arrange(start_date) 
# B$end_date <- ymd(B$end_date)
# year(B$end_date[year(B$end_date) == 2028]) <- 2023
# year(B$end_date[year(B$end_date) == 2025]) <- 2024


C<-A |> 
  mutate(
    start_date = case_when(
      response == "Mop_Up" & start_date ==2024-06-05~ as_date("2024-04-28"),
      response == "Revaccination" & start_date ==2024-05-05~ as_date("2024-04-28"),
      response == "Revaccination" & start_date ==2024-05-04~ as_date("2024-04-28"),
      response == "SNID" & start_date ==2024-05-04~ as_date("2024-04-28"),
      TRUE ~ start_date))

D<-C |> 
  mutate(country =  case_when(
    country =="Ethiopia" ~ "ETH",
    TRUE ~ country)) 
E<-D |> 
  mutate(roundNumber =  case_when(
    roundNumber =="RND2" ~ "Rnd2",
    roundNumber =="RND1" ~ "Rnd1",
    roundNumber =="RND3" ~ "Rnd3",
    TRUE ~ roundNumber))

G <- E |>
  mutate(province = toupper(province),
         district = toupper(district)) |> 
  mutate(
    country = case_when(
      country == "GAM" ~ "GAMBIA",
      country == "GHA" ~ "GHANA",
      country == "ALG" ~ "ALGERIA",
      country == "ETH" ~ "ETHIOPIA",
      country == "ANG" ~ "ANGOLA",
      country == "BEN" ~ "BENIN",
      country == "BUI" ~ "BURUNDI",
      country == "BURKINA_FASO" ~ "BURKINA FASO",
      country == "BFA" ~ "BURKINA FASO",
      country == "BWA" ~ "BOTSWANA",
      country == "CHD" ~ "CHAD",
      country == "CAE" ~ "CAMEROON",
      country == "CIV" ~ "COTE D IVOIRE",
      country == "COG" ~ "CONGO",
      country == "GUI" ~ "GUINEA",
      country == "KEN" ~ "KENYA",
      country == "LIB" ~ "LIBERIA",
      country == "MAL" ~ "MALI",
      country == "MAU" ~ "MAURITANIA",
      country == "MDG" ~ "MADAGASCAR",
      country == "MLW" ~ "MALAWI",
      country == "MOZ" ~ "MOZAMBIQUE",
      country == "NIE" ~ "NIGERIA",
      country == "NIG" ~ "NIGER",
      country == "RCA" ~ "CENTRAL AFRICAN REPUBLIC",
      country == "RDC" ~ "DEMOCRATIC REPUBLIC OF THE CONGO",
      country == "RWA" ~ "RWANDA",
      country == "SEN" ~ "SENEGAL",
      country == "SIL" ~ "SIERRA LEONE",
      country == "SSUD" ~ "SOUTH SUDAN",
      country == "TNZ" ~ "UNITED REPUBLIC OF TANZANIA",
      country == "TOG" ~ "TOGO",
      country == "UGA" ~ "UGANDA",
      country == "ZIM" ~ "ZIMBABWE",
      country == "ZMB" ~ "ZAMBIA",
      TRUE ~ country
    ),
    district = case_when(
      country	=="MALI"&	district	=="district KOULIKORO" ~	"KOULIKORO",
      country	=="MALI"&	district	=="district SIKASSO" ~	"SIKASSO",
      country	=="GUINEA"&	district	=="NZEREKORE" ~	"N'ZÉRÉKORÉ",
      country	=="NIGER"&	district	=="AGADEZ COMMUNE" ~	"AGADEZ",
      country	=="NIGER"&	district	=="DIFFA COMMUNE" ~	"DIFFA",
      country	=="NIGER"&	district	=="MAINÉ SOROA" ~	"MAINE SOROA",
      country	=="NIGER"&	district	=="DOGON-DOUTCHI" ~	"DOGON DOUTCHI",
      country	=="NIGER"&	district	=="FALMEYE" ~	"FALMEY",
      country	=="NIGER"&	district	=="TAHOUA COMMUNE" ~	"TAHOUA COM",
      country	=="NIGER"&	district	=="TAHOUA DÉPARTEMENT" ~	"TAHOUA DEP",
      country	=="NIGER"&	district	=="MATAMAYE" ~	"MATAMÈYE",
      country	=="NIGER"&	district	=="TIBIRI (DOUTCHI)" ~	"TIBIRI",
      country	=="NIGER"&	district	=="NIAMEY  I" ~	"NIAMEY 1",
      country	=="NIGER"&	district	=="NIAMEY  II" ~	"NIAMEY 2",
      country	=="NIGER"&	district	=="NIAMEY  III" ~	"NIAMEY 3",
      country	=="NIGER"&	district	=="NIAMEY  IV" ~	"NIAMEY 4",
      country	=="NIGER"&	district	=="NIAMEY  V" ~	"NIAMEY 5",
      country	=="NIGER"&	district	=="TAHOUA VILLE" ~	"TAHOUA COM",
      country	=="NIGER"&	district	=="BALLAYARA" ~	"BALLEYARA",
      country	=="NIGER"&	district	=="GOTHEYE" ~	"GOTHÈYE",
      country	=="NIGER"&	district	=="OULLAM" ~	"OUALLAM",
      country	=="NIGER"&	district	=="TILLABÉRY" ~	"DS TILLABERI",
      country	=="NIGER"&	district	=="BELBÉDJI" ~	"BELBEDJI",
      country	=="NIGER"&	district	=="TCHIROZÃ‰RINE" ~	"TCHIROZÉRINE",
      country	=="CAMEROON"&	district	=="BIYEM_ASSI" ~	"BIYEM ASSI",
      country	=="CAMEROON"&	district	=="CITE_VERTE" ~	"CITE VERTE",
      country	=="CAMEROON"&	district	=="ELIG_MFOMO" ~	"ELIG MFOMO",
      country	=="CAMEROON"&	district	=="NANGA_EBOKO" ~	"NANGA EBOKO",
      country	=="CAMEROON"&	district	=="NGOG_MAPUBI" ~	"NGOG MAPUBI",
      country	=="CAMEROON"&	district	=="ABONG_MBANG" ~	"ABONG MBANG",
      country	=="CAMEROON"&	district	=="BETARE_OYA" ~	"BETARE OYA",
      country	=="CAMEROON"&	district	=="GAROUA-BOULAI" ~	"GAROUA BOULAI",
      country	=="CAMEROON"&	district	=="NGUELEMENDOUGA" ~	"NGUELEMENDOUKA",
      country	=="CAMEROON"&	district	=="KAR_HAY" ~	"KAR HAY",
      country	=="CAMEROON"&	district	=="MAROUA1" ~	"MAROUA 1",
      country	=="CAMEROON"&	district	=="MAROUA2" ~	"MAROUA 2",
      country	=="CAMEROON"&	district	=="MAROUA3" ~	"MAROUA 3",
      country	=="CAMEROON"&	district	=="CITE_DES_PALMIERS" ~	"CITE PALMIERS",
      country	=="CAMEROON"&	district	=="NJOMBE_PENJA" ~	"NJOMBE PENJA",
      country	=="CAMEROON"&	district	=="NEWBELL" ~	"NEW BELL",
      country	=="CAMEROON"&	district	=="BAMENDA 3" ~	"BAMENDA",
      country	=="CAMEROON"&	district	=="BAMENDA III" ~	"BAMENDA",
      country	=="CAMEROON"&	district	=="KUMBOEAST" ~	"KUMBO EAST",
      country	=="CAMEROON"&	district	=="KUMBOWEST" ~	"KUMBO WEST",
      country	=="CAMEROON"&	district	=="GAROUA I" ~	"GAROUA 1",
      country	=="CAMEROON"&	district	=="GAROUA II" ~	"GAROUA 2",
      country	=="CAMEROON"&	district	=="GASHIGA" ~	"GASCHIGA",
      country	=="CAMEROON"&	district	=="MALANTOUEN" ~	"MALENTOUEN",
      country	=="CAMEROON"&	district	=="PENKAMICHEL" ~	"PENKA MICHEL",
      country	=="CAMEROON"&	district	=="EKONDO_TITI" ~	"EKONDO TITI",
      country	=="CAMEROON"&	district	=="EYUMOJOCK" ~	"EYUMODJOCK",
      country	=="CAMEROON"&	district	=="KUMBA NORD" ~	"KUMBA",
      country	=="CAMEROON"&	district	=="KUMBA SUD" ~	"KUMBA",
      country	=="CAMEROON"&	district	=="KUMBA NORTH" ~	"KUMBA",
      country	=="CAMEROON"&	district	=="KUMBA SOUTH" ~	"KUMBA",
      country	=="CHAD"&	district	=="OUM-HADJER" ~	"OUM HADJER",
      country	=="CHAD"&	district	=="KOUBA OLANGA5" ~	"KOUBA OLANGA",
      country	=="CHAD"&	district	=="BAILLI" ~	"BA ILLI",
      country	=="CHAD"&	district	=="MOURDI" ~	"MOURDI DJONA",
      country	=="CHAD"&	district	=="OUNIANGA" ~	"OUNIANGA KEBIR",
      country	=="CHAD"&	district	=="LAOKASSI" ~	"LAOKASSY",
      country	=="CHAD"&	district	=="NDJAMENA CENTRE" ~	"N'DJAMENA CENTRE",
      country	=="CHAD"&	district	=="NDJAMENA EST" ~	"N'DJAMENA EST",
      country	=="CHAD"&	district	=="NDJAMENA NORD" ~	"N'DJAMENA NORD",
      country	=="CHAD"&	district	=="NDJAMENA SUD" ~	"N'DJAMENA SUD",
      country	=="CHAD"&	district	=="AMADAM" ~	"AM DAM",
      country	=="CHAD"&	district	=="AMTIMAN" ~	"AM TIMAN",
      country	=="CHAD"&	district	=="MANGUEIGNE" ~	"HARAZE MANGUEIGNE",
      country	=="CHAD"&	district	=="GOZ-BEIDA" ~	"GOZ BEIDA",
      country	=="CHAD"&	district	=="N’TIONA" ~	"N'TIONA",
      country	=="CHAD"&	district	=="DS CHADDRA" ~	"CHADRA",
      country	=="CHAD"&	district	=="DS MOUSSORO" ~	"MOUSSORO",
      country	=="CHAD"&	district	=="DJEDA" ~	"DJEDDA",
      country	=="CHAD"&	district	=="OUMHADJER" ~	"OUM HADJER",
      country	=="CHAD"&	district	=="BAGASOLA" ~	"BAGASSOLA",
      country	=="CENTRAL AFRICAN REPUBLIC"&	district	=="BABOUA" ~	"BABOUA-ABBA",
      country	=="CENTRAL AFRICAN REPUBLIC"&	district	=="BOUAR" ~	"BOUAR-BAORO",
      country	=="CENTRAL AFRICAN REPUBLIC"&	district	=="CARNOT" ~	"CARNOT-GADZI",
      country	=="CENTRAL AFRICAN REPUBLIC"&	district	=="BOCARANGA" ~	"BOCARANGA-KOUI",
      country	=="CENTRAL AFRICAN REPUBLIC"&	district	=="BOGUILA" ~	"NANGA-BOGUILA",
      country	=="CENTRAL AFRICAN REPUBLIC"&	district	=="BOZOUM" ~	"BOZOUM-BOSSEMPTELE",
      country	=="CENTRAL AFRICAN REPUBLIC"&	district	=="GRIMARI" ~	"KOUANGO-GRIMARI",
      country	=="CENTRAL AFRICAN REPUBLIC"&	district	=="ALINDAO" ~	"ALINDAO-MINGALA",
      country	=="CENTRAL AFRICAN REPUBLIC"&	district	=="KEMBE" ~	"KEMBE-SATEMA",
      country	=="CENTRAL AFRICAN REPUBLIC"&	district	=="MOBAYE" ~	"MOBAYE-ZANGBA",
      country	=="CENTRAL AFRICAN REPUBLIC"&	district	=="OUANGO" ~	"OUANGO-GAMBO",
      country	== "GAMBIA" & district	== "NIAMINA DANKUNKU" ~	"NIAMNA DANKUNKU",
      country	== "GAMBIA" &	district	== "SABAKH SANJAL" ~	"SABACH",
      country	== "GAMBIA" &	district	== "FULLADU EAST" ~	"BASSE",
      country	== "GAMBIA" &
        district	== "LOWER FULLADU WEST" ~	"CENTRAL RIVER province",
      country	== "GHANA" &
        district	== "ASANTE MAMPONG" ~	"ASANTE-MAMPONG",
      country	== "GHANA" &
        district	== "ASOKORE MAMPONG" ~	"ASOKORE-MAMPONG",
      country	== "GHANA" &
        district	== "ABUAKWA SOUTH" ~	"EAST AKIM - ABUAKWA SOUTH",
      country	== "GHANA" &
        district	== "LOWER MANYA-KROBO" ~	"LOWER-MANYA-KROBO",
      country	== "GHANA" &
        district	== "TWIFO ATI MORKWA" ~	"TWIFO ATI-MORKWA",
      country	== "ANGOLA" &	district	== "N'HARÃŠA" ~	"NHAREA",
      country	== "ANGOLA" &	district	== "NGONGUEMBO" ~	"GONGUEMBO",
      country	== "ANGOLA" &	district	== "NÃ“QUI" ~	"NOQUI",
      country	== "ANGOLA" &	district	== "PANGO-ALUQUEM" ~	"PANGO ALUQUEM",
      country	== "ANGOLA" &
        district	== "TÃ”MBUA (EX. PORTO ALEXANDRE)" ~	"TOMBUA",
      country	== "ANGOLA" &	district	== "UCUMA" ~	"UKUMA",
      country	== "ANGOLA" &	district	== "UÃGE" ~	"UIGE",
      country	== "ANGOLA" &	district	== "XÃ-MUTEBA" ~	"XA MUTEBA",
      country	== "ANGOLA" &	district	== "NZETU" ~	"NZETO",
      country	== "ANGOLA" &	district	== "CELA (EX. UACU-CUNGO)" ~	"CELA",
      country	== "ANGOLA" &
        district	== "OMBADJA (EX. CUAMATO)" ~	"OMBADJA",
      country	== "ANGOLA" &
        district	== "TCHICALA TCHOLOHANGA" ~	"TCHIKALA-TCHOLOHAN",
      country	== "ANGOLA" &
        district	== "BUNDAS" ~	"LUMBALA NGUIMBO (BUNDAS)",
      country	== "ANGOLA" &	district	== "AMBOIM (EX. GABELA)" ~	"AMBOIM",
      country	== "ANGOLA" &	district	== "AMBUÃLA" ~	"AMBUILA",
      country	== "ANGOLA" &	district	== "BAÃA FARTA" ~	"BAIA FARTA",
      country	== "ANGOLA" &
        district	== "BUENGAS (EX. NOVA ESPERANÃ‡A)" ~	"BUENGAS",
      country	== "ANGOLA" &	district	== "BULA-ATUMBA" ~	"BULA ATUMBA",
      country	== "ANGOLA" &	district	== "QUIUABA-N'ZOGI" ~	"KIWABA NZOGI",
      country	== "ANGOLA" &	district	== "SAMBA CAJÃš" ~	"SAMBA CAJU",
      country	== "ANGOLA" &	district	== "SELES (EX. UCU SELES)" ~	"SELES",
      country	== "ANGOLA" &	district	== "SUMBE (EX. NGUNZA)" ~	"SUMBE",
      country	== "ANGOLA" &	district	== "CAMEIA" ~	"LUMEJE (CAMEIA)",
      country	== "ANGOLA" &
        district	== "CATABOLA (EX. NOVA SINTRA)" ~	"CATABOLA",
      country	== "ANGOLA" &	district	== "LÃ‰UA" ~	"LEUA",
      country	== "ANGOLA" &	district	== "LIBOLO (EX. CALULO)" ~	"LIBOLO",
      country	== "ANGOLA" &	district	== "LÃ“VUA" ~	"LOVUA",
      country	== "ANGOLA" &
        district	== "BUNDAS-LUMBALA-NGUIMBO" ~	"LUMBALA NGUIMBO (BUNDAS)",
      country	== "ANGOLA" &	district	== "CAÃLA" ~	"CAALA",
      country	== "ANGOLA" &
        district	== "CACONGO (EX. LÃ‚NDANA)" ~	"CACONGO",
      country	== "ANGOLA" &	district	== "DANDE (CAXITO)" ~	"DANDE",
      country	== "ANGOLA" &
        district	== "DEMBOS-QUIBAXE" ~	"DEMBOS (QUIBAXE)",
      country	== "ANGOLA" &	district	== "GAMBOS (EX. CHIANGE)" ~	"GAMBOS",
      country	== "ANGOLA" &
        district	== "CUNDA-DIA-BAZE" ~	"KUNDA-DIA-BAZE",
      country	== "ANGOLA" &	district	== "CUNHINGA (VOUGA)" ~	"CUNHINGA",
      country	== "ANGOLA" &
        district	== "MUCABA (EX. QUINZALA)" ~	"MUCABA",
      country	== "ANGOLA" &	district	== "MUCARI" ~	"CACULAMA (MUCARI)",
      country	== "ANGOLA" &
        district	== "TCHIKALA TCHOLOHANG" ~	"TCHIKALA-TCHOLOHAN",
      country	== "ANGOLA" &	district	== "CUROCA (EX. ONCOCUA)" ~	"CUROCA",
      country	== "ANGOLA" &
        district	== "MILUNGA (SANTA CRUZ)" ~	"MILUNGA",
      country	== "ANGOLA" &	district	== "LUENA" ~	"MOXICO (LUENA)",
      country	== "NIGER" &	district	== "AGUIÃ‰" ~	"AGUIÉ",
      country	== "NIGE" &	district	== "TCHIROZÃ‰RINE" ~	"TCHIROZÉRINE",
      country	== "NIGER" &	district == "TÃ‰RA" ~	"TERA",
      country	== "NIGER" &	district	== "GOURÃ‰" ~	"GOURÉ",
      country	== "NIGER" &	district	== "IFÃ‰ROUANE" ~	"IFÉROUANE",
      country	== "NIGER" &	district	== "ILLÃ‰LA" ~	"ILLÉLA",
      country	== "NIGER" &	district	== "MATAMÃˆYE" ~	"MATAMEYE",
      country	== "NIGER" &	district	== "FILINGUÃ‰" ~	"FILINGUE",
      country	== "NIGER" &	district	== "KANTCHÃ‰" ~	"KANTCHÉ",
      country	== "NIGER" &	district	== "GOTHÃˆYE" ~	"GOTHÈYE",
      country	== "NIGER" &	district	== "JINJA CITY‰" ~	"JINJA",
      country	== "NIGER" &	district	== "MBALE CITY" ~	"MBALE",
      country	== "MAURITANIA" &	district	== "RIYADH" ~	"RIYAD",
      country	== "MAURITANIA" &	district	== "LEKSEIBE" ~	"LEXEIBA",
      country	== "MAURITANIA" &	district	== "WOMPOU" ~	"WOMPO",
      country	== "MAURITANIA" &
        district	== "ADEL BAGHROU" ~	"ADEL BEGROU",
      country	== "MAURITANIA" &	district	== "AKJOUJET" ~	"AKJOUJT",
      country	== "MAURITANIA" &	district	== "BABABE" ~	"BABABÉ",
      country	== "MAURITANIA" &
        district	== "BIR OUMGREINE" ~	"BIR MOGHREN",
      country	== "MAURITANIA" &	district	== "BOGHE" ~	"BOGHÉ",
      country	== "MAURITANIA" &	district	== "BARKEOL" ~	"BARKÉOLE",
      country	== "MAURITANIA" &	district	== "CHINGUITTI" ~	"CHINGUITTY",
      country	== "MAURITANIA" &	district	== "D_HAR" ~	"D'HAR",
      country	== "MAURITANIA" &	district	== "BOUTILIMITT" ~	"BOUTILIMIT",
      country	== "MAURITANIA" &	district	== "F_DERIK" ~	"F'DERICK",
      country	== "MAURITANIA" &	district	== "GUERROU" ~	"GUÉRROU",
      country	== "MAURITANIA" &	district	== "KANKOUSSA" ~	"KANKOSSA",
      country	== "MAURITANIA" &	district	== "KOBENNI" ~	"KOBENI",
      country	== "MAURITANIA" &	district	== "M_BAGNE" ~	"M'BAGNE",
      country	== "MAURITANIA" &	district	== "M_BOUT" ~	"M'BOUT",
      country	== "MAURITANIA" &
        district	== "MAGHTA LEHJAR" ~	"MAGTA LAHJAR",
      country	== "MAURITANIA" &	district	== "MOUDJRIA" ~	"MOUDJÉRIA",
      country	== "MAURITANIA" &	district	== "NEMA" ~	"NÉMA",
      country	== "MAURITANIA" &	district	== "OUAD-NAGA" ~	"OUAD NAGA",
      country	== "MAURITANIA" &	district	== "R_KIZ" ~	"R'KIZ",
      country	== "MAURITANIA" &	district	== "SEILIBABY" ~	"SELIBABY",
      country	== "MAURITANIA" &	district	== "TAMCHEKETT" ~	"TAMCHAKET",
      country	== "MAURITANIA" &
        district	== "TEVRAGH ZEINE" ~	"TEVRAGH ZEINA",
      country	== "MAURITANIA" &	district	== "TICHITT" ~	"TICHIT",
      country	== "MAURITANIA" &	district	== "TIMBEDRA" ~	"TIMBÉDRA",
      country	== "MAURITANIA" &	district	== "ZOUERATE" ~	"ZOUÉRAT",
      country	== "MOZAMBIQUE" &	district	== "CHIÃšRE" ~	"CHIÚRE",
      country	== "MOZAMBIQUE" &	district	== "MARÃVIA" ~	"MARÁVIA",
      country	== "MOZAMBIQUE" &	district	== "MAÃšA" ~	"MAUA",
      country	== "MOZAMBIQUE" &
        district	== "ALTO MOLÃ“CUÃˆ" ~	"ALTO MOLOCUE",
      country	== "MOZAMBIQUE" &	district	== "ANGÃ“NIA" ~	"ANGONIA",
      country	== "MOZAMBIQUE" &
        district	== "MOCÃMBOA DA PRAIA" ~	"MACIMBOA DA PRAI",
      country	== "MOZAMBIQUE" &	district	== "MÃGOÃˆ" ~	"MÁGOÈ",
      country	== "MOZAMBIQUE" &	district	== "GURUÃ‰" ~	"GURUE",
      country	== "MOZAMBIQUE" &	district	== "GILÃ‰" ~	"GILÉ",
      country	== "MOZAMBIQUE" &	district	== "NGAÃšMA" ~	"NGAÚMA",
      country	== "ALGERIA" & district	== "EPSP ADRAR" ~	"Adrar",
      country	== "ALGERIA" & district	== "EPSP AOULEF" ~	"Aoulef",
      country	== "ALGERIA" &
        district	== "EPSP BADJI MOKHTAR" ~	"Bordj Badji Mokhtar",
      country	== "ALGERIA" & district	== "EPSP REGGANE" ~	"Reggana",
      country	== "ALGERIA" & district	== "EPSP TIMIMOUN" ~	"Timimmoun",
      country	== "ALGERIA" & district	== "EPSP TINERKOUK" ~	"Tinerkouk",
      country	== "ALGERIA" & district	== "EPSP ABADLA" ~	"Abadla",
      country	== "ALGERIA" & district	== "EPSP BECHAR" ~	"Bechar",
      country	== "ALGERIA" &
        district	== "EPSP BENI ABBES" ~	"Benni Abbes",
      country	== "ALGERIA" &
        district	== "EPSP BENI OUNIF" ~	"Beni Ounif",
      country	== "ALGERIA" & district	== "EPSP KERZAZ" ~	"Kerzaz",
      country	== "ALGERIA" & district	== "EPSP KERZAZ" ~	"Kerzaz",
      country	== "ALGERIA" & district	== "EPSP TABELBALA" ~	"Tabelbala",
      country	== "ALGERIA" & district	== "EPSP TAGHIT" ~	"Taghit",
      country	== "ALGERIA" & district	== "EPSP BREZINA" ~	"Breizina",
      country	== "ALGERIA" & district	== "EPSP CHELLALA" ~	"Chellala",
      country	== "ALGERIA" & district	== "EPSP EL BAYADH" ~	"El Baydh",
      country	== "ALGERIA" & district	== "EPSP KHEITER" ~	"Kheiter",
      country	== "ALGERIA" & district	== "EPSP DEBILA" ~	"Debila",
      country	== "ALGERIA" & district	== "EPSP DEBILA" ~	"Debila",
      country	== "ALGERIA" & district	== "EPSP DJEMAA" ~	"Djemaa",
      country	== "ALGERIA" &
        district	== "EPSP EL MEGHAIER" ~	"El Meghaeir",
      country	== "ALGERIA" &
        district	== "EPSP EL MEGHAIER" ~	"El Meghaeir",
      country	== "ALGERIA" & district	== "EPSP EL OUED" ~	"El Oued",
      country	== "ALGERIA" & district	== "EPSP EL OUED" ~	"El Oued",
      country	== "ALGERIA" & district	== "EPSP GUEMAR" ~	"Guemar",
      country	== "ALGERIA" & district	== "EPSP GUEMAR" ~	"Guemar",
      country	== "ALGERIA" &
        district	== "EPSP TALEB LARBI" ~	"Taleb Arby",
      country	== "ALGERIA" &
        district	== "EPSP TALEB LARBI" ~	"Taleb Arby",
      country	== "ALGERIA" & district	== "EPSP BERIANE" ~	"Berriane",
      country	== "ALGERIA" & district	== "EPSP EL MENEA" ~	"El Menea",
      country	== "ALGERIA" & district	== "EPSP GUERRARA" ~	"Guerrara",
      country	== "ALGERIA" & district	== "EPSP METLILI" ~	"Metlili",
      country	== "ALGERIA" &
        district	== "EPSP BORDJ OMAR IDRISS" ~	"Borj Omar Idriss",
      country	== "ALGERIA" &
        district	== "EPSP BORDJ-EL-HAOUESS" ~	"Borj El Haoues",
      country	== "ALGERIA" & district	== "EPSP DEBDEB" ~	"Deb Deb",
      country	== "ALGERIA" & district	== "EPSP DJANET" ~	"Djanet",
      country	== "ALGERIA" & district	== "EPSP ILLIZI" ~	"Illizi",
      country	== "ALGERIA" & district	== "EPSP IN AMENAS" ~	"In Amenas",
      country	== "ALGERIA" & district	== "EPSP AIN SEFRA" ~	"Ain Sefra",
      country	== "ALGERIA" & district	== "EPSP MECHERIA" ~	"Mecheria",
      country	== "ALGERIA" &
        district	== "EPSP MEKMEN BENAMER" ~	"Mekmen benamer",
      country	== "ALGERIA" & district	== "EPSP NAAMA" ~	"Naama",
      country	== "ALGERIA" & district	== "EPSP EL BORMA" ~	"El Borma",
      country	== "ALGERIA" &
        district	== "EPSP EL HADJIRA" ~	"El Hadjira",
      country	== "ALGERIA" &
        district	== "EPSP HASSI MESSAOUD" ~	"Hassi Messaoud",
      country	== "ALGERIA" & district	== "EPSP OUARGLA" ~	"Ouargla",
      country	== "ALGERIA" & district	== "EPSP TOUGGOURT" ~	"Touggourt",
      country	== "ALGERIA" &
        district	== "EPSP ABALESSA (SILET)" ~	"Abalessa",
      country	== "ALGERIA" &
        district	== "EPSP IN GUEZZAM" ~	"In Guezzam",
      country	== "ALGERIA" & district	== "EPSP IN MGUEL" ~	"In Amgueul",
      country	== "ALGERIA" & district	== "EPSP IN SALAH" ~	"In Salah",
      country	== "ALGERIA" &
        district	== "EPSP TAMENRASSET" ~	"Tamanrasset",
      country	== "ALGERIA" & district	== "EPSP TAZROUK" ~	"Tazrouk",
      country	== "ALGERIA" &
        district	== "EPSP TIN ZAOUATINE" ~	"Tin Zaouatine",
      country	== "ALGERIA" &
        district	== "EPSP OUM EL ASSEL" ~	"Oum El Assel",
      country	== "ALGERIA" & district	== "EPSP TINDOUF" ~	"Tindouf",
      country	== "ETHIOPIA" &	district	== "Abiy Adi" ~	"Abi Adi Town",
      country	== "ETHIOPIA" &	district	== "Adet" ~	"Naeder Adet",
      country	== "ETHIOPIA" &	district	== "Adwa Town" ~	"Adwa Town",
      country	== "ETHIOPIA" &	district	== "Adwa Zuria" ~	"Adwa",
      country	== "ETHIOPIA" &	district	== "Ahiferom" ~	"Aheferom",
      country	== "ETHIOPIA" &	district	== "Axum Town" ~	"Axum Town",
      country	== "ETHIOPIA" &
        district	== "Laelay Maichew" ~	"Laelay Maychew",
      country	== "ETHIOPIA" &
        district	== "Tahitay Maichew" ~	"Tahtay Mayechew",
      country	== "ETHIOPIA" &
        district	== "Tankua Milash" ~	"Tanqua Abergele",
      country	== "ETHIOPIA" &	district	== "Adigrat" ~	"Adigrat Town",
      country	== "ETHIOPIA" &	district	== "Atsbi" ~	"Atsbi Wenberta",
      country	== "ETHIOPIA" &
        district	== "Ganta Afeshum" ~	"Ganta Afeshum",
      country	== "ETHIOPIA" &	district	== "Gulomekeda" ~	"Gulo Mekeda",
      country	== "ETHIOPIA" &	district	== "Hawzen" ~	"Hawzen",
      country	== "ETHIOPIA" &
        district	== "Kilte Awlaelo" ~	"Kelete Awelallo",
      country	== "ETHIOPIA" &
        district	== "Tsaeda Emba" ~	"Saesie Tsaedamba",
      country	== "ETHIOPIA" &	district	== "Wukro" ~	"Wukro Town",
      country	== "ETHIOPIA" &	district	== "Adi Haki" ~	"Adhaki",
      country	== "ETHIOPIA" &	district	== "Ayder" ~	"Ayder",
      country	== "ETHIOPIA" &	district	== "Hawolti" ~	"Hawelti",
      country	== "ETHIOPIA" &	district	== "Quiha" ~	"Kuha",
      country	== "ETHIOPIA" &	district	== "Asgede" ~	"Tsegede (Tigray)",
      country	== "ETHIOPIA" &
        district	== "Seyemti Adiyabo" ~	"Laelay Adiabo",
      country	== "ETHIOPIA" &	district	== "Shire Town" ~	"Sheraro Town",
      country	== "ETHIOPIA" &
        district	== "Tahitay Koraro" ~	"Tahtay Koraro",
      country	== "ETHIOPIA" &	district	== "Tsimbla" ~	"Asgede Tsimbila",
      country	== "ETHIOPIA" &	district	== "Degua Temben" ~	"Dega Temben",
      country	== "ETHIOPIA" &	district	== "Enderta" ~	"Enderta",
      country	== "ETHIOPIA" &	district	== "Samre" ~	"Saharti Samre",
      country	== "ETHIOPIA" &	district	== "Endamekoni" ~	"Endamehoni",
      country	== "ETHIOPIA" &	district	== "Maichew Town" ~	"Maychew Town",
      country	== "ETHIOPIA" &	district	== "Raya Azebo" ~	"Raya Azebo",
      country	== "CAMEROON" &	district	== "MOZOGO" ~	"MOZONGO",
      country	== "CAMEROON" &
        district	== "CITE DES PALMIERS" ~	"CITE PALMIERS",
      country	== "GUINEA" &	district	== "NZEREKORE" ~	"N'ZÉRÉKORÉ",
      country	== "GUINEA" &	district	== "N'ZÃ‰RÃ‰KORÃ‰" ~	"N'ZÉRÉKORÉ",
      country	== "COTE D IVOIRE" &
        district	== "ADJAME_PLATEAU_ATTECOUBE" ~	"ADJAME-PLATEAU-ATTECOUBE",
      country	== "COTE D IVOIRE" &
        district	== "TREICHVILLE_MARCORY" ~	"TREICHVILLE-MARCORY",
      country	== "COTE D IVOIRE" &
        district	== "PORT-BOUET-VRIDI" ~	"PORT BOUET-VRIDI",
      country	== "COTE D IVOIRE" &
        district	== "BOUAKE-SUD" ~	"BOUAKE SUD",
      country	== "COTE D IVOIRE" &	district	== "SAN-PEDRO" ~	"SAN PEDRO",
      country	== "COTE D IVOIRE" &
        district	== "YOPOUGON-EST" ~	"YOPOUGON EST",
      country	== "COTE D IVOIRE" &
        district	== "YOPOUGON-OUEST SONGON" ~	"YOPOUGON OUEST-SONGON",
      country	== "COTE D IVOIRE" &
        district	== "KOUASSI KOUASSIKRO" ~	"KOUASSI-KOUASSIKRO",
      country	== "COTE D IVOIRE" &
        district	== "COCODY BINGERVILLE" ~	"COCODY-BINGERVILLE",
      country	== "COTE D IVOIRE" &	district	== "GAGNOA1" ~	"GAGNOA 1",
      country	== "COTE D IVOIRE" &	district	== "M'BENGUE" ~	"MBENGUE",
      country	== "COTE D IVOIRE" &
        district	== "BOUAKE-SUD" ~	"BOUAKE SUD",
      country	== "COTE D IVOIRE" &	district	== "GAGNOA2" ~	"GAGNOA 2",
      country	== "COTE D IVOIRE" &
        district	== "GRAND_LAHOU" ~	"GRAND-LAHOU",
      country	== "COTE D IVOIRE" &
        district	== "YAKASSE_ATTOBROU" ~	"YAKASSE-ATTOBROU",
      country	== "COTE D IVOIRE" &
        district	== "KOUASSI KOUASSIKRO" ~	"KOUASSI-KOUASSIKRO",
      country	== "COTE D IVOIRE" &
        district	== "GRAND_BASSAM" ~	"GRAND-BASSAM",
      country	== "COTE D IVOIRE" &
        district	== "ZOUAN_HOUNIEN" ~	"ZOUAN-HOUNIEN",
      country	== "COTE D IVOIRE" &
        district	== "YOPOUGON-EST" ~	"YOPOUGON EST",
      country	== "COTE D IVOIRE" &
        district	== "YOPOUGON-OUEST SONGON" ~	"YOPOUGON OUEST-SONGON",
      country	== "COTE D IVOIRE" &
        district	== "COCODY BINGERVILLE" ~	"COCODY-BINGERVILLE",
      country	== "COTE D IVOIRE" &
        district	== "PORT-BOUET-VRIDI" ~	"PORT BOUET-VRIDI",
      country	== "COTE D IVOIRE" &	district	== "SAN-PEDRO" ~	"SAN PEDRO",
      country == "COTE D IVOIRE" &
        district == "SAN-PEDRO" ~ "SAN PEDRO",
      country	 == "GUINEA" &	district	 == "NZEREKORE"	 ~ "N'ZEREKORE",
      country	== "GUINEA" &	district	== "DUBREKA" ~	"DUBRÉKA",
      country	== "GUINEA" &	district	== "GUECKEDOU" ~	"GUECKÉDOU",
      country	== "GUINEA" &	district	== "LELOUMA" ~	"LÉLOUMA",
      country	== "GUINEA" &	district	== "BOKE" ~	"BOKÉ",
      country	== "GUINEA" &	district	== "KEROUANE" ~	"KÉROUANE",
      country	== "GUINEA" &	district	== "FORECARIAH" ~	"FORÉCARIAH",
      country	== "GUINEA" &	district	== "TELIMELE" ~	"TÉLIMÉLÉ",
      country	== "GUINEA" &	district	== "LABE" ~	"LABÉ",
      country	== "GUINEA" &	district	== "N'ZEREKORE" ~	"N'ZÉRÉKORÉ",
      country	== "GUINEA" &	district	== "NZEREKORE" ~	"N'ZÉRÉKORÉ",
      country	== "GUINEA" &	district	== "TOUGUE" ~	"TOUGUÉ",
      country == "ANGOLA" &
        district == "TCHIKALA TCHOLOHANG" ~ "TCHIKALA TCHOLOHANGA",
      country == "ANGOLA" &
        district == "NGOLA KILUANGE" ~ "NGOLA QUILUANGE",
      country == "ANGOLA" &
        district == "CACULAMA" ~ "CACULAMA (MUCARI)",
      country == "ANGOLA" & district == "BUNDAS" ~ "BUENGAS",
      country == "ANGOLA" &
        district == "LUMBALA NGUIMBO" ~ "LUMBALA NGUIMBO (BUNDAS)",
      country == "BENIN" & district == "KARIMAMA" ~ "KARMAMA",
      country == "BENIN" & district == "BOUKOUMBE" ~ "BOUKOMBE",
      country == "BENIN" & district == "DASSA" ~ "DASSA-ZOUNME",
      country == "BENIN" & district == "DJAKOTOMEY" ~ "DJAKOTOME",
      country == "BENIN" & district == "ZA KPOTA" ~ "ZA-KPOTA",
      country == "BENIN" & district == "PORTO-NOVO 1" ~ "PORTO-NOVO 1",
      country == "BENIN" & district == "Cotonou I" ~ "COTONOU 1",
      country == "BENIN" &
        district == "Abomey-Calavi 1" ~ "ABOMEY-CALAVI 1",
      country	== "BENIN" &	district	== "GODOMEY" ~	"ABOMEY-CALAVI 1",
      country	== "BENIN" &	district	== "COTONOU I" ~	"COTONOU 1",
      country	== "BENIN" &	district	== "COTONOU II" ~	"COTONOU 2",
      country	== "BENIN" &	district	== "COTONOU III" ~	"COTONOU 3",
      country	== "BENIN" &	district	== "COTONOU IV" ~	"COTONOU 4",
      country	== "BENIN" &	district	== "COTONOU V" ~	"COTONOU 5",
      country	== "BENIN" &	district	== "COTONOU VI" ~	"COTONOU 6",
      country	== "BENIN" &	district	== "SEME-PODJI" ~	"SEME-KPODJI",
      country	== "BENIN" &	district	== "PORTO-NOVO" ~	"PORTO-NOVO 1",
      country == "BURKINA FASO" &
        district == "NONGR-MASSOM" ~ "NONGR MASSOM",
      country == "BURKINA FASO" &
        district == "SIGH-NOGHIN" ~ "SIG NOGHIN",
      country == "BURKINA FASO" & district == "NDOROLA" ~ "N'DOROLA",
      country == "BURKINA FASO" & district == "PO" ~ "PÔ",
      country == "BURUNDI" & district == "GATERANYI" ~ "GITERANYI",
      country == "BOTSWANA" & district == "GHANZI" ~ "GANTSI",
      country == "BOTSWANA" &
        district == "GREATER FRANCISTOWN" ~ "FRANCISTOWN",
      country == "BOTSWANA" & district == "KANYE" ~ "KANYE/MOSHUPA",
      country == "BOTSWANA" &
        district == "KGALAGADI NORTH" ~ "KGALAGADI",
      country == "BOTSWANA" & district == "SEROWE" ~ "SEROWE/PALAPYE",
      country	== "BOTSWANA" &	district	== "CHARLESHILL" ~	"GANTSI",
      country	== "BOTSWANA" &	district	== "KGALAGADI SOUTH" ~	"KGALAGADI",
      country	== "BOTSWANA" &	district	== "MOSHUPA" ~	"KANYE/MOSHUPA",
      country	== "BOTSWANA" &	district	== "PALAPYE" ~	"SEROWE/PALAPYE",
      country	 == "CHAD" &	district	 == "BA-ILLI"	 ~ "BA ILLI",
      country	 == "CHAD" &	district	 == "PONT_CAROL"	 ~ "PONT CAROL",
      country	 == "CHAD" &
        district	 == "HARAZE_MANGUEIGNE"	 ~ "HARAZE MANGUEIGNE",
      country	 == "CHAD" &	district	 == "AM_TIMAN"	 ~ "AM TIMAN",
      country	 == "CHAD" &
        district	 == "NDJAMENA_9AR"	 ~ "N'DJAMENA SUD",
      country	 == "CHAD" &	district	 == "BA_ILLI"	 ~ "BA ILLI",
      country	 == "CHAD" &	district	 == "OUM_HADJER"	 ~ "OUM HADJER",
      country	 == "CHAD" &	district	 == "RIG_RIG"	 ~ "RIG RIG",
      country	== "CHAD" &
        district	== "N'DJAMENA-CENTRE" ~	"N'DJAMENA CENTRE",
      country	== "CHAD" &	district	== "N'DJAMENA-SUD" ~	"N'DJAMENA SUD",
      country	== "CHAD" &	district	== "N'DJAMENA-EST" ~	"N'DJAMENA EST",
      country	== "CHAD" &	district	== "N'DJAMENA-NORD" ~	"N'DJAMENA NORD",
      country	== "CHAD" &	district	== "HADJER-HADID" ~	"OUM HADJER",
      country	== "CHAD" &	district	== "TINE" ~	"BILTINE",
      country	== "CHAD" &	district	== "MICHEMERE" ~	"MICHEMIRE",
      country	== "CHAD" &	district	== "MOUNDOU EST" ~	"MOUNDOU",
      country	== "CHAD" &	district	== "MOUNDOU CENTRE" ~	"MOUNDOU",
      country	== "CHAD" &	district	== "NDJAMENA-NORD" ~	"N'DJAMENA NORD",
      country	== "CHAD" &	district	== "GOUNOUGAYA" ~	"GOUNOU GAYA",
      country	== "CHAD" &	district	== "NDJAMENA-SUD" ~	"N'DJAMENA SUD",
      country	== "CHAD" &	district	== "BARDAÏ" ~	"BARDAI",
      country	== "CHAD" &	district	== "MOUNDOU OUEST" ~	"MOUNDOU",
      country	== "CHAD" &
        district	== "9E ARRONDISSEMENT" ~	"N'DJAMENA SUD",
      country	== "CHAD" &
        district	== "NDJAMENA-CENTRE" ~	"N'DJAMENA CENTRE",
      country	== "CHAD" &	district	== "NDJAMENA-EST" ~	"N'DJAMENA EST",
      country	 == "CHAD" &	district	 == "BIOBE"	 ~ "BIOBE SINGAKO",
      country	 == "CHAD" &	district	 == "GOZ_BEIDA"	 ~ "GOZ BEIDA",
      country	 == "CHAD" &	district	 == "KOUKOU"	 ~ "KOUKOU ANGARANA",
      country	 == "CHAD" &	district	 == "NOUKOU"	 ~ "NOKOU",
      country	 == "CHAD" &	district	 == "NTIONA"	 ~ "N'TIONA",
      country	 == "CHAD" &	district	 == "RIG-RIG"	 ~ "RIG RIG",
      country	 == "CHAD" &	district	 == "GUELO"	 ~ "GUELAO",
      country	 == "CHAD" &
        district	 == "NDJAMENA_CENTRE"	 ~ "N'DJAMENA CENTRE",
      country	 == "CHAD" &
        district	 == "NDJAMENA_EST"	 ~ "N'DJAMENA EST",
      country	 == "CHAD" &	district	 == "BEBIDJA"	 ~ "BEBEDJIA",
      country	 == "CHAD" &
        district	 == "NDJAMENA_NORD"	 ~ "N'DJAMENA NORD",
      country	 == "CHAD" &
        district	 == "NDJAMENA_SUD"	 ~ "N'DJAMENA SUD",
      country	 == "CHAD" &	district	 == "AM-TIMAN"	 ~ "AM TIMAN",
      country	 == "CHAD" &	district	 == "BAKTCHORO"	 ~ "BAKCTCHORO",
      country	 == "CHAD" &	district	 == "BAGA SOLA"	 ~ "BAGASSOLA",
      country	 == "CONGO" &	district	 == "GOMA TSE-TSE"	 ~ "GOMA TSETSE",
      country	 == "CONGO" &
        district	 == "KINKALA BOKO"	 ~ "KINKALA-BOKO",
      country	 == "CONGO" &	district	 == "SEMBE"	 ~ "SEMBE SOUANKE",
      country	 == "CONGO" &
        district	 == "MADINGO-NKAYES"	 ~ "MADINGO KAYES-ZAMBI",
      country	 == "CONGO" &
        district	 == "KIMONGO"	 ~ "KIMONGO LONDELA KAYES",
      country	 == "CONGO" &
        district	 == "IGNIE NGABE"	 ~ "IGNIE NGABE MAYAMA",
      country	 == "CONGO" &	district	 == "LOUTÉTÉ"	 ~ "LOUTETE",
      country	 == "CONGO" &	district	 == "MAKÉLÉKÉLÉ"	 ~ "MAKELEKELE",
      country	 == "CONGO" &	district	 == "OUENZÉ"	 ~ "OUENZE",
      country	 == "CONGO" &	district	 == "TIÉ-TIÉ"	 ~ "TIE-TIE",
      country	 == "CONGO" &	district	 == "GOMA TSÉTSÉ"	 ~ "GOMA TSETSE",
      country	 == "CONGO" &	district	 == "KINTÉLÉ"	 ~ "KINTELE",
      country	 == "LIBERIA" &
        district	== "BUCHANAN district" ~	"BUCHANAN",
      country	== "LIBERIA" &
        district	== "GOLA KONNEH district" ~	"GOLAKONNEH",
      country	== "LIBERIA" &	district	== "PORKPA district" ~	"PORKPA",
      country	== "LIBERIA" &
        district	== "BARCLAYVILLE district" ~	"BARCLAYVILLE",
      country	== "LIBERIA" &	district	== "SALAYEA district" ~	"SALAYEA",
      country	== "LIBERIA" &
        district	== "VOINJAMA district" ~	"VOINJAMA",
      country	== "LIBERIA" &	district	== "ZORZOR district" ~	"ZORZOR",
      country	== "LIBERIA" &	district	== "KAKATA district" ~	"KAKATA",
      country	== "LIBERIA" &	district	== "BUSHROD district" ~	"BUSHROD",
      country	== "LIBERIA" &
        district	== "ST PAUL RIVER district" ~	"ST. PAUL RIVER",
      country	== "LIBERIA" &	district	== "TODEE district" ~	"TODEE",
      country	== "LIBERIA" &	district	== "BOPOLU district" ~	"BOPOLU",
      country	== "LIBERIA" &	district	== "GBARMA district" ~	"GBARMA",
      country	== "LIBERIA" &	district	== "GARWULA district" ~	"GARWULA",
      country	== "LIBERIA" &	district	== "TEWOR district" ~	"TEWOR",
      country	== "LIBERIA" &	district	== "CAVALLA district" ~	"CAVALLA",
      country	== "LIBERIA" &	district	== "TCHIEN district" ~	"TCHIEN",
      country	== "LIBERIA" &	district	== "JRAOH district" ~	"JRAOH",
      country	== "LIBERIA" &	district	== "FOYA district" ~	"FOYA",
      country	== "LIBERIA" &
        district	== "MAMBAH - KABA district" ~	"MAMBAH-KABA",
      country	== "LIBERIA" &
        district	== "KARLUWAY 2 district" ~	"KARLUWAY 2",
      country	== "LIBERIA" &	district	== "PLEEBO district" ~	"PLEEBO",
      country	== "LIBERIA" &
        district	== "CAREYSBURG district" ~	"CAREYSBURG",
      country	== "LIBERIA" &
        district	== "GBEHLAY - GEH district" ~	"GBEHLAY-GEH",
      country	== "LIBERIA" &
        district	== "SANNIQUELLEH MAHN district" ~	"SANNIQUELLEH MAHN",
      country	== "LIBERIA" &
        district	== "CENTRAL C district" ~	"CENTRAL C",
      country	== "LIBERIA" &	district	== "TIMBO district" ~	"TIMBO",
      country	== "LIBERIA" &	district	== "CHEDEPO district" ~	"CHEDEPO",
      country	== "LIBERIA" &	district	== "GBEAPO district" ~	"GBEAPO",
      country	== "LIBERIA" &	district	== "BUTAW district" ~	"BUTAW",
      country	== "LIBERIA" &
        district	== "GREENVILLE district" ~	"GREENVILLE",
      country	== "LIBERIA" &	district	== "SANOYEA" ~	"SANOYEAH",
      country	== "LIBERIA" & district	== "BELLEH district" ~	"BELLEH",
      country	== "LIBERIA" &	district	== "BOKOMU district" ~	"BOKOMU",
      country	== "LIBERIA" &	district	== "KONGBA district" ~	"KONGBA",
      country	== "LIBERIA" &
        district	== "CAMPWOOD district" ~	"CAMP WOOD",
      country	== "LIBERIA" &
        district	== "district # 3 A&B" ~	"district # 3A & 3B",
      country	== "LIBERIA" &
        district	== "district # 3 C" ~	"district # 3C",
      country	== "LIBERIA" &
        district	== "OWENSGROVE district" ~	"OWENSGROVE",
      country	== "LIBERIA" &
        district	== "COMMONWEALTH - C district" ~	"COMMONWEALTH-C",
      country	== "LIBERIA" &	district	== "B-HAI district" ~	"B'HAI",
      country	== "LIBERIA" & district	== "GBAO district" ~	"GBAO",
      country	== "LIBERIA" & district	== "KONOBO district" ~	"KONOBO",
      country	== "LIBERIA" & district	== "PUTU district" ~	"PUTU",
      country	== "LIBERIA" & district	== "BUAH district" ~	"BUAH",
      country	== "LIBERIA" & district	== "DORBOR district" ~	"DORBOR",
      country	== "LIBERIA" & district	== "TREHN district" ~	"TREHN",
      country	== "LIBERIA" &
        district	== "KOLAHUN district" ~	"KOLAHUN",
      country	== "LIBERIA" & district	== "VAHUN district" ~	"VAHUN",
      country	== "LIBERIA" &
        district	== "FIRESTONE district" ~	"FIRESTONE",
      country	== "LIBERIA" & district	== "GIBI district" ~	"GIBI",
      country	== "LIBERIA" &
        district	== "BARROBO FARJAH district" ~	"BAROBO FARJAH",
      country	== "LIBERIA" &
        district	== "BARROBO WHOJAH district" ~	"BARROBO WHOJAH",
      country	== "LIBERIA" & district	== "HARPER district" ~	"HARPER",
      country	== "LIBERIA" &
        district	== "KARLUWAY 1 district" ~	"KARLUWAY 1",
      country	== "LIBERIA" &
        district	== "SACLEPEA-MAH" ~	"SACLEPEA-MAHN",
      country	== "LIBERIA" &
        district	== "YARWIN MEHNSONNOH" ~	"YARWEIN MEHNSOHNNEH",
      country	== "LIBERIA" & district	== "POTUPO district" ~	"POTUPO",
      country	== "LIBERIA" & district	== "SARBO district" ~	"SARBO",
      country	== "LIBERIA" & district	== "TIENPO district" ~	"TIENPO",
      country	== "LIBERIA" & district	== "WEBBO district" ~	"WEBBO",
      country	== "LIBERIA" &
        district	== "DOEDIAN district" ~	"DOEDIAN",
      country	== "LIBERIA" & district	== "JOE RIVER district" ~	"JOE",
      country	== "LIBERIA" & district	== "YARNIE district" ~	"YARNIE",
      country	== "LIBERIA" &
        district	== "DUGBE RIVER district" ~	"DUGBE",
      country	== "LIBERIA" &
        district	== "GBLONEE district" ~	"GBLONEE",
      country	== "LIBERIA" & district	== "JEADE district" ~	"JEADE",
      country	== "LIBERIA" & district	== "JEDEPO district" ~	"JEDEPO",
      country	== "LIBERIA" &
        district	== "KPANYAN district" ~	"KPANYAN",
      country	== "LIBERIA" &
        district	== "PYNES TOWN district" ~	"PYNES",
      country	== "LIBERIA" &
        district	== "TARJUWON district" ~	"TARJUWON",
      country	== "LIBERIA" & district	== "TARSUE district" ~	"TARSUE",
      country	== "LIBERIA" &	district	== "TAPPITA district" ~	"TAPPITA",
      country	== "LIBERIA" &	district	== "B'HAI district" ~	"B'HAI",
      country	== "LIBERIA" &
        district	== "SACLEPEA MAH district" ~	"SACLEPEA-MAHN",
      country	== "LIBERIA" &
        district	== "YARWEIN MEHNSOHNNEH district" ~	"YARWEIN MEHNSOHNNEH",
      country	== "LIBERIA" &	district	== "ZOE-GEH district" ~	"ZOE-GEH",
      country	== "LIBERIA" &
        district	== "SANIQUELLIE - MAH" ~	"SANNIQUELLEH MAHN",
      country	== "LIBERIA" &	district	== "DOEDIAN" ~	"DOEDAIN",
      country	== "LIBERIA" &	district	== "BUU-YAO" ~	"BUAH",
      country	== "LIBERIA" &
        district	== "COMMONWEALTH-C district" ~	"COMMONWEALTH-C",
      country	== "LIBERIA" &
        district	== "MAMBAH-KABA district" ~	"MAMBAH-KABA",
      country	== "LIBERIA" &
        district	== "CENTRAL MONROVIA district" ~	"CENTRAL MONROVIA",
      country	== "LIBERIA" &
        district	== "SOMALIA DRIVE district" ~	"SOMALIA DRIVE",
      country	== "LIBERIA" &	district	== "JOE" ~	"JOE RIVER",
      country	== "LIBERIA" &	district	== "JOWEIN district" ~	"JOWEIN",
      country	== "LIBERIA" &	district	== "PYNES" ~	"PYNES TOWN",
      country	== "LIBERIA" &
        district	== "COMMONWEALTH district" ~	"COMMONWEALTH",
      country	== "LIBERIA" &	district	== "DUGBE" ~	"DUGBE RIVER",
      country	== "LIBERIA" &	district	== "BAROBO FARJAH" ~	"BAROBO FAJAH",
      country	== "MALI" &	district	 == "KALABAN-CORO" ~	"KALABANCORO",
      country	== "MALI" &	district	 == "TAOUDENIT" ~	"TAOUDENI",
      country	== "MALI" &
        district	 == "district COMMUNE I" ~	"COMMUNE I",
      country	== "MALI" &
        district	 == "district COMMUNE II" ~	"COMMUNE II",
      country	== "MALI" &
        district	 == "district COMMUNE III" ~	"COMMUNE III",
      country	== "MALI" &
        district	 == "district COMMUNE IV" ~	"COMMUNE IV",
      country	== "MALI" &
        district	 == "district COMMUNE V" ~	"COMMUNE V",
      country	== "MALI" &
        district	 == "district COMMUNE VI" ~	"COMMUNE VI",
      country	== "MALI" &
        district	 == "district KOULIKORO" ~	"KOULIKORO",
      country	== "MALI" &	district	 == "district SIKASSO" ~	"SIKASSO",
      country	== "MALI" &	district	 == "district KITA" ~	"KITA",
      country	== "MALI" &	district	 == "district BANAMBA" ~	"BANAMBA",
      country	== "MALI" &	district	 == "district DIOILA" ~	"DIOILA",
      country	== "MALI" &	district	 == "district FANA" ~	"FANA",
      country	== "MALI" &
        district	 == "district KALABANCORO" ~	"KALABANCORO",
      country	== "MALI" &	district	 == "district KANGABA" ~	"KANGABA",
      country	== "MALI" &	district	 == "district KATI" ~	"KATI",
      country	== "MALI" &	district	 == "district KOLOKANI" ~	"KOLOKANI",
      country	== "MALI" &	district	 == "district NARA" ~	"NARA",
      country	== "MALI" &
        district	 == "district OUELESSEBOUGOU" ~	"OUELESSEBOUGOU",
      country	== "MALI" &	district	 == "district BOUGOUNI" ~	"BOUGOUNI",
      country	== "MALI" &	district	 == "district KADIOLO" ~	"KADIOLO",
      country	== "MALI" &	district	 == "district KIGNAN" ~	"KIGNAN",
      country	== "MALI" &
        district	 == "district KOLONDIEBA" ~	"KOLONDIEBA",
      country	== "MALI" &	district	 == "district KOUTIALA" ~	"KOUTIALA",
      country	== "MALI" &	district	 == "district NIENA" ~	"NIENA",
      country	== "MALI" &	district	 == "district SELINGUE" ~	"SELINGUE",
      country	== "MALI" &
        district	 == "district YANFOLILA" ~	"YANFOLILA",
      country	== "MALI" &	district	 == "district YOROSSO" ~	"YOROSSO",
      country	== "MALI" &	district	== "ALMOUSTRAT" ~	"ALMOUSTARAT",
      country	== "MALI" &	district	== "COMMUNE 1" ~	"COMMUNE I",
      country	== "MALI" &	district	== "COMMUNE 2" ~	"COMMUNE II",
      country	== "MALI" &	district	== "COMMUNE 3" ~	"COMMUNE III",
      country	== "MALI" &	district	== "COMMUNE 4" ~	"COMMUNE IV",
      country	== "MALI" &	district	== "COMMUNE 5" ~	"COMMUNE V",
      country	== "MALI" &	district	== "COMMUNE 6" ~	"COMMUNE VI",
      country	== "MALI" &	district	== "DJENNE" ~	"DJENNÉ",
      country	== "MALI" &	district	== "SAGABARY" ~	"SAGABARI",
      country	== "MALI" &
        district	== "OUSSOUBIDIAGNIA" ~	"OUSSOUBIDIAGNA",
      country	== "MALI" &
        district	== "district GOURMA-RHAROUS" ~	"GOURMA-RHAROUS",
      country	== "MALI" &	district	== "district TIDERMENE" ~	"TIDERMENE",
      country	== "MALI" &	district	== "district BOUREM" ~	"BOUREM",
      country	== "MALI" &	district	== "district GAO" ~	"GAO",
      country	== "MALI" &	district	== "district ABEIBARA" ~	"ABEIBARA",
      country	== "MALI" &	district	== "district MENAKA" ~	"MENAKA",
      country	== "MALI" &	district	== "district NIAFUNKE" ~	"NIAFUNKE",
      country	== "MALI" &	district	== "district INEKAR" ~	"INEKAR",
      country	== "MALI" &	district	== "district DJENNE" ~	"DJENNÉ",
      country	== "MALI" &
        district	== "district BANDIAGARA" ~	"BANDIAGARA",
      country	== "MALI" &	district	== "district DIRE" ~	"DIRE",
      country	== "MALI" &	district	== "district TESSALIT" ~	"TESSALIT",
      country	== "MALI" &
        district	== "district ANDERAMBOUKANE" ~	"ANDERAMBOUKANE",
      country	== "MALI" &	district	== "district KORO" ~	"KORO",
      country	== "MALI" &	district	== "district TENENKOU" ~	"TENENKOU",
      country	== "MALI" &	district	== "district YOUWAROU" ~	"YOUWAROU",
      country	== "MALI" &	district	== "district AL-OURCHE" ~	"AL-OURCHE",
      country	== "MALI" &	district	== "district KIDAL" ~	"KIDAL",
      country	== "MALI" &	district	== "district TINESSAKO" ~	"TINESSAKO",
      country	== "MALI" &	district	== "district ARAWANE" ~	"ARAWANE",
      country	== "MALI" &	district	== "district GOUNDAM" ~	"GOUNDAM",
      country	== "MALI" &	district	== "district BOUJBEHA" ~	"BOUJBEHA",
      country	== "MALI" &
        district	== "district ALMOUSTARAT" ~	"ALMOUSTARAT",
      country	== "MALI" &	district	== "district ANSONGO" ~	"ANSONGO",
      country	== "MALI" &	district	== "district BANKASS" ~	"BANKASS",
      country	== "MALI" &	district	== "district DOUENTZA" ~	"DOUENTZA",
      country	== "MALI" &	district	== "district MOPTI" ~	"MOPTI",
      country	== "MALI" &	district	== "district ACHOURATT" ~	"ACHOURATT",
      country	== "MALI" &	district	== "district FOUM-ALBA" ~	"FOUM-ALBA",
      country	== "MALI" &	district	== "district TAOUDENIT" ~	"TAOUDENI",
      country	== "MALI" &
        district	== "district TOMBOUCTOU" ~	"TOMBOUCTOU",
      country	== "MALI" &
        district	== "district OUSSOUBIDIAGNA" ~	"OUSSOUBIDIAGNA",
      country	== "MALI" &	district	== "district BLA" ~	"BLA",
      country	== "MALI" &	district	== "district MACINA" ~	"MACINA",
      country	== "MALI" &	district	== "district MARKALA" ~	"MARKALA",
      country	== "MALI" &	district	== "district NIONO" ~	"NIONO",
      country	== "MALI" &	district	== "district DIEMA" ~	"DIEMA",
      country	== "MALI" &	district	== "district SAGABARI" ~	"SAGABARI",
      country	== "MALI" &	district	== "district SAN" ~	"SAN",
      country	== "MALI" &	district	== "district KENIEBA" ~	"KENIEBA",
      country	== "MALI" &	district	== "district KAYES" ~	"KAYES",
      country	== "MALI" &	district	== "district SEFETO" ~	"SEFETO",
      country	== "MALI" &	district	== "district TOMINIAN" ~	"TOMINIAN",
      country	== "MALI" &	district	== "district BAFOULABE" ~	"BAFOULABE",
      country	== "MALI" &	district	== "district NIORO" ~	"NIORO",
      country	== "MALI" &	district	== "district YELIMANE" ~	"YELIMANE",
      country	== "MALI" &	district	== "district BAROUELI" ~	"BAROUELI",
      country	== "MALI" &	district	== "district SEGOU" ~	"SEGOU",
      country	== "MAURITANIA" &
        district	 == "MAGHTALAHJAR" ~	"MAGTA LAHJAR",
      country	== "MAURITANIA" &	district	 == "SELIBABI" ~	"SELIBABY",
      country	== "MAURITANIA" &
        district	 == "AIOUN AL ATROUSS" ~	"AIOUN",
      country	== "MAURITANIA" &	district	 == "KOUBENNI" ~	"KOBENI",
      country	== "MAURITANIA" &	district	 == "TICHITT" ~	"TICHIT",
      country	== "MAURITANIA" &	district	 == "F_DERICK" ~	"F'DERICK",
      country	== "MAURITANIA" &	district	 == "CHINGUITTI" ~	"CHINGUITTY",
      country	== "MAURITANIA" &	district	 == "BARKEOL" ~	"BARKÉOLE",
      country	== "MAURITANIA" &	district	 == "KANKOUSSA" ~	"KANKOSSA",
      country	== "MAURITANIA" &	district	 == "M_BAGNE" ~	"M'BAGNE",
      country	== "MAURITANIA" &
        district	 == "MAGHTA LEHJAR" ~	"MAGTA LAHJAR",
      country	== "MAURITANIA" &	district	 == "LEKSEIBE" ~	"LEXEIBA",
      country	== "MAURITANIA" &	district	 == "M_BOUT" ~	"M'BOUT",
      country	== "MAURITANIA" &	district	 == "SEILIBABY" ~	"SELIBABY",
      country	== "MAURITANIA" &	district	 == "WOMPOU" ~	"WOMPO",
      country	== "MAURITANIA" &
        district	 == "ADEL BAGHROU" ~	"ADEL BEGROU",
      country	== "MAURITANIA" &	district	 == "D_HAR" ~	"D'HAR",
      country	== "MAURITANIA" &	district	 == "KOBENNI" ~	"KOBENI",
      country	== "MAURITANIA" &	district	 == "TAMCHEKETT" ~	"TAMCHAKET",
      country	== "MAURITANIA" &	district	 == "AKJOUJET" ~	"AKJOUJT",
      country	== "MAURITANIA" &
        district	 == "TEVRAGH ZEINE" ~	"TEVRAGH ZEINA",
      country	== "MAURITANIA" &	district	 == "RIYADH" ~	"RIYAD",
      country	== "MAURITANIA" &	district	 == "MOUDJRIA" ~	"MOUDJÉRIA",
      country	== "MAURITANIA" &
        district	 == "BIR OUMGREINE" ~	"BIR MOGHREN",
      country	== "MAURITANIA" &	district	 == "F_DERIK" ~	"F'DERICK",
      country	== "MAURITANIA" &
        district	 == "BOUTILIMITT" ~	"BOUTILIMIT",
      country	== "MAURITANIA" &	district	 == "OUAD-NAGA" ~	"OUAD NAGA",
      country	== "MAURITANIA" &	district	 == "R_KIZ" ~	"R'KIZ",
      country	== "MAURITANIA" &	district	== "GUEROU" ~	"GUÉRROU",
      country	== "MAURITANIA" &	district	== "BASSEKNOU" ~	"BASSIKNOU",
      country	== "MAURITANIA" &	district	== "BARKEIWEL" ~	"BARKÉOLE",
      country	== "MAURITANIA" &
        district	== "BIRMOUGREIN" ~	"BIR MOGHREN",
      country	== "MAURITANIA" &	district	== "TIJIKJA" ~	"TIDJIKJA",
      country	== "MAURITANIA" &	district	== "BARKEOLE" ~	"BARKÉOLE",
      country	== "MAURITANIA" &	district	== "GUERROU" ~	"GUÉRROU",
      country	== "MAURITANIA" &	district	== "BABABE" ~	"BABABÉ",
      country	== "MAURITANIA" &	district	== "BOGHE" ~	"BOGHÉ",
      country	== "MAURITANIA" &	district	== "NEMA" ~	"NÉMA",
      country	== "MAURITANIA" &	district	== "TIMBEDRA" ~	"TIMBÉDRA",
      country	== "MAURITANIA" &	district	== "MOUDJERIA" ~	"MOUDJÉRIA",
      country	== "MAURITANIA" &	district	== "ZOUERAT" ~	"ZOUÉRAT",
      country	== "MAURITANIA" &	district	== "ZOUERATE" ~	"ZOUÉRAT",
      country	== "MADAGASCAR" &
        district	== "ANTANANARIVO RENIVOHITRA" ~	"ANTANANARIVO-RENIVOHITRA",
      country	== "MADAGASCAR" &
        district	== "ANKAZOABO ATSIMO" ~	"ANKAZOABO-ATSIMO",
      country	== "MADAGASCAR" &
        district	== "TOLIARA I" ~	"TOLIARA I ET II",
      country	== "MADAGASCAR" &
        district	== "MIDONGY DU SUD" ~	"MIDONGY-ATSIMO",
      country	== "MADAGASCAR" &
        district	== "IKONGO (FORT_CARNOT)" ~	"IKONGO",
      country	== "MADAGASCAR" &
        district	== "ANTANANARIVO ATSIMONDRANO" ~	"ANTANANARIVO-ATSIMONDRANO",
      country	== "MADAGASCAR" &
        district	== "FENOARIVO ATSINANANA" ~	"FENOARIVO-ATSINANANA",
      country	== "MADAGASCAR" &
        district	== "SOANIERANA IVONGO" ~	"SOANIERANA-IVONGO",
      country	== "MADAGASCAR" &
        district	== "AMBOVOMBE ANDROY" ~	"AMBOVOMBE-ANDROY",
      country	== "MADAGASCAR" &	district	== "BELOHA ANDROY" ~	"BELOHA",
      country	== "MADAGASCAR" &
        district	== "BELO SUR TSIRIBIHINA" ~	"BELO-TSIRIBIHINA",
      country	== "MADAGASCAR" &
        district	== "ANTANANARIVO AVARADRANO" ~	"ANTANANARIVO-AVARADRANO",
      country	== "MADAGASCAR" &
        district	== "MANANARA AVARATRA" ~	"MANANARA-AVARATRA",
      country	== "MADAGASCAR" &
        district	== "AMPANIHY OUEST" ~	"AMPANIHY",
      country	== "MADAGASCAR" &
        district	== "BETIOKY ATSIMO" ~	"BETIOKY-ATSIMO",
      country	== "MADAGASCAR" &
        district	== "ANTANAMBAO MANAMPONTSY" ~	"ANTANAMBAO-MANAMPOTSY",
      country	== "MADAGASCAR" &
        province	== "BONGOLAVA" &
        district	== "FENOARIVOBE" ~	"FENOARIVO-AFOVOANY",
      country	== "MADAGASCAR" &
        province	== "ANOSY" &
        district	== "AMBOASARY SUD" ~	"AMBOASARY-ATSIMO",
      country	== "MADAGASCAR" &
        district	== "TOLIARA II" ~	"TOLIARA I ET II",
      country	== "MADAGASCAR" &
        district	== "AMBATOBOENY" ~	"AMBATO-BOINA",
      country	== "MADAGASCAR" &
        district	== "ANOSIBE AN ALA" ~	"ANOSIBE AN-ALA",
      country	== "MADAGASCAR" &
        district	== "ANOSIBE AN'ALA" ~	"ANOSIBE AN-ALA",
      country	== "MADAGASCAR" &
        district	== "NOSY BORAHA (SAINTE MARIE)" ~	"NOSY-BORAHA (SAINTE MARIE)",
      country	== "MADAGASCAR" &	district	== "NOSY BE" ~	"NOSY-BE",
      country	== "MADAGASCAR" &
        district	== "MANAKARA ATSIMO" ~	"MANAKARA-ATSIMO",
      country	== "MADAGASCAR" &
        district	== "VOHIMARINA (VOHÃ©MAR)" ~	"VOHIMARINA (VOHEMAR)",
      country	== "MADAGASCAR" &
        district	== "BEFANDRIANA AVARATRA" ~	"BEFANDRIANA-AVARATRA",
      country	== "MADAGASCAR" &
        district	== "BORIZINY (PORT BERGE)" ~	"BORIZINY (PORT-BERGER)",
      country	== "MADAGASCAR" &
        district	== "NOSY VARIKA" ~	"NOSY-VARIKA",
      country	== "MALAWI" &	district	== "MZIMBA NORTH" ~	"M'ZIMBA",
      country	== "MALAWI" &	district	== "MZIMBA SOUTH" ~	"M'ZIMBA",
      country	== "MOZAMBIQUE" &	district	 == "PEMBA" ~	"PEMBA-METUGE",
      country	== "MOZAMBIQUE" &
        district	 == "CHIMOIO" ~	"CIDADE DE CHIMOIO",
      country	== "MOZAMBIQUE" &
        district	 == "NACALA PORTO" ~	"NACALA PORTO",
      country	== "MOZAMBIQUE" &	district	 == "GORONGOZA" ~	"GORONGOSA",
      country	== "MOZAMBIQUE" &	district	 == "TETE" ~	"CIDADE DE TETE",
      country	== "NIGERIA" &	district	== "BURUKU" ~	"BUKURU",
      country	== "NIGERIA" &	district	== "ONUIMO" ~	"UNUIMO",
      country	== "NIGERIA" &	district	== "MUNYA" ~	"MUYA",
      country	== "NIGERIA" &	district	== "AYEDADE" ~	"AIYEDADE",
      country	== "NIGERIA" &	district	== "AYEDIRE" ~	"AIYEDIRE",
      country	== "NIGERIA" &	district	== "GIREI" ~	"GIRIE",
      country	== "NIGERIA" &	district	== "TOUNGO" ~	"TEUNGO",
      country	== "NIGERIA" &	district	== "KIRI KASAMMA" ~	"KIRI KASAMA",
      country	== "NIGERIA" &	district	== "LAMURDE" ~	"LARMURDE",
      country	== "NIGERIA" &	district	== "BIRNIWA" ~	"BIRNIN KUDU",
      country	== "NIGERIA" &	district	== "MALAM MADORI" ~	"MALAM MADURI",
      country	== "NIGERIA" &
        district	== "SULE TANKARKAR" ~	"SULE TANKAKAR",
      country	== "NIGERIA" &	district	== "KUBAU" ~	"KUBAN",
      country	== "NIGERIA" &	district	== "UNGOGO" ~	"UNGONGO",
      country	== "NIGERIA" &	district	== "WAMAKKO" ~	"WAMAKO",
      country	== "NIGERIA" &	district	== "BADE" ~	"BARDE",
      country	== "NIGERIA" &	district	== "BURSARI" ~	"BORSARI",
      country	== "NIGERIA" &	district	== "TARMUWA" ~	"TARMUA",
      country	== "NIGER" &	district	== "KANTCHE" ~	"KANTCHÉ",
      country	== "NIGER" &	district	== "TCHIROZERINE" ~	"TCHIROZÉRINE",
      country	== "NIGER" &	district	== "IFEROUANE" ~	"IFÉROUANE",
      country	== "NIGER" &
        district	== "DAMAGARAM TAKAYYA" ~	"DAMAGARAM TAKAYA",
      country	== "NIGER" &	district	== "NGOURTI" ~	"N'GOURTI",
      country	== "NIGER" &	district	== "DOGONDOUTCHI" ~	"DOGON DOUTCHI",
      country	== "NIGER" &	district	== "GUIDAN ROUMDJI" ~	"G. ROUMDJI",
      country	== "NIGER" &	district	== "TAHOUA DEPT" ~	"TAHOUA DEP",
      country	== "NIGER" &	district	== "TILLABERY" ~	"TILLABERI",
      country	== "NIGER" &	district	== "TAKIETA" ~	"TAKEITA",
      country	== "NIGER" &	district	== "TARKA (BELBEJI)" ~	"BELBEDJI",
      country	== "NIGER" &	district	== "ZINDER VILLE" ~	"ZINDER",
      country	== "CENTRAL AFRICAN REPUBLIC" &
        district	== "NANGHA-BOGUILA" ~	"NANGA-BOGUILA",
      country	== "CENTRAL AFRICAN REPUBLIC" &
        district	== "NANA-GRIBIZI" ~	"NANA-GREBIZI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KWILU NGONGO" ~	"KWILU-NGONGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KIMBAO" ~	"KIMBAU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "WAMBA LWADI" ~	"WAMBA LUADI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "NIANIA" ~	"NIA-NIA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BENATSHIADI" ~	"BENA-TSHIADI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "SHABUNDA" ~	"SHABUNDA CENTRE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MUFUNGA SAMPWE" ~	"MUFUNGA-SAMPWE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BENALEKA" ~	"BENA-LEKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "LUBONDAIE" ~	"LUBONDAYI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KASAVUBU" ~	"KASA-VUBU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MONT NGAFULA I" ~	"MONT-NGAFULA I",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MONT NGAFULA II" ~	"MONT-NGAFULA II",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KASONGO LUNDA" ~	"KASONGOLUNDA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MASIMANIMBA" ~	"MASI-MANIMBA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "YASA BONGA" ~	"YASA-BONGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BANDJOW MOKE" ~	"BANJOW MOKE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "SARAMABILA" ~	"SALAMABILA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MOBAYI-MBONGO" ~	"MOBAYI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BENA DIBELE" ~	"BENA-DIBELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "DJALO NDJEKA" ~	"DJALO-NDJEKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KATAKO KOMBE" ~	"KATAKO-KOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "TSHUDI LOTO" ~	"TSHUDI-LOTO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "WEMBO NYAMA" ~	"WEMBO-NYAMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BAGIRA" ~	"BAGIRA-KASHA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KIMBI-LULENGE" ~	"KIMBI LULENGE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MITI-MURHESA" ~	"MITI-MURRHESA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BANGA BOLA" ~	"BANGABOLA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "WANIE RUKULA" ~	"WANIE-RUKULA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KALAMBAYI" ~	"KALAMBAYI KABANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KALONDA EST" ~	"KALONDA-EST",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "LUDIMBI LUKULA" ~	"LUDIMBI-LUKULA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "RUASHI" ~	"RWASHI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BAMBU" ~	"BAMBU-MINES",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BINZA METEO" ~	"BINZA-METEO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BINZA OZONE" ~	"BINZA-OZONE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "NGIRI NGIRI" ~	"NGIRI-NGIRI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BOKO KIVULU" ~	"BOKO-KIVULU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "GOMBE MATADI" ~	"GOMBE-MATADI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MBANZA NGUNGU" ~	"MBANZA-NGUNGU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "NSONA MPANGU" ~	"NSONA-PANGU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "SEKE BANZA" ~	"SEKEBANZA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "SONA BATA" ~	"SONA-BATA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KIKWIT NORD" ~	"KIKWIT-NORD",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KIKWIT SUD" ~	"KIKWIT-SUD",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BANDJOW-MOKE" ~	"BANJOW MOKE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "NTANDEMBELO" ~	"NTAND EMBELO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "PENDJWA" ~	"PENDJUA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MOBAYI MBONGO" ~	"MOBAYI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "VANGA KETE" ~	"VANGA-KETE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BAGIRA KASHA" ~	"BAGIRA-KASHA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BOGOSENUBEA" ~	"BOGOSE NUBEA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MWENEDITU" ~	"MWENE DITU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BENA LEKA" ~	"BENA-LEKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BENA TSHADI" ~	"BENA-TSHIADI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MASSA" ~	"MASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KAMWESHA" ~	"KAMUESHA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "NDJOKO MPUNDA" ~	"NDJOKO PUNDA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "HAUT PLATEAU" ~	"HAUTS PLATEAUX UVIRA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MITI MURHESA" ~	"MITI-MURRHESA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MALEMBA NKULU" ~	"MALEMBA-NKULU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "GETY" ~	"GETHY",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "NIA NIA" ~	"NIA-NIA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KALONDA OUEST" ~	"KALONDA-OUEST",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KALAMU 1" ~	"KALAMU I",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KALAMU 2" ~	"KALAMU II",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KASA VUBU" ~	"KASA-VUBU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MALUKU 1" ~	"MALUKU I",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MALUKU 2" ~	"MALUKU II",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MASINA 1" ~	"MASINA I",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MASINA 2" ~	"MASINA II",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MONT NGAFULA 1" ~	"MONT-NGAFULA I",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MONT NGAFULA 2" ~	"MONT-NGAFULA II",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KISANJI" ~	"KISANDJI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "MASI MANIMBA" ~	"MASI-MANIMBA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BANZOW MOKE" ~	"BANJOW MOKE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BOSO MONDANDA" ~	"BOSOMODANDA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "KABONDO DIANDA" ~	"KABONDO-DIANDA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        district	== "BOSO MANZI" ~	"BOSOMANZI",
      country	== "SENEGAL" &	district	== "BIRKELANE" ~	"BIRKILANE",
      country	== "SENEGAL" &	district	== "MALEM HODAR" ~	"MALEM HODDAR",
      country	== "SENEGAL" &	district	== "DAROU-MOUSTY" ~	"DAROU MOUSTY",
      country	== "SENEGAL" &	district	== "KOKI" ~	"COKI",
      country	== "SENEGAL" &	district	== "SAINT-LOUIS" ~	"SAINT LOUIS",
      country	== "SENEGAL" &
        district	== "DIANKHE MAKHAN" ~	"DIANKE MAKHA",
      country	== "SENEGAL" &
        district	== "MAKACOLIBANTANG" ~	"MAKA COLIBANTANG",
      country	== "SENEGAL" &
        district	== "THIONCK-ESSYL" ~	"THIONCK ESSYL",
      country	== "SIERRA LEONE" &
        district	== "WESTERN RUR" ~	"WESTERN RURAL",
      country	== "SIERRA LEONE" &
        district	== "WESTERN URB" ~	"WESTERN URBAN",
      country	== "SOUTH SUDAN" &	district	== "KAJO-KEJI" ~	"KAJO KEJI",
      country	== "SOUTH SUDAN" &	district	== "LAPON" ~	"LAFON/LOPA",
      country	== "SOUTH SUDAN" &	district	== "BOR SOUTH" ~	"SOUTH BOR",
      country	== "SOUTH SUDAN" &
        district	== "AWEIL CENTRE" ~	"AWEIL CENTRAL",
      country	== "SOUTH SUDAN" &	district	== "RUBKONA" ~	"RUBKOANA",
      country	== "SOUTH SUDAN" &	district	== "LUAKPINY/NASIR" ~	"NASIR",
      country	== "SOUTH SUDAN" &	district	== "RAGA" ~	"RAJA",
      country	== "UNITED REPUBLIC OF TANZANIA" &
        district	== "MICHEWENI DC" ~	"MICHEWENI",
      country	== "UNITED REPUBLIC OF TANZANIA" &
        district	== "WETE DC" ~	"WETE",
      country	== "UNITED REPUBLIC OF TANZANIA" &
        district	== "KASKAZINI A DC" ~	"KASKAZINI A",
      country	== "UNITED REPUBLIC OF TANZANIA" &
        district	== "KASKAZINI B DC" ~	"KASKAZINI B",
      country	== "UNITED REPUBLIC OF TANZANIA" &
        district	== "CHAKE CHAKE DC" ~	"CHAKE CHAKE",
      country	== "UNITED REPUBLIC OF TANZANIA" &
        district	== "MKOANI DC" ~	"MKOANI",
      country	== "UNITED REPUBLIC OF TANZANIA" &
        district	== "KATI DC" ~	"KATI",
      country	== "UNITED REPUBLIC OF TANZANIA" &
        district	== "KUSINI DC" ~	"KUSINI",
      country	== "UNITED REPUBLIC OF TANZANIA" &
        district	== "MJINI DC" ~	"MJINI",
      country	== "UNITED REPUBLIC OF TANZANIA" &
        district	== "KIBITI DC" ~	"KIBITI TC",
      country	== "UNITED REPUBLIC OF TANZANIA" &
        district	== "KIGOMA UJIJI MC" ~	"KIGOMA MC",
      country	== "TOGO" &	district	== "BAS MONO" ~	"BAS-MONO",
      country	== "TOGO" &	district	== "AGOE NYIVE" ~	"AGOE",
      country	== "TOGO" &	district	== "EST MONO" ~	"EST-MONO",
      country	== "TOGO" &	district	== "MOYEN MONO" ~	"MOYEN-MONO",
      country	== "UGANDA" &	district	== "ADJUMANI district" ~	"ADJUMANI",
      country	== "UGANDA" &	district	== "ARUA CITY" ~	"ARUA",
      country	== "UGANDA" &	district	== "ARUA district" ~	"ARUA",
      country	== "UGANDA" &	district	== "KOBOKO district" ~	"KOBOKO",
      country	== "UGANDA" &
        district	== "MADI-OKOLLO district" ~	"MADI-OKOLLO",
      country	== "UGANDA" &	district	== "MARACHA district" ~	"MARACHA",
      country	== "UGANDA" &	district	== "MOYO district" ~	"MOYO",
      country	== "UGANDA" &	district	== "TEREGO" ~	"TEREGO",
      country	== "UGANDA" &	district	== "ZOMBO district" ~	"ZOMBO",
      country	== "UGANDA" &	district	== "BUIKWE district" ~	"BUIKWE",
      country	== "UGANDA" &
        district	== "BUTAMBALA district" ~	"BUTAMBALA",
      country	== "UGANDA" &	district	== "BUVUMA district" ~	"BUVUMA",
      country	== "UGANDA" &	district	== "GOMBA district" ~	"GOMBA",
      country	== "UGANDA" &	district	== "KAYUNGA district" ~	"KAYUNGA",
      country	== "UGANDA" &	district	== "AMURU district" ~	"AMURU",
      country	== "UGANDA" &	district	== "GULU CITY" ~	"GULU",
      country	== "UGANDA" &	district	== "KITGUM district" ~	"KITGUM",
      country	== "UGANDA" &	district	== "LAMWO district" ~	"LAMWO",
      country	== "UGANDA" &	district	== "NWOYA district" ~	"NWOYA",
      country	== "UGANDA" &	district	== "BULIISA district" ~	"BULIISA",
      country	== "UGANDA" &	district	== "HOIMA CITY" ~	"HOIMA",
      country	== "UGANDA" &	district	== "HOIMA district" ~	"HOIMA",
      country	== "UGANDA" &	district	== "KAKUMIRO district" ~	"KAKUMIRO",
      country	== "UGANDA" &	district	== "KIKUUBE district" ~	"KIKUUBE",
      country	== "UGANDA" &	district	== "MASINDI district" ~	"MASINDI",
      country	== "UGANDA" &	district	== "BUGWERI district" ~	"BUGWERI",
      country	== "UGANDA" &	district	== "BUYENDE district" ~	"BUYENDE",
      country	== "UGANDA" &	district	== "IGANGA district" ~	"IGANGA",
      country	== "UGANDA" &	district	== "JINJA CITY" ~	"JINJA",
      country	== "UGANDA" &	district	== "JINJA district" ~	"JINJA",
      country	== "UGANDA" &	district	== "KAMULI district" ~	"KAMULI",
      country	== "UGANDA" &	district	== "LUUKA district" ~	"LUUKA",
      country	== "UGANDA" &
        district	== "NAMAYINGO district" ~	"NAMAYINGO",
      country	== "UGANDA" &
        district	== "NAMUTUMBA district" ~	"NAMUTUMBA",
      country	== "UGANDA" &	district	== "KABALE district" ~	"KABALE",
      country	== "UGANDA" &	district	== "KANUNGU district" ~	"KANUNGU",
      country	== "UGANDA" &	district	== "KISORO district" ~	"KISORO",
      country	== "UGANDA" &	district	== "RUBANDA district" ~	"RUBANDA",
      country	== "UGANDA" &
        district	== "RUKUNGIRI district" ~	"RUKUNGIRI",
      country	== "UGANDA" &
        district	== "BUNDIBUGYO district" ~	"BUNDIBUGYO",
      country	== "UGANDA" &
        district	== "BUNYANGABU district" ~	"BUNYANGABU",
      country	== "UGANDA" &
        district	== "FORT PORTAL CITY" ~	"FORT PORTAL",
      country	== "UGANDA" &	district	== "KABAROLE district" ~	"KABAROLE",
      country	== "UGANDA" &	district	== "KASESE district" ~	"KASESE",
      country	== "UGANDA" &
        district	== "KITAGWENDA district" ~	"KITAGWENDA",
      country	== "UGANDA" &	district	== "KYEGEGWA district" ~	"KYEGEGWA",
      country	== "UGANDA" &	district	== "KYENJOJO district" ~	"KYENJOJO",
      country	== "UGANDA" &	district	== "NTOROKO district" ~	"NTOROKO",
      country	== "UGANDA" &	district	== "ALEBTONG district" ~	"ALEBTONG",
      country	== "UGANDA" &	district	== "APAC district" ~	"APAC",
      country	== "UGANDA" &	district	== "KOLE district" ~	"KOLE",
      country	== "UGANDA" &	district	== "KWANIA district" ~	"KWANIA",
      country	== "UGANDA" &	district	== "LIRA CITY" ~	"LIRA",
      country	== "UGANDA" &	district	== "LIRA district" ~	"LIRA",
      country	== "UGANDA" &	district	== "OTUKE district" ~	"OTUKE",
      country	== "UGANDA" &
        district	== "BUKOMANSIMBI district" ~	"BUKOMANSIMBI",
      country	== "UGANDA" &
        district	== "KALANGALA district" ~	"KALANGALA",
      country	== "UGANDA" &	district	== "KYOTERA district" ~	"KYOTERA",
      country	== "UGANDA" &
        district	== "LYANTONDE district" ~	"LYANTONDE",
      country	== "UGANDA" &	district	== "MASAKA CITY" ~	"MASAKA",
      country	== "UGANDA" &	district	== "MASAKA district" ~	"MASAKA",
      country	== "UGANDA" &	district	== "SSEMBABAULE" ~	"SSEMBABAULE",
      country	== "UGANDA" &	district	== "BUDUDA district" ~	"BUDUDA",
      country	== "UGANDA" &
        district	== "BULAMBULI district" ~	"BULAMBULI",
      country	== "UGANDA" &	district	== "BUTEBO district" ~	"BUTEBO",
      country	== "UGANDA" &
        district	== "KAPCHORWA district" ~	"KAPCHORWA",
      country	== "UGANDA" &	district	== "KIBUKU district" ~	"KIBUKU",
      country	== "UGANDA" &	district	== "MBALE CITY" ~	"MBALE",
      country	== "UGANDA" &	district	== "MBALE district" ~	"MBALE",
      country	== "UGANDA" &	district	== "TORORO district" ~	"TORORO",
      country	== "UGANDA" &	district	== "BUHWEJU district" ~	"BUHWEJU",
      country	== "UGANDA" &	district	== "IBANDA district" ~	"IBANDA",
      country	== "UGANDA" &	district	== "ISINGIRO district" ~	"ISINGIRO",
      country	== "UGANDA" &	district	== "KAZO district" ~	"KAZO",
      country	== "UGANDA" &	district	== "KIRUHURA district" ~	"KIRUHURA",
      country	== "UGANDA" &	district	== "MBARARA CITY" ~	"MBARARA",
      country	== "UGANDA" &	district	== "MBARARA district" ~	"MBARARA",
      country	== "UGANDA" &	district	== "MITOOMA district" ~	"MITOOMA",
      country	== "UGANDA" &	district	== "NTUNGAMO district" ~	"NTUNGAMO",
      country	== "UGANDA" &	district	== "RUBIRIZI district" ~	"RUBIRIZI",
      country	== "UGANDA" &	district	== "RWAMPARA district" ~	"RWAMPARA",
      country	== "UGANDA" &	district	== "SHEEMA district" ~	"SHEEMA",
      country	== "UGANDA" &	district	== "ABIM district" ~	"ABIM",
      country	== "UGANDA" &	district	== "AMUDAT district" ~	"AMUDAT",
      country	== "UGANDA" &	district	== "KARENGA district" ~	"KARENGA",
      country	== "UGANDA" &	district	== "KOTIDO district" ~	"KOTIDO",
      country	== "UGANDA" &	district	== "MOROTO district" ~	"MOROTO",
      country	== "UGANDA" &
        district	== "NABILATUK district" ~	"NABILATUK",
      country	== "UGANDA" &
        district	== "NAKAPIRIPIRIT district" ~	"NAKAPIRIPIRIT",
      country	== "UGANDA" &	district	== "NAPAK district" ~	"NAPAK",
      country	== "UGANDA" &	district	== "KIBOGA district" ~	"KIBOGA",
      country	== "UGANDA" &
        district	== "KYANKWANZI district" ~	"KYANKWANZI",
      country	== "UGANDA" &	district	== "LUWERO district" ~	"LUWERO",
      country	== "UGANDA" &	district	== "NAKASEKE district" ~	"NAKASEKE",
      country	== "UGANDA" &
        district	== "NAKASONGOLA district" ~	"NAKASONGOLA",
      country	== "UGANDA" &	district	== "AMURIA district" ~	"AMURIA",
      country	== "UGANDA" &	district	== "BUKEDEA district" ~	"BUKEDEA",
      country	== "UGANDA" &
        district	== "KABERAMAIDO district" ~	"KABERAMAIDO",
      country	== "UGANDA" &	district	== "KALAKI district" ~	"KALAKI",
      country	== "UGANDA" &	district	== "KATAKWI district" ~	"KATAKWI",
      country	== "UGANDA" &	district	== "KUMI district" ~	"KUMI",
      country	== "UGANDA" &	district	== "NGORA district" ~	"NGORA",
      country	== "UGANDA" &	district	== "SERERE district" ~	"SERERE",
      country	== "UGANDA" &	district	== "SOROTI CITY" ~	"SOROTI",
      country	== "UGANDA" &	district	== "SOROTI district" ~	"SOROTI",
      country	== "UGANDA" &	district	== "CENTRAL DIVISION" ~	"CENTRAL",
      country	== "UGANDA" &	district	== "ENTEBBE DIVISION" ~	"ENTEBBE",
      country	== "UGANDA" &	district	== "KAWEMPE DIVISION" ~	"KAWEMPE",
      country	== "UGANDA" &	district	== "MAKINDYE DIVISION" ~	"MAKINDYE",
      country	== "UGANDA" &	district	== "MUKONO district" ~	"MUKONO",
      country	== "UGANDA" &	district	== "NAKAWA DIVISION" ~	"NAKAWA",
      country	== "UGANDA" &	district	== "RUBAGA DIVISION" ~	"RUBAGA",
      country	== "UGANDA" &	district	== "WAKISO district" ~	"WAKISO",
      country	== "UGANDA" &	district	== "KASSANDA district" ~	"KASSANDA",
      country	== "UGANDA" &	district	== "BUTALEJA district" ~	"BUTALEJA",
      country	== "UGANDA" &	district	== "KAMPALA district" ~	"KAMPALA",
      country	== "UGANDA" &
        district	== "KIRYANDONGO district" ~	"KIRYANDONGO",
      country	== "UGANDA" &	district	== "BUKWO district" ~	"BUKWO",
      country	== "UGANDA" &	district	== "BUDAKA district" ~	"BUDAKA",
      country	== "UGANDA" &	district	== "KALUNGU district" ~	"KALUNGU",
      country	== "UGANDA" &	district	== "NEBBI district" ~	"NEBBI",
      country	== "UGANDA" &	district	== "YUMBE district" ~	"YUMBE",
      country	== "UGANDA" &	district	== "MPIGI district" ~	"MPIGI",
      country	== "UGANDA" &	district	== "OMORO district" ~	"OMORO",
      country	== "UGANDA" &	district	== "PADER district" ~	"PADER",
      country	== "UGANDA" &	district	== "KAGADI district" ~	"KAGADI",
      country	== "UGANDA" &	district	== "KIBAALE district" ~	"KIBAALE",
      country	== "UGANDA" &	district	== "KALIRO district" ~	"KALIRO",
      country	== "UGANDA" &	district	== "MAYUGE district" ~	"MAYUGE",
      country	== "UGANDA" &	district	== "RUKIGA district" ~	"RUKIGA",
      country	== "UGANDA" &	district	== "KAMWENGE district" ~	"KAMWENGE",
      country	== "UGANDA" &	district	== "AMOLATAR district" ~	"AMOLATAR",
      country	== "UGANDA" &	district	== "DOKOLO district" ~	"DOKOLO",
      country	== "UGANDA" &	district	== "OYAM district" ~	"OYAM",
      country	== "UGANDA" &	district	== "LWENGO district" ~	"LWENGO",
      country	== "UGANDA" &	district	== "RAKAI district" ~	"RAKAI",
      country	== "UGANDA" &
        district	== "SEMBABULE district" ~	"SEMBABULE",
      country	== "UGANDA" &	district	== "BUSIA district" ~	"BUSIA",
      country	== "UGANDA" &	district	== "KWEEN district" ~	"KWEEN",
      country	== "UGANDA" &	district	== "MANAFWA district" ~	"MANAFWA",
      country	== "UGANDA" &
        district	== "NAMISINDWA district" ~	"NAMISINDWA",
      country	== "UGANDA" &	district	== "PALLISA district" ~	"PALLISA",
      country	== "UGANDA" &	district	== "SIRONKO district" ~	"SIRONKO",
      country	== "UGANDA" &	district	== "BUSHENYI district" ~	"BUSHENYI",
      country	== "UGANDA" &
        district	== "KAPELEBYONG district" ~	"KAPELEBYONG",
      country	== "UGANDA" &	district	== "MADI-OKOLLO" ~	"MADI OKOLLO",
      country	== "UGANDA" &	district	== "PAKWACH district" ~	"PAKWACH",
      country	== "UGANDA" &	district	== "GULU district" ~	"GULU",
      country	== "UGANDA" &	district	== "MITYANA district" ~	"MITYANA",
      country	== "UGANDA" &	district	== "MUBENDE district" ~	"MUBENDE",
      country	== "UGANDA" &	district	== "OBONGI district" ~	"OBONGI",
      country	== "UGANDA" &	district	== "BUGIRI district" ~	"BUGIRI",
      country	== "UGANDA" &	district	== "KAABONG district" ~	"KAABONG",
      country	== "UGANDA" &	district	== "SSEMBABAULE" ~	"SEMBABULE",
      country	== "UGANDA" &	district	== "KYANKWANZI" ~	"KYAKWANZI",
      country	== "UGANDA" &	district	== "AGAGO district" ~	"AGAGO",
      country	== "UGANDA" &	district	== "KASSANDA" ~	"KASANDA",
      country	== "ZAMBIA" &	district	== "LAVUSHI" ~	"LAVUSHI MANDA",
      country	== "NIGERIA" &	district	== "YEWA SOUTH" ~	"EGBADO SOUTH",
      country	== "NIGERIA" &	district	== "YEWA NORTH" ~	"EGBADO NORTH",
      country	== "ZIMBABWE" &	district	== "MUTARE CITY" ~	"MUTARE",
      country	== "ZIMBABWE" &	district	== "MT DARWIN" ~	"MOUNT DARWIN",
      country	== "ZIMBABWE" &	district	== "MHONDORO" ~	"MHONDORO NGEZI",
      country	== "ZIMBABWE" &	district	== "MUREWA" ~	"MUREHWA",
      TRUE ~ district
    ),
    province = case_when(
      country	== "BENIN" &
        province	== "ATLANTIQUE" &
        district	== "ABOMEY-CALAVI 1" ~	"ATLANTIQUE",
      country	== "BENIN" &
        province	== "ATLANTIQUE" &
        district	== "ABOMEY-CALAVI 2" ~	"ATLANTIQUE",
      country	== "BENIN" &
        province	== "ATLANTIQUE" &
        district	== "ABOMEY-CALAVI 3" ~	"ATLANTIQUE",
      country	== "BENIN" &
        province	== "ATLANTIQUE" &	district	== "SO-AVA" ~	"ATLANTIQUE",
      country	== "BENIN" &
        province	== "ATLANTIQUE" &	district	== "TOFFO" ~	"ATLANTIQUE",
      country	== "BENIN" &
        province	== "BORGOU" &	district	== "BEMBEREKE" ~	"BORGOU",
      country	== "BENIN" &
        province	== "BORGOU" &	district	== "NIKKI" ~	"BORGOU",
      country	== "BENIN" &
        province	== "BORGOU" &	district	== "PARAKOU" ~	"BORGOU",
      country	== "BENIN" &
        province	== "BORGOU" &	district	== "PERERE" ~	"BORGOU",
      country	== "BENIN" &
        province	== "BORGOU" &	district	== "TCHAOUROU" ~	"BORGOU",
      country	== "BENIN" &
        province	== "DONGA" &	district	== "DJOUGOU" ~	"DONGA",
      country	== "BENIN" &
        province	== "LITTORAL" &	district	== "COTONOU 1" ~	"LITTORAL",
      country	== "BENIN" &
        province	== "LITTORAL" &	district	== "COTONOU 2" ~	"LITTORAL",
      country	== "BENIN" &
        province	== "LITTORAL" &	district	== "COTONOU 3" ~	"LITTORAL",
      country	== "BENIN" &
        province	== "LITTORAL" &	district	== "COTONOU 4" ~	"LITTORAL",
      country	== "BENIN" &
        province	== "LITTORAL" &	district	== "COTONOU 5" ~	"LITTORAL",
      country	== "BENIN" &
        province	== "LITTORAL" &	district	== "COTONOU 6" ~	"LITTORAL",
      country	== "BENIN" &
        province	== "OUEME" &	district	== "AGUEGUES" ~	"OUEME",
      country	== "BENIN" &
        province	== "OUEME" &	district	== "PORTO-NOVO 1" ~	"OUEME",
      country	== "BENIN" &
        province	== "OUEME" &	district	== "PORTO-NOVO 2" ~	"OUEME",
      country	== "BENIN" &
        province	== "OUEME" &	district	== "PORTO-NOVO 3" ~	"OUEME",
      country	== "BENIN" &
        province	== "OUEME" &	district	== "SEME-KPODJI" ~	"OUEME",
      country	== "COTE D IVOIRE" &
        province	== "CAVALLY" &	district	== "GUIGLO" ~	"CAVALLY",
      country	== "COTE D IVOIRE" &
        province	== "ABIDJAN1" &	district	== "ABOBO OUEST" ~	"ABIDJAN 1",
      country	== "COTE D IVOIRE" &
        province	== "ABIDJAN1" &	district	== "ANYAMA" ~	"ABIDJAN 1",
      country	== "COTE D IVOIRE" &
        province	== "ABIDJAN1" &	district	== "YOPOUGON EST" ~	"ABIDJAN 1",
      country	== "COTE D IVOIRE" &
        province	== "ABIDJAN1" &
        district	== "YOPOUGON OUEST-SONGON" ~	"ABIDJAN 1",
      country	== "COTE D IVOIRE" &
        province	== "ABIDJAN2" &
        district	== "COCODY-BINGERVILLE" ~	"ABIDJAN 2",
      country	== "COTE D IVOIRE" &
        province	== "ABIDJAN2" &	district	== "KOUMASSI" ~	"ABIDJAN 2",
      country	== "COTE D IVOIRE" &
        province	== "ABIDJAN2" &	district	== "PORT BOUET-VRIDI" ~	"ABIDJAN 2",
      country	== "COTE D IVOIRE" &
        province	== "ABIDJAN2" &
        district	== "TREICHVILLE-MARCORY" ~	"ABIDJAN 2",
      country	== "COTE D IVOIRE" &
        province	== "AGNEBY_TIASSA" &
        district	== "AGBOVILLE" ~	"AGNEBY-TIASSA",
      country	== "COTE D IVOIRE" &
        province	== "AGNEBY_TIASSA" &
        district	== "TIASSALE" ~	"AGNEBY-TIASSA",
      country	== "COTE D IVOIRE" &
        province	== "BAFING" &	district	== "KORO" ~	"BAFING",
      country	== "COTE D IVOIRE" &
        province	== "BAFING" &	district	== "TOUBA" ~	"BAFING",
      country	== "COTE D IVOIRE" &
        province	== "BAGOUE" &	district	== "BOUNDIALI" ~	"BAGOUE",
      country	== "COTE D IVOIRE" &
        province	== "BAGOUE" &	district	== "KOUTO" ~	"BAGOUE",
      country	== "COTE D IVOIRE" &
        province	== "BAGOUE" &	district	== "TENGRELA" ~	"BAGOUE",
      country	== "COTE D IVOIRE" &
        province	== "BELIER" &	district	== "DIDIEVI" ~	"BELIER",
      country	== "COTE D IVOIRE" &
        province	== "BELIER" &	district	== "TIEBISSOU" ~	"BELIER",
      country	== "COTE D IVOIRE" &
        province	== "BELIER" &	district	== "TOUMODI" ~	"BELIER",
      country	== "COTE D IVOIRE" &
        province	== "BELIER" &	district	== "YAMOUSSOUKRO" ~	"BELIER",
      country	== "COTE D IVOIRE" &
        province	== "BERE" &	district	== "DIANRA" ~	"BERE",
      country	== "COTE D IVOIRE" &
        province	== "BERE" &	district	== "KOUNAHIRI" ~	"BERE",
      country	== "COTE D IVOIRE" &
        province	== "BERE" &	district	== "MANKONO" ~	"BERE",
      country	== "COTE D IVOIRE" &
        province	== "BOUKANI" &	district	== "BOUNA" ~	"BOUNKANI",
      country	== "COTE D IVOIRE" &
        province	== "BOUKANI" &	district	== "DOROPO" ~	"BOUNKANI",
      country	== "COTE D IVOIRE" &
        province	== "BOUKANI" &	district	== "NASSIAN" ~	"BOUNKANI",
      country	== "COTE D IVOIRE" &
        province	== "BOUKANI" &	district	== "TEHINI" ~	"BOUNKANI",
      country	== "COTE D IVOIRE" &
        province	== "CAVALLY" &	district	== "BLOLEQUIN" ~	"CAVALLY",
      country	== "COTE D IVOIRE" &
        province	== "CAVALLY" &	district	== "DUEKOUE" ~	"MOYEN CAVALLY",
      country	== "COTE D IVOIRE" &
        province	== "CAVALLY" &	district	== "TAI" ~	"CAVALLY",
      country	== "COTE D IVOIRE" &
        province	== "CAVALLY" &	district	== "TOULEPLEU" ~	"CAVALLY",
      country	== "COTE D IVOIRE" &
        province	== "FOLON" &	district	== "KANIASSO" ~	"FOLON",
      country	== "COTE D IVOIRE" &
        province	== "FOLON" &	district	== "MINIGNAN" ~	"FOLON",
      country	== "COTE D IVOIRE" &
        province	== "GBEKE" &	district	== "BEOUMI" ~	"GBEKE",
      country	== "COTE D IVOIRE" &
        province	== "GBEKE" &	district	== "BOTRO" ~	"GBEKE",
      country	== "COTE D IVOIRE" &
        province	== "GBEKE" &	district	== "BOUAKE NORD-EST" ~	"GBEKE",
      country	== "COTE D IVOIRE" &
        province	== "GBEKE" &	district	== "BOUAKE NORD-OUEST" ~	"GBEKE",
      country	== "COTE D IVOIRE" &
        province	== "GBEKE" &	district	== "BOUAKE SUD" ~	"GBEKE",
      country	== "COTE D IVOIRE" &
        province	== "GBEKE" &	district	== "SAKASSOU" ~	"GBEKE",
      country	== "COTE D IVOIRE" &
        province	== "GBOKLE" &	district	== "SASSANDRA" ~	"GBOKLE",
      country	== "COTE D IVOIRE" &
        province	== "GOH" &	district	== "OUME" ~	"GOH",
      country	== "COTE D IVOIRE" &
        province	== "GONTOUGO" &	district	== "BONDOUKOU" ~	"GONTOUGO",
      country	== "COTE D IVOIRE" &
        province	== "GONTOUGO" &	district	== "KOUN-FAO" ~	"GONTOUGO",
      country	== "COTE D IVOIRE" &
        province	== "GONTOUGO" &	district	== "SANDEGUE" ~	"GONTOUGO",
      country	== "COTE D IVOIRE" &
        province	== "GONTOUGO" &	district	== "TANDA" ~	"GONTOUGO",
      country	== "COTE D IVOIRE" &
        province	== "GONTOUGO" &	district	== "TRANSUA" ~	"GONTOUGO",
      country	== "COTE D IVOIRE" &
        province	== "GRANDS_PONTS" &	district	== "DABOU" ~	"GRANDS PONTS",
      country	== "COTE D IVOIRE" &
        province	== "GRANDS_PONTS" &
        district	== "GRAND-LAHOU" ~	"GRANDS PONTS",
      country	== "COTE D IVOIRE" &
        province	== "GRANDS_PONTS" &
        district	== "JACQUEVILLE" ~	"GRANDS PONTS",
      country	== "COTE D IVOIRE" &
        province	== "GUEMON" &	district	== "BANGOLO" ~	"GUEMON",
      country	== "COTE D IVOIRE" &
        province	== "GUEMON" &	district	== "KOUIBLY" ~	"GUEMON",
      country	== "COTE D IVOIRE" &
        province	== "HAMBOL" &	district	== "DABAKALA" ~	"HAMBOL",
      country	== "COTE D IVOIRE" &
        province	== "HAMBOL" &	district	== "KATIOLA" ~	"HAMBOL",
      country	== "COTE D IVOIRE" &
        province	== "HAMBOL" &	district	== "NIAKARAMADOUGOU" ~	"HAMBOL",
      country	== "COTE D IVOIRE" &
        province	== "HAUT_SASSANDRA" &	district	== "DALOA" ~	"HAUT SASSANDRA",
      country	== "COTE D IVOIRE" &
        province	== "HAUT_SASSANDRA" &	district	== "ISSIA" ~	"HAUT SASSANDRA",
      country	== "COTE D IVOIRE" &
        province	== "HAUT_SASSANDRA" &
        district	== "VAVOUA" ~	"HAUT SASSANDRA",
      country	== "COTE D IVOIRE" &
        province	== "HAUT_SASSANDRA" &
        district	== "ZOUKOUGBEU" ~	"HAUT-SASSANDRA",
      country	== "COTE D IVOIRE" &
        province	== "IFFOU" &	district	== "DAOUKRO" ~	"IFOU",
      country	== "COTE D IVOIRE" &
        province	== "IFFOU" &	district	== "MBAHIAKRO" ~	"IFOU",
      country	== "COTE D IVOIRE" &
        province	== "IFFOU" &	district	== "PRIKRO" ~	"IFOU",
      country	== "COTE D IVOIRE" &
        province	== "INDENIE_DJUABLIN" &
        district	== "AGNIBILEKROU" ~	"INDENIE-DJUABLIN",
      country	== "COTE D IVOIRE" &
        province	== "INDENIE_DJUABLIN" &
        district	== "BETTIE" ~	"INDENIE-DJUABLIN",
      country	== "COTE D IVOIRE" &
        province	== "KABADOUGOU" &	district	== "MADINANI" ~	"KABADOUGOU",
      country	== "COTE D IVOIRE" &
        province	== "KABADOUGOU" &	district	== "ODIENNE" ~	"KABADOUGOU",
      country	== "COTE D IVOIRE" &
        province	== "LAME" &	district	== "YAKASSE-ATTOBROU" ~	"ME",
      country	== "COTE D IVOIRE" &
        province	== "LOH_DJIBOUA" &	district	== "DIVO" ~	"LOH-DJIBOUA",
      country	== "COTE D IVOIRE" &
        province	== "LOH_DJIBOUA" &	district	== "GUITRY" ~	"LOH-DJIBOUA",
      country	== "COTE D IVOIRE" &
        province	== "LOH_DJIBOUA" &	district	== "LAKOTA" ~	"LOH-DJIBOUA",
      country	== "COTE D IVOIRE" &
        province	== "MARAHOUE" &	district	== "BOUAFLE" ~	"MARAHOUT",
      country	== "COTE D IVOIRE" &
        province	== "MARAHOUE" &	district	== "SINFRA" ~	"MARAHOUT",
      country	== "COTE D IVOIRE" &
        province	== "MARAHOUE" &	district	== "ZUENOULA" ~	"MARAHOUT",
      country	== "COTE D IVOIRE" &
        province	== "MORONOU" &	district	== "ARRAH" ~	"MORONOU",
      country	== "COTE D IVOIRE" &
        province	== "MORONOU" &	district	== "BONGOUANOU" ~	"MORONOU",
      country	== "COTE D IVOIRE" &
        province	== "MORONOU" &	district	== "MBATTO" ~	"MORONOU",
      country	== "COTE D IVOIRE" &
        province	== "NAWA" &	district	== "BUYO" ~	"NAWA",
      country	== "COTE D IVOIRE" &
        province	== "NAWA" &	district	== "GUEYO" ~	"NAWA",
      country	== "COTE D IVOIRE" &
        province	== "NAWA" &	district	== "MEAGUI" ~	"NAWA",
      country	== "COTE D IVOIRE" &
        province	== "NAWA" &	district	== "SOUBRE" ~	"NAWA",
      country	== "COTE D IVOIRE" &
        province	== "NZI" &	district	== "BOCANDA" ~	"NZI",
      country	== "COTE D IVOIRE" &
        province	== "NZI" &	district	== "DIMBOKRO" ~	"NZI",
      country	== "COTE D IVOIRE" &
        province	== "NZI" &	district	== "KOUASSI-KOUASSIKRO" ~	"NZI",
      country	== "COTE D IVOIRE" &
        province	== "PORO" &	district	== "DIKODOUGOU" ~	"PORO",
      country	== "COTE D IVOIRE" &
        province	== "PORO" &	district	== "KORHOGO 1" ~	"PORO",
      country	== "COTE D IVOIRE" &
        province	== "PORO" &	district	== "KORHOGO 2" ~	"PORO",
      country	== "COTE D IVOIRE" &
        province	== "PORO" &	district	== "MBENGUE" ~	"PORO",
      country	== "COTE D IVOIRE" &
        province	== "PORO" &	district	== "SINEMATIALI" ~	"PORO",
      country	== "COTE D IVOIRE" &
        province	== "SAN_PEDRO1" &	district	== "SAN PEDRO" ~	"SAN-PEDRO",
      country	== "COTE D IVOIRE" &
        province	== "SAN_PEDRO1" &	district	== "TABOU" ~	"SAN-PEDRO",
      country	== "COTE D IVOIRE" &
        province	== "SUD_COMOE" &	district	== "ABOISSO" ~	"SUD-COMOE",
      country	== "COTE D IVOIRE" &
        province	== "SUD_COMOE" &	district	== "GRAND-BASSAM" ~	"SUD COMOE",
      country	== "COTE D IVOIRE" &
        province	== "SUD_COMOE" &	district	== "TIAPOUM" ~	"SUD-COMOE",
      country	== "COTE D IVOIRE" &
        province	== "TCHOLOGO" &	district	== "FERKESSEDOUGOU" ~	"TCHOLOGO",
      country	== "COTE D IVOIRE" &
        province	== "TCHOLOGO" &	district	== "KONG" ~	"TCHOLOGO",
      country	== "COTE D IVOIRE" &
        province	== "TCHOLOGO" &	district	== "OUANGOLODOUGOU" ~	"TCHOLOGO",
      country	== "COTE D IVOIRE" &
        province	== "TONKPI" &	district	== "BIANKOUMA" ~	"TONKPI",
      country	== "COTE D IVOIRE" &
        province	== "TONKPI" &	district	== "DANANE" ~	"TONKPI",
      country	== "COTE D IVOIRE" &
        province	== "TONKPI" &	district	== "MAN" ~	"TONKPI",
      country	== "COTE D IVOIRE" &
        province	== "TONKPI" &	district	== "ZOUAN-HOUNIEN" ~	"TONKPI",
      country	== "COTE D IVOIRE" &
        province	== "WORODOUGOU" &	district	== "KANI" ~	"WORODOUGOU",
      country	== "COTE D IVOIRE" &
        province	== "WORODOUGOU" &	district	== "SEGUELA" ~	"WORODOUGOU",
      country	== "COTE D IVOIRE" &
        province	== "ABIDJAN1" &	district	== "ABOBO EST" ~	"ABIDJAN 1",
      country	== "COTE D IVOIRE" &
        province	== "GBOKLE" &	district	== "FRESCO" ~	"GBOKLE",
      country	== "COTE D IVOIRE" &
        province	== "GOH" &	district	== "GAGNOA 1" ~	"GOH",
      country	== "COTE D IVOIRE" &
        province	== "SUD_COMOE" &	district	== "ADIAKE" ~	"SUD-COMOE",
      country	== "COTE D IVOIRE" &
        province	== "ABIDJAN2" &
        district	== "ADJAME-PLATEAU-ATTECOUBE" ~	"ABIDJAN 2",
      country	== "COTE D IVOIRE" &
        province	== "AGNEBY_TIASSA" &	district	== "SIKENSI" ~	"AGNEBY-TIASSA",
      country	== "COTE D IVOIRE" &
        province	== "BAFING" &	district	== "OUANINOU" ~	"BAFING",
      country	== "COTE D IVOIRE" &
        province	== "GOH" &	district	== "GAGNOA 2" ~	"GOH",
      country	== "COTE D IVOIRE" &
        province	== "INDENIE_DJUABLIN" &
        district	== "ABENGOUROU" ~	"INDENIE-DJUABLIN",
      country	== "COTE D IVOIRE" &
        province	== "LAME" &	district	== "AKOUPE" ~	"ME",
      country	== "COTE D IVOIRE" &
        province	== "LAME" &	district	== "ALEPE" ~	"ME",
      country	== "COTE D IVOIRE" &
        province	== "LAME" &	district	== "ADZOPE" ~	"ME",
      country	== "COTE D IVOIRE" &
        province	== "ME" &	district	== "ADZOPE" ~	"ME",
      country	== "COTE D IVOIRE" &
        province	== "ME" &	district	== "AKOUPE" ~	"ME",
      country	== "COTE D IVOIRE" &
        province	== "ME" &	district	== "YAKASSE-ATTOBROU" ~	"ME",
      country	== "COTE D IVOIRE" &
        province	== "SAN PEDRO" &	district	== "TABOU" ~	"SAN-PEDRO",
      country	== "COTE D IVOIRE" &
        province	== "ME" &	district	== "ALEPE" ~	"ME",
      country	== "COTE D IVOIRE" &
        province	== "SAN PEDRO" &	district	== "SAN PEDRO" ~	"SAN-PEDRO",
      country	== "GUINEA" &
        province	== "FARANAH" &	district	== "DABOLA" ~	"FARANAH",
      country	== "GUINEA" &
        province	== "FARANAH" &	district	== "DINGUIRAYE" ~	"FARANAH",
      country	== "GUINEA" &
        province	== "FARANAH" &	district	== "FARANAH" ~	"FARANAH",
      country	== "GUINEA" &
        province	== "FARANAH" &	district	== "KISSIDOUGOU" ~	"FARANAH",
      country	== "GUINEA" &
        province	== "KANKAN" &	district	== "KANKAN" ~	"KANKAN",
      country	== "GUINEA" &
        province	== "KANKAN" &	district	== "KÉROUANE" ~	"KANKAN",
      country	== "GUINEA" &
        province	== "KANKAN" &	district	== "KOUROUSSA" ~	"KANKAN",
      country	== "GUINEA" &
        province	== "KANKAN" &	district	== "MANDIANA" ~	"KANKAN",
      country	== "GUINEA" &
        province	== "KANKAN" &	district	== "SIGUIRI" ~	"KANKAN",
      country	== "GUINEA" &
        province	== "LABE" &	district	== "KOUBIA" ~	"LABE",
      country	== "GUINEA" &	province	== "LABE" &
        district	== "MALI" ~	"LABE",
      country	== "GUINEA" &
        province	== "LABE" &	district	== "TOUGUÉ" ~	"LABE",
      country	== "GUINEA" &
        province	== "NZEREKORE" &	district	== "BEYLA" ~	"NZEREKORE",
      country	== "GUINEA" &
        province	== "NZEREKORE" &	district	== "GUECKÉDOU" ~	"NZEREKORE",
      country	== "GUINEA" &
        province	== "NZEREKORE" &	district	== "LOLA" ~	"NZEREKORE",
      country	== "GUINEA" &
        province	== "NZEREKORE" &	district	== "MACENTA" ~	"NZEREKORE",
      country	== "GUINEA" &
        province	== "NZEREKORE" &	district	== "N'ZÉRÉKORÉ" ~	"NZEREKORE",
      country	== "GUINEA" &
        province	== "NZEREKORE" &	district	== "YOMOU" ~	"NZEREKORE",
      country	== "GUINEA" &	province	== "LABE" &
        district	== "LABÉ" ~	"LABE",
      country	== "GUINEA" &
        province	== "BOKE" &	district	== "BOFFA" ~	"BOKE",
      country	== "GUINEA" &	province	== "BOKE" &
        district	== "BOKÉ" ~	"BOKE",
      country	== "GUINEA" &	province	== "BOKE" &
        district	== "FRIA" ~	"BOKE",
      country	== "GUINEA" &
        province	== "BOKE" &	district	== "GAOUAL" ~	"BOKE",
      country	== "GUINEA" &
        province	== "BOKE" &	district	== "KOUNDARA" ~	"BOKE",
      country	== "GUINEA" &
        province	== "CONAKRY" &	district	== "DIXINN" ~	"CONAKRY",
      country	== "GUINEA" &
        province	== "CONAKRY" &	district	== "KALOUM" ~	"CONAKRY",
      country	== "GUINEA" &
        province	== "CONAKRY" &	district	== "MATAM" ~	"CONAKRY",
      country	== "GUINEA" &
        province	== "CONAKRY" &	district	== "MATOTO" ~	"CONAKRY",
      country	== "GUINEA" &
        province	== "CONAKRY" &	district	== "RATOMA" ~	"CONAKRY",
      country	== "GUINEA" &
        province	== "KINDIA" &	district	== "COYAH" ~	"KINDIA",
      country	== "GUINEA" &
        province	== "KINDIA" &	district	== "DUBRÉKA" ~	"KINDIA",
      country	== "GUINEA" &
        province	== "KINDIA" &	district	== "FORÉCARIAH" ~	"KINDIA",
      country	== "GUINEA" &
        province	== "KINDIA" &	district	== "KINDIA" ~	"KINDIA",
      country	== "GUINEA" &
        province	== "KINDIA" &	district	== "TÉLIMÉLÉ" ~	"KINDIA",
      country	== "GUINEA" &
        province	== "LABE" &	district	== "LÉLOUMA" ~	"LABE",
      country	== "GUINEA" &
        province	== "MAMOU" &	district	== "DALABA" ~	"MAMOU",
      country	== "GUINEA" &
        province	== "MAMOU" &	district	== "MAMOU" ~	"MAMOU",
      country	== "GUINEA" &
        province	== "MAMOU" &	district	== "PITA" ~	"MAMOU",
      country	== "BURKIN FASO" &
        province	== "BOUCLE_DU_MOUHOUN" ~	"DEDOUGOU",
      country	== "BURKIN FASO" &	province	== "CENTRE-SUD" ~	"MANGA",
      country	== "BURKIN FASO" &	province	== "CENTRE-OUEST" ~	"KOUDOUGOU",
      country	== "BURKIN FASO" &	province	== "HAUTS-BASSINS" ~	"BOBO",
      country	== "BURKIN FASO" &	province	== "NORD" ~	"OUAHIGOUYA",
      country	== "BURKIN FASO" &	province	== "CENTRE" ~	"OUAGADOUGOU",
      country	== "BURKIN FASO" &	province	== "CENTRE" ~	"OUAGADOUGOU",
      country	== "BURKIN FASO" &	province	== "CENTRE" ~	"OUAGADOUGOU",
      country	== "BURKIN FASO" &	province	== "CENTRE-NORD" ~	"KAYA",
      country	== "BURKIN FASO" &	province	== "CENTRE" ~	"OUAGADOUGOU",
      country	== "BURKIN FASO" &	province	== "CENTRE" ~	"OUAGADOUGOU",
      country	== "BURKIN FASO" &	province	== "PLATEAU_CENTRAL" ~	"ZINIARE",
      country	== "BURKIN FASO" &	province	== "PLATEAU_CENTRAL" ~	"ZINIARE",
      country	== "BURKIN FASO" &	province	== "NORD" ~	"OUAHIGOUYA",
      country	== "BURKIN FASO" &	province	== "PLATEAU_CENTRAL" ~	"ZINIARE",
      country	== "BURKIN FASO" &	province	== "SAHEL" ~	"DORI",
      country	== "BURKIN FASO" &	province	== "SUD-OUEST" ~	"GAOUA",
      country	== "BURKIN FASO" &	province	== "SUD-OUEST" ~	"GAOUA",
      country	== "BURKIN FASO" &	province	== "SAHEL" ~	"DORI",
      country	== "BURKIN FASO" &	province	== "SAHEL" ~	"DORI",
      country	== "BURKIN FASO" &	province	== "NORD" ~	"OUAHIGOUYA",
      country	== "BURKIN FASO" &	province	== "CENTRE-EST" ~	"TENKODOGO",
      country	== "BURKIN FASO" &	province	== "CENTRE-EST" ~	"TENKODOGO",
      country	== "BURKIN FASO" &	province	== "CASCADES" ~	"BANFORA",
      country	== "BURKIN FASO" &
        province	== "BOUCLE_DU_MOUHOUN" ~	"DEDOUGOU",
      country	== "BURKIN FASO" &
        province	== "BOUCLE_DU_MOUHOUN" ~	"DEDOUGOU",
      country	== "BURKIN FASO" &	province	== "CENTRE-EST" ~	"TENKODOGO",
      country	== "BURKIN FASO" &	province	== "HAUTS-BASSINS" ~	"BOBO",
      country	== "BURKIN FASO" &	province	== "HAUTS-BASSINS" ~	"BOBO",
      country	== "BURKIN FASO" &	province	== "CENTRE-OUEST" ~	"KOUDOUGOU",
      country	== "BURKIN FASO" &	province	== "CENTRE-OUEST" ~	"KOUDOUGOU",
      country	== "BURKIN FASO" &	province	== "CENTRE-OUEST" ~	"KOUDOUGOU",
      country	== "BURKIN FASO" &	province	== "CENTRE-SUD" ~	"MANGA",
      country	== "BURKIN FASO" &	province	== "CENTRE-SUD" ~	"OUAGADOUGOU",
      country	== "BURKIN FASO" &	province	== "EST" ~	"OUAGADOUGOU",
      country	== "BURKIN FASO" &	province	== "EST" ~	"FADA",
      country	== "BURKIN FASO" &	province	== "CENTRE-NORD" ~	"KAYA",
      country	== "BURKIN FASO" &	province	== "HAUTS-BASSINS" ~	"BOBO",
      country	== "BURKIN FASO" &	province	== "NORD" ~	"OUAHIGOUYA",
      country	== "BURKIN FASO" &	province	== "CASCADES" ~	"BANFORA",
      country	== "BURKIN FASO" &	province	== "EST" ~	"FADA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD-UBANGI" ~	"NORD UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMANI" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-CENTRAL" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS-UELE" ~	"BAS UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD-KIVU" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-KATANGA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-UELE" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-CENTRAL" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAI-NDOMBE" ~	"MAINDOMBE",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ATSINANANA" &
        district	== "MIDONGY-ATSIMO" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ATSINANANA" &
        district	== "VONDROZO" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "HAUTE MATSIATRA" &
        district	== "AMBOHIMAHASOA" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ANDREFANA" &	district	== "BEROROHA" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ATSINANANA" &	district	== "MAROLAMBO" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "MENABE" &	district	== "BELO-TSIRIBIHINA" ~	"MENABE",
      country	== "MADAGASCAR" &
        province	== "MENABE" &	district	== "MIANDRIVAZO" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "MENABE" &	district	== "MORONDAVA" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ANDREFANA" &	district	== "BENENITRA" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "VAKINANKARATRA" &
        district	== "ANTSIRABE I" ~	"VAKINANKARATRA",
      country	== "MADAGASCAR" &
        province	== "VATOVAVY FITOVINANY" &
        district	== "NOSY-VARIKA" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "AMORON'I MANIA" &
        district	== "FANDRIANA" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "ANALAMANGA" &
        district	== "ANTANANARIVO-ATSIMONDRANO" ~	"ANALAMANGA",
      country	== "MADAGASCAR" &
        province	== "ANOSY" &	district	== "BETROKA" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ANDREFANA" &	district	== "SAKARAHA" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "IHOROMBE" &	district	== "IAKORA" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "IHOROMBE" &	district	== "IVOHIBE" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "VATOVAVY FITOVINANY" &
        district	== "MANANJARY" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "AMORON'I MANIA" &
        district	== "AMBATOFINANDRAHANA" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "ANALAMANGA" &
        district	== "AMBOHIDRATRIMO" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "ANALAMANGA" &	district	== "ANDRAMASINA" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "ANALAMANGA" &	district	== "ANJOZOROBE" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "ANALAMANGA" &	district	== "ANKAZOBE" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "ANALANJIROFO" &	district	== "VAVATENINA" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "ANDROY" &	district	== "AMBOVOMBE-ANDROY" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ANOSY" &	district	== "AMBOASARY-ATSIMO" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ANOSY" &	district	== "TAOLAGNARO" ~	"ANOSY",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ANDREFANA" &	district	== "MOROMBE" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ATSINANANA" &
        district	== "FARAFANGANA" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ATSINANANA" &
        district	== "VANGAINDRANO" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "ATSINANANA" &
        district	== "ANTANAMBAO-MANAMPOTSY" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "ATSINANANA" &	district	== "MAHANORO" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "ATSINANANA" &
        district	== "VOHIBINANY (BRICKAVILLE)" ~	"ATSINANANA",
      country	== "MADAGASCAR" &
        province	== "BOENY" &	district	== "AMBATO-BOINA" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "BOENY" &	district	== "MAHAJANGA II" ~	"BOENI",
      country	== "MADAGASCAR" &
        province	== "BOENY" &	district	== "MITSINJO" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "HAUTE MATSIATRA" &
        district	== "AMBALAVAO" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "HAUTE MATSIATRA" &
        district	== "FIANARANTSOA I" ~	"HAUTE-MATSIATRA",
      country	== "MADAGASCAR" &
        province	== "HAUTE MATSIATRA" &
        district	== "IKALAMAVONY" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "MELAKY" &	district	== "MORAFENOBE" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "VAKINANKARATRA" &
        district	== "AMBATOLAMPY" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "VAKINANKARATRA" &
        district	== "ANTANIFOTSY" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "VAKINANKARATRA" &
        district	== "ANTSIRABE II" ~	"VAKINANKARATRA",
      country	== "MADAGASCAR" &
        province	== "VAKINANKARATRA" &	district	== "BETAFO" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "VAKINANKARATRA" &
        district	== "MANDOTO" ~	"VAKINANKARATRA",
      country	== "MADAGASCAR" &
        province	== "VATOVAVY FITOVINANY" &
        district	== "VOHIPENO" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "ANALAMANGA" &
        district	== "ANTANANARIVO-AVARADRANO" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "ANALAMANGA" &
        district	== "ANTANANARIVO-RENIVOHITRA" ~	"ANALAMANGA",
      country	== "MADAGASCAR" &
        province	== "ANALAMANGA" &
        district	== "MANJAKANDRIANA" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "ANALANJIROFO" &
        district	== "SOANIERANA-IVONGO" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "ATSINANANA" &	district	== "VATOMANDRY" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "BOENY" &	district	== "MAROVOAY" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "BOENY" &	district	== "SOALALA" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "HAUTE MATSIATRA" &
        district	== "VOHIBATO" ~	"HAUTE-MATSIATRA",
      country	== "MADAGASCAR" &
        province	== "IHOROMBE" &	district	== "IHOSY" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "MELAKY" &	district	== "ANTSALOVA" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "MENABE" &	district	== "MAHABO" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "AMORON'I MANIA" &
        district	== "MANANDRIANA" ~	"AMORON'I MANIA",
      country	== "MADAGASCAR" &
        province	== "ANALANJIROFO" &
        district	== "FENOARIVO-ATSINANANA" ~	"ANALANJIROFO",
      country	== "MADAGASCAR" &
        province	== "ANALANJIROFO" &	district	== "MAROANTSETRA" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "ANALANJIROFO" &
        district	== "NOSY-BORAHA (SAINTE MARIE)" ~	"ANALANJIROFO",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ANDREFANA" &
        district	== "ANKAZOABO-ATSIMO" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ANDREFANA" &
        district	== "BETIOKY-ATSIMO" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ANDREFANA" &
        district	== "TOLIARA I ET II" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ATSINANANA" &	district	== "TOAMASINA I" ~	"ATSINANANA",
      country	== "MADAGASCAR" &
        province	== "ATSINANANA" &	district	== "TOAMASINA II" ~	"ATSINANANA",
      country	== "MADAGASCAR" &
        province	== "HAUTE MATSIATRA" &
        district	== "ISANDRA" ~	"HAUTE-MATSIATRA",
      country	== "MADAGASCAR" &
        province	== "HAUTE MATSIATRA" &
        district	== "LALANGINA" ~	"HAUTE-MATSIATRA",
      country	== "MADAGASCAR" &
        province	== "MELAKY" &	district	== "AMBATOMAINTY" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "MELAKY" &	district	== "BESALAMPY" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "MENABE" &	district	== "MANJA" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "VAKINANKARATRA" &
        district	== "FARATSIHO" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "VATOVAVY FITOVINANY" &
        district	== "IFANADIANA" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "VATOVAVY FITOVINANY" &
        district	== "IKONGO" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "VATOVAVY FITOVINANY" &
        district	== "MANAKARA-ATSIMO" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "AMORON'I MANIA" &
        district	== "AMBOSITRA" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "ANDROY" &	district	== "BEKILY" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ANDROY" &	district	== "TSIHOMBE" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ATSINANANA" &
        district	== "BEFOTAKA" ~	"FIANARANTSOA",
      country	== "MADAGASCAR" &
        province	== "ANDROY" &	district	== "BELOHA" ~	"TOLIARY",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ANDREFANA" &	district	== "AMPANIHY" ~	"SUD-OUEST",
      country	== "MADAGASCAR" &
        province	== "BOENY" &	district	== "MAHAJANGA I" ~	"BOENI",
      country	== "MADAGASCAR" &
        province	== "MELAKY" &	district	== "MAINTIRANO" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "ANALANJIROFO" &
        district	== "MANANARA-AVARATRA" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "ITASY" &	district	== "MIARINARIVO" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "ALAOTRA MANGORO" &
        district	== "AMPARAFARAVOLA" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "BONGOLAVA" &
        district	== "TSIROANOMANDIDY" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "SAVA" &	district	== "VOHIMARINA (VOHÉMAR)" ~	"SAVA",
      country	== "MADAGASCAR" &
        province	== "SOFIA" &	district	== "BEALANANA" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "BONGOLAVA" &
        district	== "FENOARIVO-AFOVOANY" ~	"BONGOLAVA",
      country	== "MADAGASCAR" &
        province	== "DIANA" &	district	== "ANTSIRANANA II" ~	"DIANA",
      country	== "MADAGASCAR" &
        province	== "DIANA" &	district	== "NOSY-BE" ~	"DIANA",
      country	== "MADAGASCAR" &
        province	== "SAVA" &	district	== "ANDAPA" ~	"ANTSIRANANA",
      country	== "MADAGASCAR" &
        province	== "SOFIA" &	district	== "ANALALAVA" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "SOFIA" &
        district	== "BEFANDRIANA-AVARATRA" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "SOFIA" &
        district	== "BORIZINY (PORT-BERGER)" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "SOFIA" &	district	== "MAMPIKONY" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "SOFIA" &	district	== "MANDRITSARA" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "ALAOTRA MANGORO" &
        district	== "ANOSIBE AN-ALA" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "BETSIBOKA" &	district	== "KANDREHO" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "BETSIBOKA" &	district	== "TSARATANANA" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "DIANA" &	district	== "AMBANJA" ~	"ANTSIRANANA",
      country	== "MADAGASCAR" &
        province	== "DIANA" &	district	== "AMBILOBE" ~	"ANTSIRANANA",
      country	== "MADAGASCAR" &
        province	== "DIANA" &	district	== "ANTSIRANANA I" ~	"ANTSIRANANA",
      country	== "MADAGASCAR" &
        province	== "SAVA" &	district	== "ANTALAHA" ~	"ANTSIRANANA",
      country	== "MADAGASCAR" &
        province	== "SOFIA" &	district	== "ANTSOHIHY" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "ALAOTRA MANGORO" &
        district	== "AMBATONDRAZAKA" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "ALAOTRA MANGORO" &
        district	== "ANDILAMENA" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "ALAOTRA MANGORO" &	district	== "MORAMANGA" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "SAVA" &	district	== "SAMBAVA" ~	"ANTSIRANANA",
      country	== "MADAGASCAR" &
        province	== "BETSIBOKA" &	district	== "MAEVATANANA" ~	"MAHAJANGA",
      country	== "MADAGASCAR" &
        province	== "ITASY" &	district	== "ARIVONIMAMO" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "ITASY" &	district	== "SOAVINANDRIANA" ~	"ANTANANARIVO",
      country	== "MADAGASCAR" &
        province	== "BONGOLAVA" &
        district	== "FENOARIVO-AFOVOANY" ~	"BONGOLAVA",
      country	== "MADAGASCAR" &
        province	== "ALAOTRA MANGORO" &
        district	== "ANOSIBE AN-ALA" ~	"TOAMASINA",
      country	== "MADAGASCAR" &
        province	== "ATSIMO ANDREFANA" &
        district	== "TOLIARA I ET II" ~	"TOLIARY",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO CENTRAL" &
        district	== "NSONA-PANGU" ~	"BAS-CONGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO CENTRAL" &	district	== "MATADI" ~	"BAS-CONGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO CENTRAL" &	district	== "MUANDA" ~	"BAS-CONGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO CENTRAL" &	district	== "LUOZI" ~	"BAS-CONGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "BOMA BUNGU" ~	"BAS-CONGO",
      country	== "BURKINA FASO" &
        province	== "CENTRE-EST" &	district	== "BITTOU" ~	"TENKODOGO",
      country	== "BURKINA FASO" &
        province	== "CENTRE-EST" &	district	== "OUARGAYE" ~	"TENKODOGO",
      country	== "BURKINA FASO" &
        province	== "CENTRE-NORD" &	district	== "BOUSSOUMA" ~	"KAYA",
      country	== "BURKINA FASO" &
        province	== "CENTRE" &	district	== "BASKUY" ~	"OUAGADOUGOU",
      country	== "BURKINA FASO" &
        province	== "CENTRE-NORD" &	district	== "TOUGOURI" ~	"KAYA",
      country	== "BURKINA FASO" &
        province	== "PLATEAU_CENTRAL" &	district	== "ZORGHO" ~	"ZINIARE",
      country	== "BURKINA FASO" &
        province	== "EST" &	district	== "MANNI" ~	"FADA",
      country	== "BURKINA FASO" &
        province	== "CENTRE" &	district	== "BOULMIOUGOU" ~	"OUAGADOUGOU",
      country	== "BURKINA FASO" &
        province	== "CENTRE-OUEST" &	district	== "SABOU" ~	"KOUDOUGOU",
      country	== "BURKINA FASO" &
        province	== "CENTRE-NORD" &	district	== "KAYA" ~	"KAYA",
      country	== "BURKINA FASO" &
        province	== "CENTRE-NORD" &	district	== "BARSALOGHO" ~	"KAYA",
      country	== "BURKINA FASO" &
        province	== "CENTRE-OUEST" &	district	== "LEO" ~	"KOUDOUGOU",
      country	== "BURKINA FASO" &
        province	== "CENTRE-OUEST" &	district	== "NANORO" ~	"KOUDOUGOU",
      country	== "BURKINA FASO" &
        province	== "CENTRE-OUEST" &	district	== "REO" ~	"KOUDOUGOU",
      country	== "BURKINA FASO" &
        province	== "CENTRE-OUEST" &	district	== "SAPOUY" ~	"KOUDOUGOU",
      country	== "BURKINA FASO" &
        province	== "CENTRE-OUEST" &	district	== "TENADO" ~	"KOUDOUGOU",
      country	== "BURKINA FASO" &
        province	== "EST" &	district	== "BOGANDE" ~	"FADA",
      country	== "BURKINA FASO" &
        province	== "EST" &	district	== "DIAPAGA" ~	"FADA",
      country	== "BURKINA FASO" &
        province	== "EST" &	district	== "FADA" ~	"FADA",
      country	== "BURKINA FASO" &
        province	== "EST" &	district	== "GAYERI" ~	"FADA",
      country	== "BURKINA FASO" &
        province	== "EST" &	district	== "PAMA" ~	"FADA",
      country	== "BURKINA FASO" &
        province	== "PLATEAU_CENTRAL" &	district	== "BOUSSE" ~	"ZINIARE",
      country	== "BURKINA FASO" &
        province	== "PLATEAU_CENTRAL" &	district	== "ZINIARE" ~	"ZINIARE",
      country	== "BURKINA FASO" &
        province	== "CENTRE-EST" &	district	== "GARANGO" ~	"TENKODOGO",
      country	== "BURKINA FASO" &
        province	== "CENTRE-EST" &	district	== "KOUPELA" ~	"TENKODOGO",
      country	== "BURKINA FASO" &
        province	== "CENTRE-EST" &	district	== "POUYTENGA" ~	"TENKODOGO",
      country	== "BURKINA FASO" &
        province	== "CENTRE-EST" &	district	== "TENKODOGO" ~	"TENKODOGO",
      country	== "BURKINA FASO" &
        province	== "CENTRE-EST" &	district	== "ZABRE" ~	"TENKODOGO",
      country	== "BURKINA FASO" &
        province	== "CENTRE-NORD" &	district	== "BOULSA" ~	"KAYA",
      country	== "BURKINA FASO" &
        province	== "CENTRE-NORD" &	district	== "KONGOUSSI" ~	"KAYA",
      country	== "BURKINA FASO" &
        province	== "CENTRE-OUEST" &	district	== "KOUDOUGOU" ~	"KOUDOUGOU",
      country	== "BURKINA FASO" &
        province	== "CENTRE-SUD" &	district	== "KOMBISSIRI" ~	"MANGA",
      country	== "BURKINA FASO" &
        province	== "CENTRE-SUD" &	district	== "MANGA" ~	"MANGA",
      country	== "BURKINA FASO" &
        province	== "CENTRE-SUD" &	district	== "PÔ" ~	"MANGA",
      country	== "BURKINA FASO" &
        province	== "CENTRE-SUD" &	district	== "SAPONE" ~	"MANGA",
      country	== "BURKINA FASO" &
        province	== "CENTRE" &	district	== "BOGODOGO" ~	"OUAGADOUGOU",
      country	== "BURKINA FASO" &
        province	== "CENTRE" &	district	== "NONGR MASSOM" ~	"OUAGADOUGOU",
      country	== "BURKINA FASO" &
        province	== "CENTRE" &	district	== "SIG NOGHIN" ~	"OUAGADOUGOU",
      country	== "BURKINA FASO" &
        province	== "NORD" &	district	== "GOURCY" ~	"OUAHIGOUYA",
      country	== "BURKINA FASO" &
        province	== "NORD" &	district	== "THIOU" ~	"OUAHIGOUYA",
      country	== "BURKINA FASO" &
        province	== "NORD" &	district	== "YAKO" ~	"OUAHIGOUYA",
      country	== "BURKINA FASO" &
        province	== "SAHEL" &	district	== "DORI" ~	"DORI",
      country	== "BURKINA FASO" &
        province	== "NORD" &	district	== "OUAHIGOUYA" ~	"OUAHIGOUYA",
      country	== "BURKINA FASO" &
        province	== "NORD" &	district	== "SEGUENEGA" ~	"OUAHIGOUYA",
      country	== "BURKINA FASO" &
        province	== "NORD" &	district	== "TITAO" ~	"OUAHIGOUYA",
      country	== "BURKINA FASO" &
        province	== "SAHEL" &	district	== "DJIBO" ~	"DORI",
      country	== "BURKINA FASO" &
        province	== "SAHEL" &	district	== "GOROM" ~	"DORI",
      country	== "BURKINA FASO" &
        province	== "SAHEL" &	district	== "SEBBA" ~	"DORI",
      country	== "BURKINA FASO" &
        province	== "BOUCLE_DU_MOUHOUN" &	district	== "DEDOUGOU" ~	"DEDOUGOU",
      country	== "BURKINA FASO" &
        province	== "BOUCLE_DU_MOUHOUN" &	district	== "SOLENZO" ~	"DEDOUGOU",
      country	== "BURKINA FASO" &
        province	== "BOUCLE_DU_MOUHOUN" &	district	== "TOMA" ~	"DEDOUGOU",
      country	== "BURKINA FASO" &
        province	== "CASCADES" &	district	== "SINDOU" ~	"BANFORA",
      country	== "BURKINA FASO" &
        province	== "HAUTS-BASSINS" &	district	== "HOUNDE" ~	"BOBO",
      country	== "BURKINA FASO" &
        province	== "HAUTS-BASSINS" &	district	== "KARANGASSO-VIGUE" ~	"BOBO",
      country	== "BURKINA FASO" &
        province	== "SUD-OUEST" &	district	== "DANO" ~	"GAOUA",
      country	== "BURKINA FASO" &
        province	== "BOUCLE_DU_MOUHOUN" &	district	== "BOROMO" ~	"DEDOUGOU",
      country	== "BURKINA FASO" &
        province	== "HAUTS-BASSINS" &	district	== "DAFRA" ~	"BOBO",
      country	== "BURKINA FASO" &
        province	== "SUD-OUEST" &	district	== "GAOUA" ~	"GAOUA",
      country	== "BURKINA FASO" &
        province	== "CASCADES" &	district	== "MANGODARA" ~	"BANFORA",
      country	== "BURKINA FASO" &
        province	== "HAUTS-BASSINS" &	district	== "DANDE" ~	"BOBO",
      country	== "BURKINA FASO" &
        province	== "CASCADES" &	district	== "BANFORA" ~	"BANFORA",
      country	== "BURKINA FASO" &
        province	== "HAUTS-BASSINS" &	district	== "N'DOROLA" ~	"BOBO",
      country	== "BURKINA FASO" &
        province	== "BOUCLE_DU_MOUHOUN" &	district	== "NOUNA" ~	"DEDOUGOU",
      country	== "BURKINA FASO" &
        province	== "BOUCLE_DU_MOUHOUN" &	district	== "TOUGAN" ~	"DEDOUGOU",
      country	== "BURKINA FASO" &
        province	== "SUD-OUEST" &	district	== "BATIE" ~	"GAOUA",
      country	== "BURKINA FASO" &
        province	== "SUD-OUEST" &	district	== "DIEBOUGOU" ~	"GAOUA",
      country	== "BURKINA FASO" &
        province	== "SUD-OUEST" &	district	== "KAMPTI" ~	"GAOUA",
      country	== "BURKINA FASO" &
        province	== "HAUTS-BASSINS" &	district	== "DO" ~	"BOBO",
      country	== "BURKINA FASO" &
        province	== "HAUTS-BASSINS" &	district	== "LENA" ~	"BOBO",
      country	== "BURKINA FASO" &
        province	== "HAUTS-BASSINS" &	district	== "ORODARA" ~	"BOBO",
      country	== "MAURITANIA" &
        province	== "HODH EL GHARBI" &
        district	== "TAMCHAKET" ~	"HODH EL GHARBI",
      country	== "MAURITANIA" &
        province	== "ASSABA" &	district	== "GUÉRROU" ~	"ASSABA",
      country	== "MAURITANIA" &
        province	== "TIRIS ZEMMOUR" &
        district	== "F'DERICK" ~	"TIRIS ZEMMOUR",
      country	== "MAURITANIA" &
        province	== "TRARZA" &	district	== "ROSSO" ~	"TRARZA",
      country	== "MAURITANIA" &
        province	== "HODH EL GHARBI" &
        district	== "TINTANE" ~	"HODH EL GHARBI",
      country	== "MAURITANIA" &
        province	== "HODH EL CHARGUI" &
        district	== "BASSIKNOU" ~	"HODH ECHARGUI",
      country	== "MAURITANIA" &
        province	== "TRARZA" &	district	== "MEDERDRA" ~	"TRARZA",
      country	== "MAURITANIA" &
        province	== "BRAKNA" &	district	== "ALEG" ~	"BRAKNA",
      country	== "MAURITANIA" &
        province	== "HODH EL CHARGUI" &
        district	== "AMOURJ" ~	"HODH ECHARGUI",
      country	== "MAURITANIA" &
        province	== "HODH EL CHARGUI" &
        district	== "DJIGUENI" ~	"HODH ECHARGUI",
      country	== "MAURITANIA" &
        province	== "NOUAKCHOTT NORD" &	district	== "TEYARETT" ~	"NOUAKCHOTT",
      country	== "MAURITANIA" &
        province	== "ASSABA" &	district	== "BARKÉOLE" ~	"ASSABA",
      country	== "MAURITANIA" &
        province	== "ASSABA" &	district	== "KIFFA" ~	"ASSABA",
      country	== "MAURITANIA" &
        province	== "BRAKNA" &	district	== "BABABÉ" ~	"BRAKNA",
      country	== "MAURITANIA" &
        province	== "BRAKNA" &	district	== "BOGHÉ" ~	"BRAKNA",
      country	== "MAURITANIA" &
        province	== "DAKHLET NOUADHIBOU" &
        district	== "NOUADHIBOU" ~	"DAKHLET NOUADHIBOU",
      country	== "MAURITANIA" &
        province	== "GORGOL" &	district	== "KAEDI" ~	"GORGOL",
      country	== "MAURITANIA" &
        province	== "GORGOL" &	district	== "MAGHAMA" ~	"GORGOL",
      country	== "MAURITANIA" &
        province	== "GUIDIMAKHA" &	district	== "GHABOU" ~	"GUIODIMAKHA",
      country	== "MAURITANIA" &
        province	== "GUIDIMAKHA" &	district	== "OULD YENGE" ~	"GUIODIMAKHA",
      country	== "MAURITANIA" &
        province	== "GUIDIMAKHA" &	district	== "SELIBABY" ~	"GUIODIMAKHA",
      country	== "MAURITANIA" &
        province	== "HODH EL GHARBI" &	district	== "AIOUN" ~	"HODH EL GHARBI",
      country	== "MAURITANIA" &
        province	== "HODH EL CHARGUI" &	district	== "NÉMA" ~	"HODH ECHARGUI",
      country	== "MAURITANIA" &
        province	== "INCHIRI" &	district	== "AKJOUJT" ~	"INCHIRI",
      country	== "MAURITANIA" &
        province	== "NOUAKCHOTT OUEST" &
        district	== "TEVRAGH ZEINA" ~	"NOUAKCHOTT",
      country	== "MAURITANIA" &
        province	== "NOUAKCHOTT SUD" &
        district	== "ARAFAT" ~	"NOUAKCHOTT SUD",
      country	== "MAURITANIA" &
        province	== "NOUAKCHOTT SUD" &
        district	== "EL MINA" ~	"NOUAKCHOTT SUD",
      country	== "MAURITANIA" &
        province	== "NOUAKCHOTT SUD" &	district	== "RIYAD" ~	"NOUAKCHOTT SUD",
      country	== "MAURITANIA" &
        province	== "TIRIS ZEMMOUR" &
        district	== "BIR MOGHREN" ~	"TIRIS ZEMMOUR",
      country	== "MAURITANIA" &
        province	== "TRARZA" &	district	== "OUAD NAGA" ~	"TRARZA",
      country	== "MAURITANIA" &
        province	== "ADRAR" &	district	== "ATAR" ~	"ADRAR",
      country	== "MAURITANIA" &
        province	== "BRAKNA" &	district	== "MAGTA LAHJAR" ~	"BRAKNA",
      country	== "MAURITANIA" &
        province	== "DAKHLET NOUADHIBOU" &
        district	== "CHAMI" ~	"DAKHLET NOUADHIBOU",
      country	== "MAURITANIA" &
        province	== "HODH EL GHARBI" &
        district	== "KOBENI" ~	"HODH EL GHARBI",
      country	== "MAURITANIA" &
        province	== "NOUAKCHOTT OUEST" &
        district	== "KSAR" ~	"NOUAKCHOTT OUEST",
      country	== "MAURITANIA" &
        province	== "TAGANT" &	district	== "TIDJIKJA" ~	"TAGANT",
      country	== "MAURITANIA" &
        province	== "ASSABA" &	district	== "BOUMDEID" ~	"ASSABA",
      country	== "MAURITANIA" &
        province	== "ADRAR" &	district	== "AOUJEFT" ~	"ADRAR",
      country	== "MAURITANIA" &
        province	== "ADRAR" &	district	== "CHINGUITTY" ~	"ADRAR",
      country	== "MAURITANIA" &
        province	== "ADRAR" &	district	== "OUADANE" ~	"ADRAR",
      country	== "MAURITANIA" &
        province	== "ASSABA" &	district	== "KANKOSSA" ~	"ASSABA",
      country	== "MAURITANIA" &
        province	== "GORGOL" &	district	== "LEXEIBA" ~	"GORGOL",
      country	== "MAURITANIA" &
        province	== "GORGOL" &	district	== "M'BOUT" ~	"GORGOL",
      country	== "MAURITANIA" &
        province	== "GORGOL" &	district	== "MONGUEL" ~	"GORGOL",
      country	== "MAURITANIA" &
        province	== "GUIDIMAGHA" &	district	== "WOMPO" ~	"GUIODIMAKHA",
      country	== "MAURITANIA" &
        province	== "HODH ECHARGHI" &
        district	== "ADEL BEGROU" ~	"HODH ECHARGUI",
      country	== "MAURITANIA" &
        province	== "INCHIRI" &	district == "BENECHABE" ~	"INCHIRI",
      country	== "MAURITANIA" &
        province	== "TAGANT" &	district	== "TICHIT" ~	"TAGANT",
      country	== "MAURITANIA" &
        province	== "BRAKNA" &	district	== "M'BAGNE" ~	"BRAKNA",
      country	== "MAURITANIA" &
        province	== "BRAKNA" &	district	== "MALE" ~	"BRAKNA",
      country	== "MAURITANIA" &
        province	== "TAGANT" &	district	== "MOUDJÉRIA" ~	"TAGANT",
      country	== "MAURITANIA" &
        province	== "TRARZA" &	district	== "R'KIZ" ~	"TRARZA",
      country	== "MAURITANIA" &
        province	== "HODH ECHARGHI" &
        district	== "TIMBÉDRA" ~	"HODH ECHARGUI",
      country	== "MAURITANIA" &
        province	== "NOUAKCHOTT NORD" &
        district	== "DAR NAIM" ~	"NOUAKCHOTT NORD",
      country	== "MAURITANIA" &
        province	== "NOUAKCHOTT NORD" &
        district	== "TOUJOUNINE" ~	"NOUAKCHOTT",
      country	== "MAURITANIA" &
        province	== "NOUAKCHOTT OUEST" &	district	== "SEBKHA" ~	"NOUAKCHOTT",
      country	== "MAURITANIA" &
        province	== "TIRIS EZMMOUR" &	district	== "ZOUÉRAT" ~	"TIRIS ZEMMOUR",
      country	== "MAURITANIA" &
        province	== "TRARZA" &	district	== "BOUTILIMIT" ~	"TRARZA",
      country	== "MAURITANIA" &
        province	== "TRARZA" &	district	== "KEUR MACENE" ~	"TRARZA",
      country	== "MAURITANIA" &
        province	== "HODH EL GHARBI" &	district	== "TOUIL" ~	"HODH EL GHARBI",
      country	== "MAURITANIA" &
        province	== "TRARZA" &	district	== "TEIKANE" ~	"TRARZA",
      country	== "MAURITANIA" &
        province	== "HODH ECHARGHI" &	district	== "D'HAR" ~	"HODH ECHARGUI",
      country	== "MAURITANIA" &
        province	== "HODH ECHARGHI" &	district	== "OUALATA" ~	"HODH ECHARGUI",
      country	== "MALI" &
        province	== "GAO" &	district	== "ALMOUSTARAT" ~	"GAO",
      country	== "MALI" &	province	== "GAO" &
        district	== "ANSONGO" ~	"GAO",
      country	== "MALI" &	province	== "GAO" &	district	== "BOUREM" ~	"GAO",
      country	== "MALI" &
        province	== "MENAKA" &	district	== "INEKAR" ~	"MENAKA",
      country	== "MALI" &	province	== "GAO" &	district	== "GAO" ~	"GAO",
      country	== "MALI" &
        province	== "MENAKA" &	district	== "ANDERAMBOUKANE" ~	"MENAKA",
      country	== "MALI" &
        province	== "MENAKA" &	district	== "MENAKA" ~	"GAO",
      country	== "MALI" &
        province	== "MENAKA" &	district	== "TIDERMENE" ~	"MENAKA",
      country	== "MALI" &	province	== "SEGOU" &
        district	== "SAN" ~	"SÉGOU",
      country	== "MALI" &
        province	== "KOULIKORO" &	district	== "OUELESSEBOUGOU" ~	"KOULIKORO",
      country	== "MALI" &
        province	== "MOPTI" &	district	== "DJENNÉ" ~	"MOPTI",
      country	== "MALI" &
        province	== "SEGOU" &	district	== "NIONO" ~	"SÉGOU",
      country	== "MALI" &
        province	== "SEGOU" &	district	== "MARKALA" ~	"SÉGOU",
      country	== "MALI" &
        province	== "SIKASSO" &	district	== "KOLONDIEBA" ~	"SIKASSO",
      country	== "MALI" &	province	== "KAYES" &
        district	== "KITA" ~	"KAYES",
      country	== "MALI" &
        province	== "KAYES" &	district	== "SAGABARI" ~	"KAYES",
      country	== "MALI" &
        province	== "KOULIKORO" &	district	== "KOULIKORO" ~	"KOULIKORO",
      country	== "MALI" &
        province	== "MOPTI" &	district	== "DOUENTZA" ~	"MOPTI",
      country	== "MALI" &
        province	== "SIKASSO" &	district	== "BOUGOUNI" ~	"SIKASSO",
      country	== "MALI" &
        province	== "KAYES" &	district	== "KENIEBA" ~	"KAYES",
      country	== "MALI" &
        province	== "KOULIKORO" &	district	== "DIOILA" ~	"KOULIKORO",
      country	== "MALI" &
        province	== "KOULIKORO" &	district	== "FANA" ~	"KOULIKORO",
      country	== "MALI" &
        province	== "KOULIKORO" &	district	== "KALABANCORO" ~	"KOULIKORO",
      country	== "MALI" &
        province	== "KOULIKORO" &	district	== "KANGABA" ~	"KOULIKORO",
      country	== "MALI" &
        province	== "KOULIKORO" &	district	== "KATI" ~	"KOULIKORO",
      country	== "MALI" &
        province	== "KOULIKORO" &	district	== "KATI" ~	"KOULIKORO",
      country	== "MALI" &
        province	== "MOPTI" &	district	== "BANDIAGARA" ~	"MOPTI",
      country	== "MALI" &
        province	== "MOPTI" &	district	== "BANKASS" ~	"MOPTI",
      country	== "MALI" &	province	== "MOPTI" &
        district	== "KORO" ~	"MOPTI",
      country	== "MALI" &
        province	== "MOPTI" &	district	== "MOPTI" ~	"MOPTI",
      country	== "MALI" &
        province	== "MOPTI" &	district	== "TENENKOU" ~	"MOPTI",
      country	== "MALI" &
        province	== "SEGOU" &	district	== "BAROUELI" ~	"SÉGOU",
      country	== "MALI" &	province	== "SEGOU" &
        district	== "BLA" ~	"SÉGOU",
      country	== "MALI" &
        province	== "SEGOU" &	district	== "MACINA" ~	"SÉGOU",
      country	== "MALI" &
        province	== "SEGOU" &	district	== "SEGOU" ~	"SÉGOU",
      country	== "MALI" &
        province	== "SEGOU" &	district	== "TOMINIAN" ~	"SÉGOU",
      country	== "MALI" &
        province	== "SIKASSO" &	district	== "KADIOLO" ~	"SIKASSO",
      country	== "MALI" &
        province	== "SIKASSO" &	district	== "KIGNAN" ~	"SIKASSO",
      country	== "MALI" &
        province	== "SIKASSO" &	district	== "NIENA" ~	"SIKASSO",
      country	== "MALI" &
        province	== "SIKASSO" &	district	== "SELINGUE" ~	"SIKASSO",
      country	== "MALI" &
        province	== "SIKASSO" &	district	== "YANFOLILA" ~	"SIKASSO",
      country	== "MALI" &
        province	== "SIKASSO" &	district	== "YOROSSO" ~	"SIKASSO",
      country	== "MALI" &
        province	== "TOMBOUCTOU" &
        district	== "GOURMA-RHAROUS" ~	"TOMBOUCTOU",
      country	== "MALI" &
        province	== "BAMAKO" &	district	== "COMMUNE I" ~	"BAMAKO",
      country	== "MALI" &
        province	== "BAMAKO" &	district	== "COMMUNE II" ~	"BAMAKO",
      country	== "MALI" &
        province	== "BAMAKO" &	district	== "COMMUNE III" ~	"BAMAKO",
      country	== "MALI" &
        province	== "BAMAKO" &	district	== "COMMUNE IV" ~	"BAMAKO",
      country	== "MALI" &
        province	== "BAMAKO" &	district	== "COMMUNE V" ~	"BAMAKO",
      country	== "MALI" &
        province	== "BAMAKO" &	district	== "COMMUNE VI" ~	"BAMAKO",
      country	== "MALI" &
        province	== "SIKASSO" &	district	== "KOUTIALA" ~	"SIKASSO",
      country	== "MALI" &
        province	== "MOPTI" &	district	== "YOUWAROU" ~	"MOPTI",
      country	== "MALI" &
        province	== "SIKASSO" &	district	== "SIKASSO" ~	"SIKASSO",
      country	== "MALI" &
        province	== "KOULIKORO" &	district	== "BANAMBA" ~	"KOULIKORO",
      country	== "MALI" &
        province	== "KAYES" &	district	== "YELIMANE" ~	"KAYES",
      country	== "MALI" &
        province	== "KAYES" &	district	== "NIORO" ~	"KAYES",
      country	== "MALI" &
        province	== "KOULIKORO" &	district	== "NARA" ~	"KOULIKORO",
      country	== "MALI" &
        province	== "TOMBOUCTOU" &	district	== "DIRE" ~	"TOMBOUCTOU",
      country	== "MALI" &
        province	== "KAYES" &	district	== "BAFOULABE" ~	"KAYES",
      country	== "MALI" &
        province	== "KAYES" &	district	== "DIEMA" ~	"KAYES",
      country	== "MALI" &
        province	== "KAYES" &	district	== "OUSSOUBIDIAGNA" ~	"KAYES",
      country	== "MALI" &
        province	== "KAYES" &	district	== "SEFETO" ~	"KAYES",
      country	== "MALI" &
        province	== "KOULIKORO" &	district	== "KOLOKANI" ~	"KOULIKORO",
      country	== "MALI" &
        province	== "TOMBOUCTOU" &	district	== "NIAFUNKE" ~	"TOMBOUCTOU",
      country	== "MALI" &
        province	== "KAYES" &	district	== "KAYES" ~	"KAYES",
      country	== "MALI" &
        province	== "TAOUDENI" &	district	== "ACHOURATT" ~	"TAOUDENIT",
      country	== "MALI" &
        province	== "TAOUDENI" &	district	== "AL-OURCHE" ~	"TAOUDENIT",
      country	== "MALI" &
        province	== "TAOUDENI" &	district	== "BOUJBEHA" ~	"TAOUDENIT",
      country	== "MALI" &
        province	== "TOMBOUCTOU" &	district	== "GOUNDAM" ~	"TOMBOUCTOU",
      country	== "MALI" &
        province	== "TOMBOUCTOU" &	district	== "TOMBOUCTOU" ~	"TOMBOUCTOU",
      country	== "MALI" &
        province	== "KIDAL" &	district	== "KIDAL" ~	"KIDAL",
      country	== "MALI" &
        province	== "TAOUDENIT" &	district	== "ARAWANE" ~	"TAOUDENIT",
      country	== "MALI" &
        province	== "TAOUDENIT" &	district	== "TAOUDENI" ~	"TAOUDENIT",
      country	== "MALI" &
        province	== "KIDAL" &	district	== "ABEIBARA" ~	"KIDAL",
      country	== "MALI" &
        province	== "TAOUDENIT" &	district	== "FOUM-ALBA" ~	"TAOUDENIT",
      country	== "MALI" &
        province	== "KIDAL" &	district	== "TESSALIT" ~	"KIDAL",
      country	== "MALI" &
        province	== "KIDAL" &	district	== "TINESSAKO" ~	"KIDAL",
      country	== "NIGER" &
        province	== "DOSSO" &	district	== "DOSSO" ~	"DOSSO",
      country	== "NIGER" &
        province	== "DOSSO" &	district	== "FALMEY" ~	"DOSSO",
      country	== "NIGER" &
        province	== "DOSSO" &	district	== "DIOUNDOU" ~	"DOSSO",
      country	== "NIGER" &
        province	== "DOSSO" &	district	== "GAYA" ~	"DOSSO",
      country	== "NIGER" &
        province	== "DOSSO" &	district	== "LOGA" ~	"DOSSO",
      country	== "NIGER" &
        province	== "TILLABERI" &	district	== "KOLLO" ~	"TILLABTRI",
      country	== "NIGER" &
        province	== "TILLABERI" &	district	== "TILLABERI" ~	"TILLABTRI",
      country	== "NIGER" &
        province	== "DOSSO" &	district	== "DOGON DOUTCHI" ~	"DOSSO",
      country	== "NIGER" &
        province	== "DOSSO" &	district	== "TIBIRI" ~	"DOSSO",
      country	== "NIGER" &
        province	== "NIAMEY" &	district	== "NIAMEY 1" ~	"NIAMEY",
      country	== "NIGER" &
        province	== "NIAMEY" &	district	== "NIAMEY 2" ~	"NIAMEY",
      country	== "NIGER" &
        province	== "NIAMEY" &	district	== "NIAMEY 4" ~	"NIAMEY",
      country	== "NIGER" &
        province	== "NIAMEY" &	district	== "NIAMEY 5" ~	"NIAMEY",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "BAGAROUA" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "BIRNI N'KONNI" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "BOUZA" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "ILLÉLA" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "KEITA" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "MADAOUA" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "MALBAZA" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "TAHOUA DEP" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "TILLABERI" &	district	== "BALLEYARA" ~	"TILLABERI",
      country	== "NIGER" &
        province	== "DOSSO" &	district	== "BOBOYE" ~	"DOSSO",
      country	== "NIGER" &
        province	== "NIAMEY" &	district	== "NIAMEY 3" ~	"NIAMEY",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "TAHOUA COM" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "TASSARA" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "ZINDER" &	district	== "TAKEITA" ~	"ZINDER",
      country	== "NIGER" &
        province	== "MARADI" &	district	== "DAKORO" ~	"MARADI",
      country	== "NIGER" &
        province	== "MARADI" &	district	== "GAZAOUA" ~	"MARADI",
      country	== "NIGER" &
        province	== "MARADI" &	district	== "G. ROUMDJI" ~	"MARADI",
      country	== "NIGER" &
        province	== "MARADI" &	district	== "MADAROUNFA" ~	"MARADI",
      country	== "NIGER" &
        province	== "MARADI" &	district	== "MAYAHI" ~	"MARADI",
      country	== "NIGER" &
        province	== "MARADI" &	district	== "TESSAOUA" ~	"MARADI",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "ABALAK" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "TCHINTABARADEN" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "ZINDER" &	district	== "KANTCHÉ" ~	"ZINDER",
      country	== "NIGER" &
        province	== "MARADI" &	district	== "AGUIÉ" ~	"MARADI",
      country	== "NIGER" &
        province	== "MARADI" &	district	== "MARADI VILLE" ~	"MARADI",
      country	== "NIGER" &
        province	== "MARADI" &	district	== "AGUIE" ~	"MARADI",
      country	== "NIGER" &
        province	== "MARADI" &	district	== "BERMO" ~	"MARADI",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "ILLELA" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "ZINDER" &	district	== "ZINDER" ~	"ZINDER",
      country	== "NIGER" &
        province	== "ZINDER" &	district	== "DAMAGARAM TAKAYA" ~ "ZINDER",
      country	== "NIGER" &
        province	== "ZINDER" &	district	== "DUNGASS" ~	"ZINDER",
      country	== "NIGER" &
        province	== "ZINDER" &	district	== "GOURE" ~	"ZINDER",
      country	== "NIGER" &
        province	== "ZINDER" &	district	== "MAGARIA" ~	"ZINDER",
      country	== "NIGER" &
        province	== "ZINDER" &	district	== "MIRRIAH" ~	"ZINDER",
      country	== "NIGER" &
        province	== "ZINDER" &	district	== "TANOUT" ~	"ZINDER",
      country	== "NIGER" &
        province	== "ZINDER" &	district	== "BELBEDJI" ~	"ZINDER",
      country	== "NIGER" &
        province	== "ZINDER" &	district	== "TESKER" ~	"ZINDER",
      country	== "NIGER" &
        province	== "AGADEZ" &	district	== "ADERBISSANAT" ~	"AGADEZ",
      country	== "NIGER" &
        province	== "AGADEZ" &	district	== "AGADEZ" ~	"AGADEZ",
      country	== "NIGER" &
        province	== "AGADEZ" &	district	== "ARLIT" ~	"AGADEZ",
      country	== "NIGER" &
        province	== "AGADEZ" &	district	== "IFÉROUANE" ~	"AGADEZ",
      country	== "NIGER" &
        province	== "AGADEZ" &	district	== "INGALL" ~	"AGADEZ",
      country	== "NIGER" &
        province	== "AGADEZ" &	district	== "TCHIROZÉRINE" ~	"AGADEZ",
      country	== "NIGER" &
        province	== "DIFFA" &	district	== "DIFFA" ~	"DIFFA",
      country	== "NIGER" &
        province	== "DIFFA" &	district	== "GOUDOUMARIA" ~	"DIFFA",
      country	== "NIGER" &
        province	== "DIFFA" &	district	== "MAINE SOROA" ~	"DIFFA",
      country	== "NIGER" &
        province	== "DIFFA" &	district	== "N'GUIGMI" ~	"DIFFA",
      country	== "NIGER" &
        province	== "TILLABERI" &	district	== "SAY" ~	"TILLABTRI",
      country	== "NIGER" &
        province	== "DIFFA" &	district	== "N'GOURTI" ~	"DIFFA",
      country	== "NIGER" &
        province	== "DIFFA" &	district	== "NGUIGMI" ~	"DIFFA",
      country	== "NIGER" &
        province	== "TAHOUA" &	district	== "BIRNI NKONNI" ~	"TAHOUA",
      country	== "NIGER" &
        province	== "DIFFA" &	district	== "BOSSO" ~	"DIFFA",
      country	== "CHAD" &
        province	== "MANDOUL" &	district	== "GOUNDI" ~	"MANDOUL",
      country	== "CHAD" &
        province	== "LOGONE_OCCIDENTAL" &
        district	== "BENOYE" ~	"LOGONE OCCIDENTAL",
      country	== "CHAD" &
        province	== "MOYEN_CHARI" &	district	== "DANAMADJI" ~	"MOYEN CHARI",
      country	== "CHAD" &
        province	== "LOGONE_OCCIDENTAL" &
        district	== "BEINAMAR" ~	"LOGONE OCCIDENTAL",
      country	== "CHAD" &
        province	== "LOGONE_ORIENTAL" &
        district	== "BEBEDJIA" ~	"LOGONE ORIENTAL",
      country	== "CHAD" &
        province	== "LOGONE_ORIENTAL" &
        district	== "DONIA" ~	"LOGONE ORIENTAL",
      country	== "CHAD" &
        province	== "MANDOUL" &	district	== "BEDJONDO" ~	"MANDOUL",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_OUEST" &
        district	== "GAGAL" ~	"MAYO KEBBI OUEST",
      country	== "CHAD" &
        province	== "KANEM" &	district	== "MONDO" ~	"KANEM",
      country	== "CHAD" &
        province	== "LOGONE_ORIENTAL" &
        district	== "BODO" ~	"LOGONE ORIENTAL",
      country	== "CHAD" &
        province	== "MANDOUL" &	district	== "KOUMRA" ~	"MANDOUL",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_EST" &	district	== "YOUE" ~	"MAYO KEBBI EST",
      country	== "CHAD" &
        province	== "MOYEN_CHARI" &	district	== "KOUMOGO" ~	"MOYEN CHARI",
      country	== "CHAD" &
        province	== "TANDJILE" &	district	== "BERE" ~	"TANDJILE",
      country	== "CHAD" &
        province	== "BARH_EL_GAZAL" &	district	== "CHADRA" ~	"BARH EL GAZEL",
      country	== "CHAD" &
        province	== "BARH_EL_GAZAL" &
        district	== "MICHEMIRE" ~	"BAHR ELGHAZAL",
      country	== "CHAD" &
        province	== "BARH_EL_GAZAL" &	district	== "MOUSSORO" ~	"KANEM",
      country	== "CHAD" &
        province	== "BARH_EL_GAZAL" &	district	== "SALAL" ~	"BAHR ELGHAZAL",
      country	== "CHAD" &
        province	== "BATHA" &	district	== "ASSINET" ~	"BATHA",
      country	== "CHAD" &	province	== "BATHA" &
        district	== "ATI" ~	"BATHA",
      country	== "CHAD" &
        province	== "BATHA" &	district	== "DJEDDA" ~	"BATHA",
      country	== "CHAD" &
        province	== "BATHA" &	district	== "KOUNDJOUROU" ~	"BATHA",
      country	== "CHAD" &
        province	== "BATHA" &	district	== "OUM HADJER" ~	"BATHA",
      country	== "CHAD" &
        province	== "DAR_SILA" &	district	== "GOZ BEIDA" ~	"SILA",
      country	== "CHAD" &
        province	== "DAR_SILA" &	district	== "KOUKOU ANGARANA" ~	"SILA",
      country	== "CHAD" &
        province	== "DAR_SILA" &	district	== "TISSI" ~	"SILA",
      country	== "CHAD" &
        province	== "HADJER_LAMIS" &	district	== "KARAL" ~	"HADJER LAMIS",
      country	== "CHAD" &
        province	== "HADJER_LAMIS" &	district	== "MANI" ~	"HADJER LAMIS",
      country	== "CHAD" &	province	== "KANEM" &
        district	== "MAO" ~	"KANEM",
      country	== "CHAD" &
        province	== "KANEM" &	district	== "NOKOU" ~	"KANEM",
      country	== "CHAD" &
        province	== "KANEM" &	district	== "N'TIONA" ~	"KANEM",
      country	== "CHAD" &
        province	== "KANEM" &	district	== "RIG RIG" ~	"KANEM",
      country	== "CHAD" &
        province	== "LAC" &	district	== "BAGASSOLA" ~	"LAC",
      country	== "CHAD" &	province	== "LAC" &
        district	== "ISSEIROM" ~	"LAC",
      country	== "CHAD" &
        province	== "LAC" &	district	== "KOULOUDIA" ~	"LAC",
      country	== "CHAD" &	province	== "LAC" &	district	== "NGOURI" ~	"LAC",
      country	== "CHAD" &
        province	== "LOGONE_OCCIDENTAL" &
        district	== "LAOKASSY" ~	"LOGONE OCCIDENTAL",
      country	== "CHAD" &
        province	== "LOGONE_OCCIDENTAL" &
        district	== "MOUNDOU" ~	"LOGONE OCCIDENTAL",
      country	== "CHAD" &
        province	== "LOGONE_ORIENTAL" &
        district	== "BESSAO" ~	"LOGONE ORIENTAL",
      country	== "CHAD" &
        province	== "LOGONE_ORIENTAL" &
        district	== "DOBA" ~	"LOGONE ORIENTAL",
      country	== "CHAD" &
        province	== "LOGONE_ORIENTAL" &
        district	== "GORE" ~	"LOGONE ORIENTAL",
      country	== "CHAD" &
        province	== "LOGONE_ORIENTAL" &
        district	== "KARA" ~	"LOGONE ORIENTAL",
      country	== "CHAD" &
        province	== "LOGONE_ORIENTAL" &
        district	== "LARMANAYE" ~	"LOGONE ORIENTAL",
      country	== "CHAD" &
        province	== "MANDOUL" &	district	== "BEDAYA" ~	"MANDOUL",
      country	== "CHAD" &
        province	== "MANDOUL" &	district	== "MOISSALA" ~	"MANDOUL",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_EST" &
        district	== "FIANGA" ~	"MAYO KEBBI EST",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_EST" &
        district	== "GOUNOU GAYA" ~	"MAYO KEBBI EST",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_EST" &
        district	== "MOULKOU" ~	"MAYO KEBBI EST",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_OUEST" &
        district	== "BINDER" ~	"MAYO KEBBI OUEST",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_OUEST" &
        district	== "GUEGOU" ~	"MAYO KEBBI OUEST",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_OUEST" &
        district	== "GUELAO" ~	"MAYO KEBBI OUEST",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_OUEST" &
        district	== "LAGON" ~	"MAYO KEBBI OUEST",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_OUEST" &
        district	== "LAME" ~	"MAYO KEBBI OUEST",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_OUEST" &
        district	== "LERE" ~	"MAYO KEBBI OUEST",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_OUEST" &
        district	== "TORROCK" ~	"MAYO KEBBI OUEST",
      country	== "CHAD" &
        province	== "MOYEN_CHARI" &
        district	== "BIOBE SINGAKO" ~	"MOYEN CHARI",
      country	== "CHAD" &
        province	== "MOYEN_CHARI" &	district	== "KORBOL" ~	"MOYEN CHARI",
      country	== "CHAD" &
        province	== "MOYEN_CHARI" &	district	== "KYABE" ~	"MOYEN CHARI",
      country	== "CHAD" &
        province	== "MOYEN_CHARI" &	district	== "MARO" ~	"MOYEN CHARI",
      country	== "CHAD" &
        province	== "MOYEN_CHARI" &	district	== "SARH" ~	"MOYEN CHARI",
      country	== "CHAD" &
        province	== "OUADDAI" &	district	== "ABECHE" ~	"OUADDAI",
      country	== "CHAD" &
        province	== "OUADDAI" &	district	== "ABOUGOUDAM" ~	"OUADDAI",
      country	== "CHAD" &
        province	== "OUADDAI" &	district	== "ADRE" ~	"OUADDAI",
      country	== "CHAD" &
        province	== "OUADDAI" &	district	== "AMDAM" ~	"OUADDAI",
      country	== "CHAD" &
        province	== "SALAMAT" &	district	== "AM TIMAN" ~	"SALAMAT",
      country	== "CHAD" &
        province	== "TANDJILE" &	district	== "BAKCTCHORO" ~	"TANDJILE",
      country	== "CHAD" &
        province	== "TANDJILE" &	district	== "DAFRA" ~	"TANDJILE",
      country	== "CHAD" &
        province	== "TANDJILE" &	district	== "DONOMANGA" ~	"TANDJILE",
      country	== "CHAD" &
        province	== "TANDJILE" &	district	== "KELO" ~	"TANDJILE",
      country	== "CHAD" &
        province	== "TANDJILE" &	district	== "KOLON" ~	"TANDJILE",
      country	== "CHAD" &
        province	== "TANDJILE" &	district	== "LAI" ~	"TANDJILE",
      country	== "CHAD" &
        province	== "WADI_FIRA" &	district	== "ARADA" ~	"WADI FIRA",
      country	== "CHAD" &
        province	== "WADI_FIRA" &	district	== "BILTINE" ~	"WADI FIRA",
      country	== "CHAD" &
        province	== "WADI_FIRA" &	district	== "IRIBA" ~	"WADI FIRA",
      country	== "CHAD" &
        province	== "WADI_FIRA" &	district	== "MATADJANA" ~	"WADI FIRA",
      country	== "CHAD" &
        province	== "DAR_SILA" &	district	== "ABDI" ~	"QUADDAI",
      country	== "CHAD" &
        province	== "HADJER_LAMIS" &	district	== "BOKORO" ~	"HADJAR LAMIS",
      country	== "CHAD" &	province	== "LAC" &	district	== "BOL" ~	"LAC",
      country	== "CHAD" &	province	== "LAC" &	district	== "LIWA" ~	"LAC",
      country	== "CHAD" &
        province	== "MANDOUL" &	district	== "BEKOUROU" ~	"MANDOUL",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_EST" &
        district	== "PONT CAROL" ~	"MAYO KEBBI EST",
      country	== "CHAD" &
        province	== "WADI_FIRA" &	district	== "AMZOER" ~	"WADI FIRA",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_EST" &
        district	== "BONGOR" ~	"MAYO KEBBI EST",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_OUEST" &
        district	== "PALA" ~	"MAYO KEBBI OUEST",
      country	== "CHAD" &
        province	== "SALAMAT" &	district	== "HARAZE MANGUEIGNE" ~	"SALAMAT",
      country	== "CHAD" &
        province	== "TANDJILE" &	district	== "GUIDARI" ~	"TANDJILE",
      country	== "CHAD" &
        province	== "GUERA" &	district	== "MANGALME" ~	"GUERA",
      country	== "CHAD" &
        province	== "BATHA" &	district	== "ALIFA" ~	"BATHA",
      country	== "CHAD" &
        province	== "NDJAMENA" &	district	== "N'DJAMENA SUD" ~	"N'DJAMENA",
      country	== "CHAD" &
        province	== "GUERA" &	district	== "MELFI" ~	"GUERA",
      country	== "CHAD" &
        province	== "NDJAMENA" &	district	== "N'DJAMENA NORD" ~	"N'DJAMENA",
      country	== "CHAD" &
        province	== "CHARI_BAGUIRMI" &
        district	== "MANDELIA" ~	"CHARI BAGUIRMI",
      country	== "CHAD" &	province	== "GUERA" &
        district	== "BARO" ~	"GUERA",
      country	== "CHAD" &
        province	== "GUERA" &	district	== "MONGO" ~	"GUERA",
      country	== "CHAD" &
        province	== "LOGONE_ORIENTAL" &
        district	== "BEBOTO" ~	"LOGONE ORIENTAL",
      country	== "CHAD" &
        province	== "NDJAMENA" &	district	== "N'DJAMENA CENTRE" ~	"N'DJAMENA",
      country	== "CHAD" &
        province	== "SALAMAT" &	district	== "ABOUDEIA" ~	"SALAMAT",
      country	== "CHAD" &
        province	== "BORKOU" &	district	== "FAYA" ~	"BORKOU-TIBESTI",
      country	== "CHAD" &
        province	== "CHARI_BAGUIRMI" &
        district	== "BOUSSO" ~	"CHARI BAGUIRMI",
      country	== "CHAD" &
        province	== "CHARI_BAGUIRMI" &
        district	== "DOURBALI" ~	"CHARI BAGUIRMI",
      country	== "CHAD" &
        province	== "CHARI_BAGUIRMI" &
        district	== "MASSENYA" ~	"CHARI BAGUIRMI",
      country	== "CHAD" &
        province	== "ENNEDI_EST" &	district	== "AMDJARASS" ~	"ENNEDI EST",
      country	== "CHAD" &
        province	== "ENNEDI_OUEST" &	district	== "FADA" ~	"ENNEDI QUEST",
      country	== "CHAD" &
        province	== "HADJER_LAMIS" &	district	== "MASSAGUET" ~	"HADJAR LAMIS",
      country	== "CHAD" &
        province	== "HADJER_LAMIS" &	district	== "MASSAKORY" ~	"HADJAR LAMIS",
      country	== "CHAD" &
        province	== "NDJAMENA" &
        district	== "N'DJAMENA EST" ~	"CHARI BAGUITMI",
      country	== "CHAD" &
        province	== "WADI_FIRA" &	district	== "GUEREDA" ~	"WADI FIRA",
      country	== "CHAD" &	province	== "BATHA" &
        district	== "YAO" ~	"BATHA",
      country	== "CHAD" &
        province	== "CHARI_BAGUIRMI" &
        district	== "BA ILLI" ~	"CHARI BAGUIRMI",
      country	== "CHAD" &
        province	== "CHARI_BAGUIRMI" &	district	== "KOUNO" ~	"CHARI BAGUIRMI",
      country	== "CHAD" &
        province	== "HADJER_LAMIS" &	district	== "GAMA" ~	"HADJER LAMIS",
      country	== "CHAD" &
        province	== "MANDOUL" &	district	== "BOUNA" ~	"MANDOUL",
      country	== "CHAD" &
        province	== "GUERA" &	district	== "BITKINE" ~	"GUERA",
      country	== "CHAD" &
        province	== "MAYO_KEBBI_EST" &
        district	== "GUELENDENG" ~	"MAYO KEBBI EST",
      country	== "CHAD" &
        province	== "LOGONE ORIENTAL" &
        district	== "BAIBOKOUM" ~	"LOGONE ORIENTAL",
      country	== "CHAD" &
        province	== "OUADDAI" &	district	== "AM DAM" ~	"SILA",
      country	== "CHAD" &
        province	== "TIBESTI" &	district	== "BARDAI" ~	"TIBESTI",
      country	== "UGANDA" &
        province	== "GREATER KAMPALA" &
        district	== "BUIKWE" ~	"NORTH BUGANDA",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "BUTALEJA" ~	"BUKEDI",
      country	== "UGANDA" &	province	== "ARUA" &
        district	== "MOYO" ~	"MOYO",
      country	== "UGANDA" &
        province	== "HOIMA" &	district	== "HOIMA" ~	"HOIMA",
      country	== "UGANDA" &
        province	== "JINJA" &	district	== "KAMULI" ~	"KAMULI",
      country	== "UGANDA" &
        province	== "JINJA" &	district	== "NAMUTUMBA" ~	"BUSOGA",
      country	== "UGANDA" &
        province	== "MASAKA" &	district	== "KALANGALA" ~	"SOUTH BUGANDA",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "TORORO" ~	"TORORO",
      country	== "UGANDA" &
        province	== "SOROTI" &	district	== "BUKEDEA" ~	"TESO",
      country	== "UGANDA" &
        province	== "ARUA" &	district	== "ARUA" ~	"MARACHA",
      country	== "UGANDA" &
        province	== "GREATER KAMPALA" &	district	== "KAMPALA" ~	"KAMPALA",
      country	== "UGANDA" &
        province	== "GULU" &	district	== "AGAGO" ~	"ACHOLI",
      country	== "UGANDA" &
        province	== "JINJA" &	district	== "BUGWERI" ~	"BUSOGA",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "KIRUHURA" ~	"ANKOLE",
      country	== "UGANDA" &
        province	== "MUBENDE" &	district	== "LUWERO" ~	"LUWERO",
      country	== "UGANDA" &
        province	== "GREATER KAMPALA" &
        district	== "BUVUMA" ~	"NORTH BUGANDA",
      country	== "UGANDA" &
        province	== "HOIMA" &	district	== "KIRYANDONGO" ~	"BUNYORO",
      country	== "UGANDA" &
        province	== "JINJA" &	district	== "JINJA" ~	"BUSOGA",
      country	== "UGANDA" &
        province	== "KABALE" &	district	== "RUBANDA" ~	"KIGEZI",
      country	== "UGANDA" &
        province	== "KABAROLE" &	district	== "KABAROLE" ~	"KAMWNGE",
      country	== "UGANDA" &
        province	== "KABAROLE" &	district	== "KASESE" ~	"KASESE",
      country	== "UGANDA" &
        province	== "MASAKA" &	district	== "LYANTONDE" ~	"SOUTH BUGANDA",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "BUKWO" ~	"ELGON",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "MBALE" ~	"MBALE",
      country	== "UGANDA" &
        province	== "SOROTI" &	district	== "KABERAMAIDO" ~	"TESO",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "MITOOMA" ~	"ANKOLE",
      country	== "UGANDA" &
        province	== "SOROTI" &	district	== "SOROTI" ~	"SOROTI",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "BUDAKA" ~	"BUKEDI",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "MBARARA" ~	"KIRUHURA",
      country	== "UGANDA" &
        province	== "KABAROLE" &	district	== "KYENJOJO" ~	"TORO",
      country	== "UGANDA" &
        province	== "SOROTI" &	district	== "KUMI" ~	"KUMI",
      country	== "UGANDA" &
        province	== "JINJA" &	district	== "BUYENDE" ~	"BUSOGA",
      country	== "UGANDA" &
        province	== "KABALE" &	district	== "KABALE" ~	"KABALE",
      country	== "UGANDA" &
        province	== "KABAROLE" &	district	== "NTOROKO" ~	"TORO",
      country	== "UGANDA" &
        province	== "MASAKA" &	district	== "KALUNGU" ~	"SOUTH BUGANDA",
      country	== "UGANDA" &
        province	== "MASAKA" &	district	== "MASAKA" ~	"KALANGALA",
      country	== "UGANDA" &
        province	== "ARUA" &	district	== "MARACHA" ~	"WEST NILE",
      country	== "UGANDA" &
        province	== "ARUA" &	district	== "NEBBI" ~	"NEBBI",
      country	== "UGANDA" &
        province	== "ARUA" &	district	== "YUMBE" ~	"WEST NILE",
      country	== "UGANDA" &
        province	== "ARUA" &	district	== "ZOMBO" ~	"WEST NILE",
      country	== "UGANDA" &
        province	== "GREATER KAMPALA" &	district	== "MPIGI" ~	"MPIGI",
      country	== "UGANDA" &
        province	== "GREATER KAMPALA" &	district	== "MUKONO" ~	"MUKONO",
      country	== "UGANDA" &
        province	== "GREATER KAMPALA" &
        district	== "WAKISO" ~	"NORTH BUGANDA",
      country	== "UGANDA" &
        province	== "GULU" &	district	== "AMURU" ~	"ACHOLI",
      country	== "UGANDA" &
        province	== "GULU" &	district	== "OMORO" ~	"ACHOLI",
      country	== "UGANDA" &
        province	== "GULU" &	district	== "PADER" ~	"ACHOLI",
      country	== "UGANDA" &
        province	== "HOIMA" &	district	== "BULIISA" ~	"BUNYORO",
      country	== "UGANDA" &
        province	== "HOIMA" &	district	== "KAGADI" ~	"BUNYORO",
      country	== "UGANDA" &
        province	== "HOIMA" &	district	== "KAKUMIRO" ~	"BUNYORO",
      country	== "UGANDA" &
        province	== "HOIMA" &	district	== "KIBAALE" ~	"KIBAALE",
      country	== "UGANDA" &
        province	== "HOIMA" &	district	== "KIKUUBE" ~	"BUNYORO",
      country	== "UGANDA" &
        province	== "HOIMA" &	district	== "MASINDI" ~	"MASINDI",
      country	== "UGANDA" &
        province	== "JINJA" &	district	== "IGANGA" ~	"MAYUNGE",
      country	== "UGANDA" &
        province	== "JINJA" &	district	== "KALIRO" ~	"BUSOGA",
      country	== "UGANDA" &
        province	== "JINJA" &	district	== "LUUKA" ~	"BUSOGA",
      country	== "UGANDA" &
        province	== "JINJA" &	district	== "MAYUGE" ~	"BUSOGA",
      country	== "UGANDA" &
        province	== "JINJA" &	district	== "NAMAYINGO" ~	"BUSOGA",
      country	== "UGANDA" &
        province	== "KABALE" &	district	== "KANUNGU" ~	"KIGEZI",
      country	== "UGANDA" &
        province	== "KABALE" &	district	== "KISORO" ~	"KIGEZI",
      country	== "UGANDA" &
        province	== "KABALE" &	district	== "RUKIGA" ~	"KIGEZI",
      country	== "UGANDA" &
        province	== "KABAROLE" &	district	== "BUNDIBUGYO" ~	"HOIMA",
      country	== "UGANDA" &
        province	== "KABAROLE" &	district	== "BUNYANGABU" ~	"TORO",
      country	== "UGANDA" &
        province	== "KABAROLE" &	district	== "KAMWENGE" ~	"TORO",
      country	== "UGANDA" &
        province	== "LIRA" &	district	== "ALEBTONG" ~	"LANGO",
      country	== "UGANDA" &
        province	== "LIRA" &	district	== "AMOLATAR" ~	"LANGO",
      country	== "UGANDA" &	province	== "LIRA" &
        district	== "APAC" ~	"APAC",
      country	== "UGANDA" &
        province	== "LIRA" &	district	== "DOKOLO" ~	"LANGO",
      country	== "UGANDA" &
        province	== "LIRA" &	district	== "KOLE" ~	"LANGO",
      country	== "UGANDA" &
        province	== "LIRA" &	district	== "KWANIA" ~	"LANGO",
      country	== "UGANDA" &	province	== "LIRA" &
        district	== "LIRA" ~	"LIRA",
      country	== "UGANDA" &
        province	== "LIRA" &	district	== "OTUKE" ~	"LANGO",
      country	== "UGANDA" &
        province	== "LIRA" &	district	== "OYAM" ~	"LANGO",
      country	== "UGANDA" &
        province	== "MASAKA" &	district	== "BUKOMANSIMBI" ~	"SOUTH BUGANDA",
      country	== "UGANDA" &
        province	== "MASAKA" &	district	== "KYOTERA" ~	"SOUTH BUGANDA",
      country	== "UGANDA" &
        province	== "MASAKA" &	district	== "LWENGO" ~	"SOUTH BUGANDA",
      country	== "UGANDA" &
        province	== "MASAKA" &	district	== "RAKAI" ~	"RAKAI",
      country	== "UGANDA" &
        province	== "MASAKA" &	district	== "SEMBABULE" ~	"SEMBABULE",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "BUDUDA" ~	"ELGON",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "BUSIA" ~	"BUKEDI",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "BUTEBO" ~	"BUKEDI",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "KAPCHORWA" ~	"KAPCHORWA",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "KIBUKU" ~	"BUKEDI",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "KWEEN" ~	"ELGON",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "MANAFWA" ~	"ELGON",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "NAMISINDWA" ~	"ELGON",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "PALLISA" ~	"PALLISA",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "SIRONKO" ~	"ELGON",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "BUSHENYI" ~	"BUSHENYI",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "ISINGIRO" ~	"ANKOLE",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "KAZO" ~	"ANKOLE",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "NTUNGAMO" ~	"ANKOLE",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "RUBIRIZI" ~	"ANKOLE",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "RWAMPARA" ~	"ANKOLE",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "SHEEMA" ~	"ANKOLE",
      country	== "UGANDA" &
        province	== "MOROTO" &	district	== "ABIM" ~	"KARAMOJA",
      country	== "UGANDA" &
        province	== "MOROTO" &	district	== "AMUDAT" ~	"KARAMOJA",
      country	== "UGANDA" &
        province	== "MOROTO" &	district	== "KOTIDO" ~	"KAABONG",
      country	== "UGANDA" &
        province	== "MOROTO" &	district	== "NABILATUK" ~	"KARAMOJA",
      country	== "UGANDA" &
        province	== "MOROTO" &	district	== "NAPAK" ~	"KARAMOJA",
      country	== "UGANDA" &
        province	== "MUBENDE" &	district	== "KASANDA" ~	"NORTH BUGANDA",
      country	== "UGANDA" &
        province	== "MUBENDE" &	district	== "KIBOGA" ~	"KIBOGA",
      country	== "UGANDA" &
        province	== "MUBENDE" &	district	== "KYAKWANZI" ~	"NORTH BUGANDA",
      country	== "UGANDA" &
        province	== "MUBENDE" &	district	== "NAKASEKE" ~	"NORTH BUGANDA",
      country	== "UGANDA" &
        province	== "MUBENDE" &	district	== "NAKASONGOLA" ~	"NAKASONGOLA",
      country	== "UGANDA" &
        province	== "SOROTI" &	district	== "AMURIA" ~	"TESO",
      country	== "UGANDA" &
        province	== "SOROTI" &	district	== "KALAKI" ~	"TESO",
      country	== "UGANDA" &
        province	== "SOROTI" &	district	== "KAPELEBYONG" ~	"TESO",
      country	== "UGANDA" &
        province	== "SOROTI" &	district	== "KATAKWI" ~	"AMURIA",
      country	== "UGANDA" &
        province	== "SOROTI" &	district	== "NGORA" ~	"TESO",
      country	== "UGANDA" &
        province	== "ARUA" &	district	== "MADI OKOLLO" ~	"WEST NILE",
      country	== "UGANDA" &
        province	== "ARUA" &	district	== "PAKWACH" ~	"WEST NILE",
      country	== "UGANDA" &
        province	== "GREATER KAMPALA" &
        district	== "BUTAMBALA" ~	"SOUTH BUGANDA",
      country	== "UGANDA" &
        province	== "GREATER KAMPALA" &	district	== "GOMBA" ~	"SOUTH BUGANDA",
      country	== "UGANDA" &
        province	== "GREATER KAMPALA" &
        district	== "KAYUNGA" ~	"NORTH BUGANDA",
      country	== "UGANDA" &
        province	== "GULU" &	district	== "GULU" ~	"KILAK",
      country	== "UGANDA" &
        province	== "GULU" &	district	== "KITGUM" ~	"KITGUM",
      country	== "UGANDA" &
        province	== "GULU" &	district	== "LAMWO" ~	"ACHOLI",
      country	== "UGANDA" &
        province	== "GULU" &	district	== "NWOYA" ~	"ACHOLI",
      country	== "UGANDA" &
        province	== "KABALE" &	district	== "RUKUNGIRI" ~	"RUKUNGIRI",
      country	== "UGANDA" &
        province	== "KABAROLE" &	district	== "KITAGWENDA" ~	"TORO",
      country	== "UGANDA" &
        province	== "KABAROLE" &	district	== "KYEGEGWA" ~	"TORO",
      country	== "UGANDA" &
        province	== "MBALE" &	district	== "BULAMBULI" ~	"ELGON",
      country	== "UGANDA" &
        province	== "MOROTO" &	district	== "KARENGA" ~	"KARAMOJA",
      country	== "UGANDA" &
        province	== "MOROTO" &	district	== "NAKAPIRIPIRIT" ~	"KARAMOJA",
      country	== "UGANDA" &
        province	== "MUBENDE" &	district	== "MITYANA" ~	"NORTH BUGANDA",
      country	== "UGANDA" &
        province	== "SOROTI" &	district	== "SERERE" ~	"TESO",
      country	== "UGANDA" &
        province	== "MUBENDE" &	district	== "MUBENDE" ~	"MUBENDE",
      country	== "UGANDA" &
        province	== "ARUA" &	district	== "ADJUMANI" ~	"ADJUMANI",
      country	== "UGANDA" &
        province	== "ARUA" &	district	== "OBONGI" ~	"WEST NILE",
      country	== "UGANDA" &
        province	== "JINJA" &	district	== "BUGIRI" ~	"BUSOGA",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "IBANDA" ~	"ANKOLE",
      country	== "UGANDA" &
        province	== "MOROTO" &	district	== "MOROTO" ~	"MOROTO",
      country	== "UGANDA" &
        province	== "ARUA" &	district	== "KOBOKO" ~	"WEST NILE",
      country	== "UGANDA" &
        province	== "MBARARA" &	district	== "BUHWEJU" ~	"ANKOLE",
      country	== "UGANDA" &
        province	== "MOROTO" &	district	== "KAABONG" ~	"KARAMOJA",
      country	== "KENYA" &
        province	== "MANDERA" &	district	== "MANDERA WEST" ~	"MANDERA",
      country	== "KENYA" &
        province	== "GARISSA" &	district	== "BALAMBALA" ~	"GARISSA",
      country	== "KENYA" &
        province	== "GARISSA" &	district	== "FAFI" ~	"GARISSA",
      country	== "KENYA" &
        province	== "GARISSA" &	district	== "GARISSA" ~	"NORTH EASTERN",
      country	== "KENYA" &
        province	== "GARISSA" &	district	== "LAGDERA" ~	"GARISSA",
      country	== "KENYA" &
        province	== "ISIOLO" &	district	== "ISIOLO" ~	"KENEASTERN",
      country	== "KENYA" &
        province	== "ISIOLO" &	district	== "MERTI" ~	"ISIOLO",
      country	== "KENYA" &
        province	== "KAJIADO" &	district	== "KAJIADO EAST" ~	"KAJIADO",
      country	== "KENYA" &
        province	== "KAJIADO" &	district	== "KAJIADO NORTH" ~	"KAJIADO",
      country	== "KENYA" &
        province	== "KAJIADO" &	district	== "KAJIADO WEST" ~	"KAJIADO",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "JUJA" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "KABETE" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "KIAMBAA" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "KIAMBU TOWN" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "KIKUYU" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "THIKA TOWN" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "KILIFI" &	district	== "KALOLENI" ~	"KILIFI",
      country	== "KENYA" &
        province	== "KILIFI" &	district	== "KILIFI NORTH" ~	"KILIFI",
      country	== "KENYA" &
        province	== "KILIFI" &	district	== "MAGARINI" ~	"KILIFI",
      country	== "KENYA" &
        province	== "KILIFI" &	district	== "MALINDI" ~	"COAST",
      country	== "KENYA" &
        province	== "KITUI" &	district	== "KITUI EAST" ~	"KITUI",
      country	== "KENYA" &
        province	== "KITUI" &	district	== "KITUI SOUTH" ~	"KITUI",
      country	== "KENYA" &
        province	== "KITUI" &	district	== "MWINGI CENTRAL" ~	"KITUI",
      country	== "KENYA" &
        province	== "KITUI" &	district	== "MWINGI NORTH" ~	"KITUI",
      country	== "KENYA" &
        province	== "LAMU" &	district	== "LAMU EAST" ~	"LAMU",
      country	== "KENYA" &
        province	== "LAMU" &	district	== "LAMU WEST" ~	"LAMU",
      country	== "KENYA" &
        province	== "MACHAKOS" &	district	== "ATHI RIVER" ~	"MACHAKOS",
      country	== "KENYA" &
        province	== "MACHAKOS" &	district	== "MACHAKOS" ~	"KENEASTERN",
      country	== "KENYA" &
        province	== "MACHAKOS" &	district	== "MASINGA" ~	"MACHAKOS",
      country	== "KENYA" &
        province	== "MACHAKOS" &	district	== "MATUNGULU" ~	"MACHAKOS",
      country	== "KENYA" &
        province	== "MANDERA" &	district	== "MANDERA EAST" ~	"MANDERA",
      country	== "KENYA" &
        province	== "MANDERA" &	district	== "MANDERA NORTH" ~	"MANDERA",
      country	== "KENYA" &
        province	== "MANDERA" &	district	== "MANDERA SOUTH" ~	"MANDERA",
      country	== "KENYA" &
        province	== "MOMBASA" &	district	== "KISAUNI" ~	"MOMBASA",
      country	== "KENYA" &
        province	== "MOMBASA" &	district	== "LIKONI" ~	"MOMBASA",
      country	== "KENYA" &
        province	== "MOMBASA" &	district	== "NYALI" ~	"MOMBASA",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "DAGORETTI SOUTH" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "EMBAKASI EAST" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "EMBAKASI NORTH" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "EMBAKASI WEST" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "KASARANI" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "KIBRA" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "MATHARE" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "RUARAKA" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "STAREHE" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "WESTLANDS" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "TANA RIVER" &	district	== "BURA" ~	"TANA RIVER",
      country	== "KENYA" &
        province	== "TANA RIVER" &	district	== "GALOLE" ~	"TANA RIVER",
      country	== "KENYA" &
        province	== "WAJIR" &	district	== "WAJIR EAST" ~	"WAJIR",
      country	== "KENYA" &
        province	== "WAJIR" &	district	== "WAJIR NORTH" ~	"WAJIR",
      country	== "KENYA" &
        province	== "WAJIR" &	district	== "WAJIR SOUTH" ~	"WAJIR",
      country	== "KENYA" &
        province	== "MACHAKOS" &	district	== "MWALA" ~	"MACHAKOS",
      country	== "KENYA" &
        province	== "TANA RIVER" &	district	== "GARSEN" ~	"TANA RIVER",
      country	== "KENYA" &
        province	== "MOMBASA" &	district	== "MVITA" ~	"MOMBASA",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "MAKADARA" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "GARISSA" &	district	== "IJARA" ~	"GARISSA",
      country	== "KENYA" &
        province	== "KAJIADO" &	district	== "KAJIADO CENTRAL" ~	"KAJIADO",
      country	== "KENYA" &
        province	== "KILIFI" &	district	== "RABAI" ~	"KILIFI",
      country	== "KENYA" &
        province	== "KITUI" &	district	== "KITUI WEST" ~	"KITUI",
      country	== "KENYA" &
        province	== "GARISSA" &	district	== "HULUGHO" ~	"GARISSA",
      country	== "KENYA" &
        province	== "ISIOLO" &	district	== "GARBATULLA" ~	"ISIOLO",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "LANGATA" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "ROYSAMBU" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "GARISSA" &	district	== "DADAAB" ~	"GARISSA",
      country	== "KENYA" &
        province	== "KAJIADO" &	district	== "LOITOKITOK" ~	"KAJIADO",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "EMBAKASI CENTRAL" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "EMBAKASI SOUTH" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "KAMUKUNJI" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "GATUNDU NORTH" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "GATUNDU SOUTH" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "GITHUNGURI" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "LARI" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "LIMURU" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "KIAMBU" &	district	== "RUIRU" ~	"KIAMBU",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "DAGORETTI NORTH" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "KITUI" &	district	== "KITUI CENTRAL" ~	"KITUI",
      country	== "KENYA" &
        province	== "KITUI" &	district	== "KITUI RURAL" ~	"KITUI",
      country	== "KENYA" &
        province	== "KITUI" &	district	== "MWINGI WEST" ~	"KITUI",
      country	== "KENYA" &
        province	== "MACHAKOS" &	district	== "KANGUNDO" ~	"MACHAKOS",
      country	== "KENYA" &
        province	== "MACHAKOS" &	district	== "KATHIANI" ~	"MACHAKOS",
      country	== "KENYA" &
        province	== "MACHAKOS" &	district	== "YATTA" ~	"MACHAKOS",
      country	== "KENYA" &
        province	== "MANDERA" &	district	== "LAFEY" ~	"MANDERA",
      country	== "KENYA" &
        province	== "MANDERA" &	district	== "BANISSA" ~	"MANDERA",
      country	== "KENYA" &
        province	== "NAIROBI" &	district	== "DAGORETTI" ~	"NAIROBI",
      country	== "KENYA" &
        province	== "WAJIR" &	district	== "ELDAS" ~	"WAJIR",
      country	== "KENYA" &
        province	== "WAJIR" &	district	== "TARBAJ" ~	"WAJIR",
      country	== "KENYA" &
        province	== "WAJIR" &	district	== "WAJIR WEST" ~	"WAJIR",
      country	== "KENYA" &
        province	== "MACHAKOS" &	district	== "KALAMA" ~	"MACHAKOS",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "KATETE" ~	"ZAMEASTERN",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "LUNDAZI" ~	"EASTERN",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "KASENENGWA" ~	"EASTERN",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "LUMEZI" ~	"EASTERN",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "MAMBWE" ~	"ZAMEASTERN",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "CHADIZA" ~	"ZAMEASTERN",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "CHASEFU" ~	"EASTERN",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "CHIPANGALI" ~	"EASTERN",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "CHIPATA" ~	"EASTERN",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "PETAUKE" ~	"ZAMEASTERN",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "VUBWI" ~	"EASTERN",
      country	== "ZAMBIA" &
        province	== "LUSAKA" &	district	== "CHONGWE" ~	"LUSAKA",
      country	== "ZAMBIA" &
        province	== "MUCHINGA" &	district	== "NAKONDE" ~	"MUCHINGA",
      country	== "ZAMBIA" &
        province	== "MUCHINGA" &	district	== "SHIWANG'ANDU" ~	"MUCHINGA",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "NYIMBA" ~	"ZAMEASTERN",
      country	== "ZAMBIA" &
        province	== "EASTERN" &	district	== "SINDA" ~	"EASTERN",
      country	== "ZAMBIA" &
        province	== "LUSAKA" &	district	== "KAFUE" ~	"LUSAKA",
      country	== "ZAMBIA" &
        province	== "LUSAKA" &	district	== "LUANGWA" ~	"LUSAKA",
      country	== "ZAMBIA" &
        province	== "LUSAKA" &	district	== "LUSAKA" ~	"LUSAKA",
      country	== "ZAMBIA" &
        province	== "LUSAKA" &	district	== "RUFUNSA" ~	"LUSAKA",
      country	== "ZAMBIA" &
        province	== "MUCHINGA" &	district	== "CHAMA" ~	"ZAMEASTERN",
      country	== "ZAMBIA" &
        province	== "MUCHINGA" &	district	== "CHINSALI" ~	"ZAMNORTHERN",
      country	== "ZAMBIA" &
        province	== "MUCHINGA" &	district	== "ISOKA" ~	"ZAMNORTHERN",
      country	== "ZAMBIA" &
        province	== "MUCHINGA" &	district	== "KANCHIBIYA" ~	"MUCHINGA",
      country	== "ZAMBIA" &
        province	== "MUCHINGA" &	district	== "MAFINGA" ~	"MUCHINGA",
      country	== "ZAMBIA" &
        province	== "MUCHINGA" &	district	== "MPIKA" ~	"ZAMNORTHERN",
      country	== "ZAMBIA" &
        province	== "LUSAKA" &	district	== "CHILANGA" ~	"LUSAKA",
      country	== "ZAMBIA" &
        province	== "LUSAKA" &	district	== "CHIRUNDU" ~	"LUSAKA",
      country	== "ZAMBIA" &
        province	== "MUCHINGA" &	district	== "LAVUSHI MANDA" ~	"MUCHINGA",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "SESHEKE" ~	"ZAMWESTERN",
      country	== "ZAMBIA" &
        province	== "COPPERBELT" &	district	== "KITWE" ~	"COPPERBELT",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "NCHELENGE" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "CHITAMBO" ~	"CENTRAL",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "KABWE" ~	"CENTRAL",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "SERENJE" ~	"ZAMCENTRAL",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "MILENGE" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "LUPOSOSHI" ~	"NORTHERN",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "MBALA" ~	"NORTHERN",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "NSAMA" ~	"NORTHERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "GWEMBE" ~	"ZAMSOUTHERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "MWANDI" ~	"WESTERN",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "CHIBOMBO" ~	"CENTRAL",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "LUANO" ~	"CENTRAL",
      country	== "ZAMBIA" &
        province	== "COPPERBELT" &	district	== "MASAITI" ~	"COPPERBELT",
      country	== "ZAMBIA" &
        province	== "COPPERBELT" &	district	== "MPONGWE" ~	"COPPERBELT",
      country	== "ZAMBIA" &
        province	== "NORTH WESTERN" &	district	== "CHAVUMA" ~	"NORTH-WESTERN",
      country	== "ZAMBIA" &
        province	== "NORTH WESTERN" &	district	== "KABOMPO" ~	"NORTH-WESTERN",
      country	== "ZAMBIA" &
        province	== "NORTH WESTERN" &
        district	== "MANYINGA" ~	"NORTH WESTERN",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "CHILUBI" ~	"ZAMNORTHERN",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "KAPUTA" ~	"ZAMNORTHERN",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "LUWINGU" ~	"NORTHERN",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "MPOROKOSO" ~	"NORTHERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "MAZABUKA" ~	"ZAMSOUTHERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "KALABO" ~	"ZAMWESTERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "NKEYEMA" ~	"WESTERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "SIKONGO" ~	"WESTERN",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "CHISAMBA" ~	"CENTRAL",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "KAPIRI-MPOSHI" ~	"CENTRAL",
      country	== "ZAMBIA" &
        province	== "COPPERBELT" &	district	== "CHINGOLA" ~	"COPPERBELT",
      country	== "ZAMBIA" &
        province	== "COPPERBELT" &	district	== "LUANSHYA" ~	"COPPERBELT",
      country	== "ZAMBIA" &
        province	== "COPPERBELT" &	district	== "MUFULIRA" ~	"COPPERBELT",
      country	== "ZAMBIA" &
        province	== "NORTH WESTERN" &	district	== "KASEMPA" ~	"NORTH-WESTERN",
      country	== "ZAMBIA" &
        province	== "NORTH WESTERN" &
        district	== "MUSHINDANO" ~	"NORTH WESTERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "NALOLO" ~	"WESTERN",
      country	== "ZAMBIA" &
        province	== "COPPERBELT" &	district	== "KALULUSHI" ~	"COPPERBELT",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "CHIENGE" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "SAMFYA" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "NORTH WESTERN" &
        district	== "MUFUMBWE" ~	"NORTH-WESTERN",
      country	== "ZAMBIA" &
        province	== "NORTH WESTERN" &
        district	== "MWINILUNGA" ~	"NORTH-WESTERN",
      country	== "ZAMBIA" &
        province	== "NORTH WESTERN" &	district	== "SOLWEZI" ~	"NORTH WESTERN",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "KASAMA" ~	"ZAMNORTHERN",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "LUNTE" ~	"NORTHERN",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "MUNGWI" ~	"ZAMNORTHERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "CHOMA" ~	"ZAMSOUTHERN",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "MKUSHI" ~	"ZAMCENTRAL",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "NGABWE" ~	"CENTRAL",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "SHIBUYUNJI" ~	"CENTRAL",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "CHIPILI" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "LUNGA" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "MANSA" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "MWANSABOMBWE" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "SENGA" ~	"NORTHERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "KALOMO" ~	"ZAMSOUTHERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "NAMWALA" ~	"ZAMSOUTHERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "SIAVONGA" ~	"SOUTHERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "LUAMPA" ~	"WESTERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "LUKULU" ~	"ZAMWESTERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "MONGU" ~	"ZAMWESTERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "SENANGA" ~	"ZAMWESTERN",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "KAWAMBWA" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "KAZUNGULA" ~	"ZAMSOUTHERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "MONZE" ~	"SOUTHERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "KAOMA" ~	"ZAMWESTERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "MULOBEZI" ~	"WESTERN",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "ITEZHI-TEZHI" ~	"CENTRAL",
      country	== "ZAMBIA" &
        province	== "COPPERBELT" &
        district	== "CHILILABOMBWE" ~	"NORTH WESTERN",
      country	== "ZAMBIA" &
        province	== "COPPERBELT" &	district	== "LUFWANYAMA" ~	"COPPERBELT",
      country	== "ZAMBIA" &
        province	== "COPPERBELT" &	district	== "NDOLA" ~	"COPPERBELT",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "CHEMBE" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "CHIFUNABULI" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "LUAPULA" &	district	== "MWENSE" ~	"LUAPULA",
      country	== "ZAMBIA" &
        province	== "NORTH WESTERN" &
        district	== "IKELENGE" ~	"NORTH WESTERN",
      country	== "ZAMBIA" &
        province	== "NORTH WESTERN" &
        district	== "KALUMBILA" ~	"NORTH WESTERN",
      country	== "ZAMBIA" &
        province	== "NORTH WESTERN" &	district	== "ZAMBEZI" ~	"NORTH-WESTERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "CHIKANKATA" ~	"SOUTHERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "LIVINGSTONE" ~	"SOUTHERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "PEMBA" ~	"SOUTHERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "SINAZONGWE" ~	"SOUTHERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "LIMULUNGA" ~	"WESTERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "MITETE" ~	"WESTERN",
      country	== "ZAMBIA" &
        province	== "NORTHERN" &	district	== "MPULUNGU" ~	"ZAMNORTHERN",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "SIOMA" ~	"WESTERN",
      country	== "ZAMBIA" &
        province	== "CENTRAL" &	district	== "MUMBWA" ~	"CENTRAL",
      country	== "ZAMBIA" &
        province	== "WESTERN" &	district	== "SHANG'OMBO" ~	"WESTERN",
      country	== "ZAMBIA" &
        province	== "SOUTHERN" &	district	== "ZIMBA" ~	"SOUTHERN",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND WEST" &
        district	== "CHEGUTU" ~	"MASHONALAND WEST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND WEST" &
        district	== "SANYATI" ~	"MASHONALAND WEST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND CENTRAL" &
        district	== "CENTENARY" ~	"MASHONALAND CENTRAL",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND NORTH" &
        district	== "UMGUZA" ~	"MATABELELAND NORTH",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND SOUTH" &
        district	== "BEITBRIDGE" ~	"MATABELELAND SOUTH",
      country	== "ZIMBABWE" &
        province	== "BULAWAYO" &	district	== "BULAWAYO" ~	"BULAWAYO",
      country	== "ZIMBABWE" &
        province	== "HARARE" &	district	== "CHITUNGWIZA" ~	"HARARE",
      country	== "ZIMBABWE" &
        province	== "HARARE" &	district	== "HARARE" ~	"HARARE",
      country	== "ZIMBABWE" &
        province	== "MANICALAND" &	district	== "BUHERA" ~	"MANICALAND",
      country	== "ZIMBABWE" &
        province	== "MANICALAND" &	district	== "CHIPINGE" ~	"MANICALAND",
      country	== "ZIMBABWE" &
        province	== "MANICALAND" &	district	== "MUTARE" ~	"MANICALAND",
      country	== "ZIMBABWE" &
        province	== "MANICALAND" &	district	== "MUTASA" ~	"MANICALAND",
      country	== "ZIMBABWE" &
        province	== "MANICALAND" &	district	== "NYANGA" ~	"MANICALAND",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND CENTRAL" &
        district	== "RUSHINGA" ~	"MASHONALAND CENTRAL",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND CENTRAL" &
        district	== "SHAMVA" ~	"MASHONALAND CENTRAL",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND EAST" &
        district	== "HWEDZA" ~	"MASHONALAND EAST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND EAST" &
        district	== "MARONDERA" ~	"MASHONALAND EAST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND EAST" &
        district	== "MUDZI" ~	"MASHONALAND EAST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND EAST" &
        district	== "MUTOKO" ~	"MASHONALAND EAST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND EAST" &
        district	== "SEKE" ~	"MASHONALAND EAST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND EAST" &
        district	== "UMP" ~	"MASHONALAND EAST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND WEST" &
        district	== "KARIBA" ~	"MASHONALAND WEST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND WEST" &
        district	== "ZVIMBA" ~	"MASHONALAND WEST",
      country	== "ZIMBABWE" &
        province	== "MASVINGO" &	district	== "BIKITA" ~	"MASVINGO",
      country	== "ZIMBABWE" &
        province	== "MASVINGO" &	district	== "CHIVI" ~	"MASVINGO",
      country	== "ZIMBABWE" &
        province	== "MASVINGO" &	district	== "GUTU" ~	"MASVINGO",
      country	== "ZIMBABWE" &
        province	== "MASVINGO" &	district	== "MASVINGO" ~	"MASVINGO",
      country	== "ZIMBABWE" &
        province	== "MASVINGO" &	district	== "MWENEZI" ~	"MASVINGO",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND NORTH" &
        district	== "BUBI" ~	"MATABELELAND NORTH",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND NORTH" &
        district	== "HWANGE" ~	"MATABELELAND NORTH",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND NORTH" &
        district	== "LUPANE" ~	"MATABELELAND NORTH",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND NORTH" &
        district	== "TSHOLOTSHO" ~	"MATABELELAND NORTH",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND SOUTH" &
        district	== "BULILIMA" ~	"MATABELELAND SOUTH",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND SOUTH" &
        district	== "MANGWE" ~	"MATABELELAND SOUTH",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND SOUTH" &
        district	== "MATOBO" ~	"MATABELELAND SOUTH",
      country	== "ZIMBABWE" &
        province	== "MIDLANDS" &	district	== "CHIRUMHANZU" ~	"MIDLANDS",
      country	== "ZIMBABWE" &
        province	== "MIDLANDS" &	district	== "GOKWE NORTH" ~	"MIDLANDS",
      country	== "ZIMBABWE" &
        province	== "MIDLANDS" &	district	== "GOKWE SOUTH" ~	"MIDLANDS",
      country	== "ZIMBABWE" &
        province	== "MIDLANDS" &	district	== "GWERU" ~	"MIDLANDS",
      country	== "ZIMBABWE" &
        province	== "MIDLANDS" &	district	== "KWEKWE" ~	"MIDLANDS",
      country	== "ZIMBABWE" &
        province	== "MIDLANDS" &	district	== "SHURUGWI" ~	"MIDLANDS",
      country	== "ZIMBABWE" &
        province	== "MIDLANDS" &	district	== "ZVISHAVANE" ~	"MIDLANDS",
      country	== "ZIMBABWE" &
        province	== "MANICALAND" &	district	== "MAKONI" ~	"MANICALAND",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND CENTRAL" &
        district	== "BINDURA" ~	"MASHONALAND CENTRAL",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND CENTRAL" &
        district	== "GURUVE" ~	"MASHONALAND CENTRAL",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND EAST" &
        district	== "GOROMONZI" ~	"MASHONALAND EAST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND WEST" &
        district	== "HURUNGWE" ~	"MASHONALAND WEST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND WEST" &
        district	== "MAKONDE" ~	"MASHONALAND WEST",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND WEST" &
        district	== "MHONDORO NGEZI" ~	"MASHONALAND WEST",
      country	== "ZIMBABWE" &
        province	== "MASVINGO" &	district	== "ZAKA" ~	"MASVINGO",
      country	== "ZIMBABWE" &
        province	== "MIDLANDS" &	district	== "MBERENGWA" ~	"MIDLANDS",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND EAST" &
        district	== "MUREHWA" ~	"MASHONALAND EAST",
      country	== "ZIMBABWE" &
        province	== "MANICALAND" &	district	== "CHIMANIMANI" ~	"MANICALAND",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND CENTRAL" &
        district	== "MAZOWE" ~	"MASHONALAND CENTRAL",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND CENTRAL" &
        district	== "MBIRE" ~	"MASHONALAND CENTRAL",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND CENTRAL" &
        district	== "MOUNT DARWIN" ~	"MASHONALAND CENTRAL",
      country	== "ZIMBABWE" &
        province	== "MASVINGO" &	district	== "CHIREDZI" ~	"MASVINGO",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND NORTH" &
        district	== "BINGA" ~	"MATABELELAND NORTH",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND NORTH" &
        district	== "NKAYI" ~	"MATABELELAND NORTH",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND SOUTH" &
        district	== "GWANDA" ~	"MATABELELAND SOUTH",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND SOUTH" &
        district	== "UMZINGWANE" ~	"MATABELELAND SOUTH",
      country	== "ZIMBABWE" &
        province	== "MATABELELAND SOUTH" &
        district	== "INSIZA" ~	"MATABELELAND SOUTH",
      country	== "ZIMBABWE" &
        province	== "MASHONALAND EAST" &
        district	== "CHIKOMBA" ~	"MASHONALAND EAST",
      country	== "ANGOLA" &	province	== "BIE" &
        district	== "NHAREA" ~	"BIE",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "CACULA" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "CUANZA NORTE" &	district	== "AMBACA" ~	"CUANZA NORTE",
      country	== "ANGOLA" &
        province	== "CUANZA NORTE" &	district	== "CAZENGO" ~	"CUANZA NORTE",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "AMBOIM" ~	"KWANZA SUL",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "SUMBE" ~	"KWANZA SUL",
      country	== "ANGOLA" &
        province	== "BENGO" &	district	== "NAMBUANGONGO" ~	"BENGO",
      country	== "ANGOLA" &
        province	== "BENGUELA" &	district	== "BALOMBO" ~	"BENGUELA",
      country	== "ANGOLA" &
        province	== "CABINDA" &	district	== "BUCO ZAU" ~	"CABINDA",
      country	== "ANGOLA" &
        province	== "CUANDO CUBANGO" &
        district	== "MENONGUE" ~	"KUANDO KUBANGO",
      country	== "ANGOLA" &
        province	== "HUAMBO" &	district	== "CATCHIUNGO" ~	"HUAMBO",
      country	== "ANGOLA" &
        province	== "HUAMBO" &	district	== "LONDUIMBALE" ~	"HUAMBO",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "QUISSAMA" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUNDA SUL" &	district	== "SAURIMO" ~	"LUNDA SUL",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "CALANDULA" ~	"MALANGE",
      country	== "ANGOLA" &
        province	== "MOXICO" &	district	== "MOXICO (LUENA)" ~	"MOXICO",
      country	== "ANGOLA" &
        province	== "BENGUELA" &	district	== "CUBAL" ~	"BENGUELA",
      country	== "ANGOLA" &
        province	== "BIE" &	district	== "CHINGUAR" ~	"BIE",
      country	== "ANGOLA" &
        province	== "LUNDA NORTE" &	district	== "CAUNGULA" ~	"LUNDA NORTE",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "QUELA" ~	"MALANJE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "QUIMBELE" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "BENGUELA" &	district	== "CAIMBAMBO" ~	"BENGUELA",
      country	== "ANGOLA" &
        province	== "CUANDO CUBANGO" &	district	== "CUCHI" ~	"KUANDO KUBANGO",
      country	== "ANGOLA" &
        province	== "CUANDO CUBANGO" &
        district	== "DIRICO" ~	"KUANDO KUBANGO",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "BELAS" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "ZAIRE" &	district	== "NOQUI" ~	"ZAIRE",
      country	== "ANGOLA" &
        province	== "ZAIRE" &	district	== "SOYO" ~	"ZAIRE",
      country	== "ANGOLA" &
        province	== "BENGO" &	district	== "AMBRIZ" ~	"BENGO",
      country	== "ANGOLA" &
        province	== "BENGUELA" &	district	== "BAIA FARTA" ~	"BENGUELA",
      country	== "ANGOLA" &
        province	== "BENGUELA" &	district	== "BENGUELA" ~	"BENGUELA",
      country	== "ANGOLA" &
        province	== "BENGUELA" &	district	== "BOCOIO" ~	"BENGUELA",
      country	== "ANGOLA" &
        province	== "BENGUELA" &	district	== "CATUMBELA" ~	"BENGUELA",
      country	== "ANGOLA" &
        province	== "BENGUELA" &	district	== "CHONGOROI" ~	"BENGUELA",
      country	== "ANGOLA" &
        province	== "BENGUELA" &	district	== "LOBITO" ~	"BENGUELA",
      country	== "ANGOLA" &
        province	== "CABINDA" &	district	== "BELIZE" ~	"CABINDA",
      country	== "ANGOLA" &
        province	== "CABINDA" &	district	== "CABINDA" ~	"CABINDA",
      country	== "ANGOLA" &
        province	== "CABINDA" &	district	== "CACONGO" ~	"CABINDA",
      country	== "ANGOLA" &
        province	== "CUANDO CUBANGO" &
        district	== "CUANGAR" ~	"KUANDO KUBANGO",
      country	== "ANGOLA" &
        province	== "CUANDO CUBANGO" &
        district	== "MAVINGA" ~	"KUANDO KUBANGO",
      country	== "ANGOLA" &
        province	== "CUANDO CUBANGO" &
        district	== "RIVUNGO" ~	"KUANDO KUBANGO",
      country	== "ANGOLA" &
        province	== "CUANZA NORTE" &
        district	== "GOLUNGO ALTO" ~	"KWANZA NORTE",
      country	== "ANGOLA" &
        province	== "CUNENE" &	district	== "CUROCA" ~	"CUNENE",
      country	== "ANGOLA" &
        province	== "HUAMBO" &	district	== "LONGONJO" ~	"HUAMBO",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "GAMBOS" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "LUBANGO" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "QUIPUNGO" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "ICOLO E BENGO" ~	"BENGO",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "LUQUEMBO" ~	"MALANGE",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "MARIMBA" ~	"MALANGE",
      country	== "ANGOLA" &
        province	== "MOXICO" &	district	== "LUCHAZES" ~	"MOXICO",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "BEMBE" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "ZAIRE" &	district	== "MBANZA CONGO" ~	"ZAIRE",
      country	== "ANGOLA" &
        province	== "ZAIRE" &	district	== "TOMBOCO" ~	"ZAIRE",
      country	== "ANGOLA" &
        province	== "BENGO" &	district	== "BULA ATUMBA" ~	"KWANZA NORTE",
      country	== "ANGOLA" &
        province	== "BENGO" &	district	== "DANDE" ~	"BENGO",
      country	== "ANGOLA" &
        province	== "BENGO" &	district	== "DEMBOS (QUIBAXE)" ~	"BENGO",
      country	== "ANGOLA" &
        province	== "BENGO" &	district	== "PANGO ALUQUEM" ~	"KWANZA NORTE",
      country	== "ANGOLA" &
        province	== "BENGUELA" &	district	== "GANDA" ~	"BENGUELA",
      country	== "ANGOLA" &	province	== "BIE" &
        district	== "ANDULO" ~	"BIE",
      country	== "ANGOLA" &
        province	== "BIE" &	district	== "CAMACUPA" ~	"BIE",
      country	== "ANGOLA" &
        province	== "BIE" &	district	== "CATABOLA" ~	"BIE",
      country	== "ANGOLA" &
        province	== "BIE" &	district	== "CHITEMBO" ~	"BIE",
      country	== "ANGOLA" &	province	== "BIE" &
        district	== "CUEMBA" ~	"BIE",
      country	== "ANGOLA" &	province	== "BIE" &
        district	== "CUITO" ~	"BIE",
      country	== "ANGOLA" &
        province	== "BIE" &	district	== "CUNHINGA" ~	"BIE",
      country	== "ANGOLA" &
        province	== "CUANDO CUBANGO" &	district	== "CALAI" ~	"KUANDO KUBANGO",
      country	== "ANGOLA" &
        province	== "CUANDO CUBANGO" &
        district	== "CUITO CUANAVALE" ~	"KUANDO KUBANGO",
      country	== "ANGOLA" &
        province	== "CUANZA NORTE" &
        district	== "BOLONGONGO" ~	"KWANZA NORTE",
      country	== "ANGOLA" &
        province	== "CUANZA NORTE" &	district	== "CAMBAMBE" ~	"CUANZA NORTE",
      country	== "ANGOLA" &
        province	== "CUANZA NORTE" &	district	== "GONGUEMBO" ~	"KWANZA NORTE",
      country	== "ANGOLA" &
        province	== "CUANZA NORTE" &	district	== "LUCALA" ~	"KWANZA NORTE",
      country	== "ANGOLA" &
        province	== "CUANZA NORTE" &
        district	== "QUICULUNGO" ~	"CUANZA NORTE",
      country	== "ANGOLA" &
        province	== "CUANZA NORTE" &
        district	== "SAMBA CAJU" ~	"KWANZA NORTE",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "CASSONGUE" ~	"CUANZA SUL",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "CELA" ~	"KWANZA SUL",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "CONDA" ~	"KWANZA SUL",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "EBO" ~	"KWANZA SUL",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "LIBOLO" ~	"KWANZA SUL",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "MUSSENDE" ~	"CUANZA SUL",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "PORTO AMBOIM" ~	"KWANZA SUL",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "QUIBALA" ~	"CUANZA SUL",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "QUILENDA" ~	"CUANZA SUL",
      country	== "ANGOLA" &
        province	== "CUANZA SUL" &	district	== "SELES" ~	"KWANZA SUL",
      country	== "ANGOLA" &
        province	== "CUNENE" &	district	== "CAHAMA" ~	"CUNENE",
      country	== "ANGOLA" &
        province	== "CUNENE" &	district	== "CUANHAMA" ~	"CUNENE",
      country	== "ANGOLA" &
        province	== "CUNENE" &	district	== "CUVELAI" ~	"CUNENE",
      country	== "ANGOLA" &
        province	== "CUNENE" &	district	== "NAMACUNDE" ~	"CUNENE",
      country	== "ANGOLA" &
        province	== "CUNENE" &	district	== "OMBADJA" ~	"CUNENE",
      country	== "ANGOLA" &
        province	== "HUAMBO" &	district	== "BAILUNDO" ~	"HUAMBO",
      country	== "ANGOLA" &
        province	== "HUAMBO" &	district	== "CAALA" ~	"HUAMBO",
      country	== "ANGOLA" &
        province	== "HUAMBO" &	district	== "ECUNHA" ~	"HUAMBO",
      country	== "ANGOLA" &
        province	== "HUAMBO" &	district	== "HUAMBO" ~	"HUAMBO",
      country	== "ANGOLA" &
        province	== "HUAMBO" &	district	== "MUNGO" ~	"HUAMBO",
      country	== "ANGOLA" &
        province	== "HUAMBO" &	district	== "TCHIKALA TCHOLOHANGA" ~	"HUAMBO",
      country	== "ANGOLA" &
        province	== "HUAMBO" &	district	== "TCHINJENJE" ~	"HUAMBO",
      country	== "ANGOLA" &
        province	== "HUAMBO" &	district	== "UKUMA" ~	"HUAMBO",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "CACONDA" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "CALUQUEMBE" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "CHIBIA" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "CHICOMBA" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "CHIPINDO" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "CUVANGO" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "HUMPATA" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "JAMBA" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "MATALA" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "HUILA" &	district	== "QUILENGUES" ~	"HUILA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "CACUACO" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "CAZENGA" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "INGOMBOTA" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "KILAMBA KIAXI" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "LUANDA" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "MAIANGA" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "RANGEL" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "SAMBA" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "SAMBIZANGA" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "TALATONA" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "VIANA" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "LUNDA NORTE" &	district	== "CAMBULO" ~	"LUNDA NORTE",
      country	== "ANGOLA" &
        province	== "LUNDA NORTE" &
        district	== "CAPENDA CAMULEMBA" ~	"LUNDA NORTE",
      country	== "ANGOLA" &
        province	== "LUNDA NORTE" &	district	== "CHITATO" ~	"LUNDA NORTE",
      country	== "ANGOLA" &
        province	== "LUNDA NORTE" &	district	== "CUANGO" ~	"LUNDA NORTE",
      country	== "ANGOLA" &
        province	== "LUNDA NORTE" &	district	== "CUILO" ~	"LUNDA NORTE",
      country	== "ANGOLA" &
        province	== "LUNDA NORTE" &	district	== "LOVUA" ~	"LUNDA NORTE",
      country	== "ANGOLA" &
        province	== "LUNDA NORTE" &	district	== "LUBALO" ~	"LUNDA NORTE",
      country	== "ANGOLA" &
        province	== "LUNDA NORTE" &	district	== "LUCAPA" ~	"LUNDA NORTE",
      country	== "ANGOLA" &
        province	== "LUNDA SUL" &	district	== "CACOLO" ~	"LUNDA SUL",
      country	== "ANGOLA" &
        province	== "LUNDA SUL" &	district	== "DALA" ~	"LUNDA SUL",
      country	== "ANGOLA" &
        province	== "LUNDA SUL" &	district	== "MUCONDA" ~	"LUNDA SUL",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "CACULAMA (MUCARI)" ~	"MALANJE",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "CACUSO" ~	"MALANGE",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "CAHOMBO" ~	"MALANGE",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "CAMBUNDI CATEMBO" ~	"MALANJE",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "CANGANDALA" ~	"MALANGE",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "CUNDA DIA BAZE" ~	"MALANJE",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "KIUABA NZOJI" ~	"MALANJE",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "MALANJE" ~	"MALANJE",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "MASSANGO" ~	"MALANGE",
      country	== "ANGOLA" &
        province	== "MALANJE" &	district	== "QUIRIMA" ~	"MALANGE",
      country	== "ANGOLA" &
        province	== "MOXICO" &	district	== "ALTO ZAMBEZE" ~	"MOXICO",
      country	== "ANGOLA" &
        province	== "MOXICO" &	district	== "BUENGAS" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "MOXICO" &	district	== "CAMANONGUE" ~	"MOXICO",
      country	== "ANGOLA" &
        province	== "MOXICO" &	district	== "LEUA" ~	"MOXICO",
      country	== "ANGOLA" &
        province	== "MOXICO" &	district	== "LUACANO" ~	"MOXICO",
      country	== "ANGOLA" &
        province	== "MOXICO" &	district	== "LUAU" ~	"MOXICO",
      country	== "ANGOLA" &
        province	== "MOXICO" &	district	== "LUMEJE (CAMEIA)" ~	"MOXICO",
      country	== "ANGOLA" &
        province	== "NAMIBE" &	district	== "BIBALA" ~	"NAMIBE",
      country	== "ANGOLA" &
        province	== "NAMIBE" &	district	== "CAMUCUIO" ~	"NAMIBE",
      country	== "ANGOLA" &
        province	== "NAMIBE" &	district	== "NAMIBE" ~	"NAMIBE",
      country	== "ANGOLA" &
        province	== "NAMIBE" &	district	== "TOMBUA" ~	"NAMIBE",
      country	== "ANGOLA" &
        province	== "NAMIBE" &	district	== "VIREI" ~	"NAMIBE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "AMBUILA" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "BUNGO" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "CANGOLA" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "DAMBA" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "MAQUELA DO ZOMBO" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "MILUNGA" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "MUCABA" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "NEGAGE" ~	"UIGE",
      country	== "ANGOLA" &	province	== "UIGE" &
        district	== "PURI" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "QUITEXE" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "SANZA POMBO" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "UIGE" &	district	== "SONGO" ~	"UIGE",
      country	== "ANGOLA" &	province	== "UIGE" &
        district	== "UIGE" ~	"UIGE",
      country	== "ANGOLA" &
        province	== "CUANZA NORTE" &	district	== "BANGA" ~	"KWANZA NORTE",
      country	== "ANGOLA" &
        province	== "ZAIRE" &	district	== "CUIMBA" ~	"ZAIRE",
      country	== "ANGOLA" &
        province	== "CUANDO CUBANGO" &
        district	== "NANCOVA" ~	"CUANDO CUBANGO",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "NEVES BENDINHA" ~	"LUANDA",
      country	== "ANGOLA" &
        province	== "ZAIRE" &	district	== "NZETO" ~	"ZAIRE",
      country	== "ANGOLA" &
        province	== "LUANDA" &	district	== "NGOLA QUILUANGE" ~	"LUANDA",
      country	== "RWANDA" &
        province	== "WEST" &	district	== "KARONGI" ~	"OUE",
      country	== "RWANDA" &
        province	== "EAST" &	district	== "BUGESERA" ~	"EST",
      country	== "RWANDA" &
        province	== "EAST" &	district	== "GATSIBO" ~	"EST",
      country	== "RWANDA" &
        province	== "EAST" &	district	== "KIREHE" ~	"EST",
      country	== "RWANDA" &	province	== "EAST" &
        district	== "NGOMA" ~	"EST",
      country	== "RWANDA" &
        province	== "NORTH" &	district	== "RULINDO" ~	"NOR",
      country	== "RWANDA" &	province	== "SOUTH" &
        district	== "HUYE" ~	"SUD",
      country	== "RWANDA" &
        province	== "SOUTH" &	district	== "NYAMAGABE" ~	"SUD",
      country	== "RWANDA" &
        province	== "SOUTH" &	district	== "NYARUGURU" ~	"SUD",
      country	== "RWANDA" &
        province	== "SOUTH" &	district	== "RUHANGO" ~	"SUD",
      country	== "RWANDA" &
        province	== "WEST" &	district	== "NYAMASHEKE" ~	"OUE",
      country	== "RWANDA" &
        province	== "WEST" &	district	== "RUSIZI" ~	"OUE",
      country	== "RWANDA" &
        province	== "KIGALI_CITY" &	district	== "GASABO" ~	"MVK",
      country	== "RWANDA" &
        province	== "SOUTH" &	district	== "KAMONYI" ~	"SUD",
      country	== "RWANDA" &
        province	== "SOUTH" &	district	== "MUHANGA" ~	"SUD",
      country	== "RWANDA" &
        province	== "EAST" &	district	== "NYAGATARE" ~	"EST",
      country	== "RWANDA" &
        province	== "WEST" &	district	== "NGORORERO" ~	"OUE",
      country	== "RWANDA" &
        province	== "SOUTH" &	district	== "NYANZA" ~	"SUD",
      country	== "RWANDA" &
        province	== "NORTH" &	district	== "GAKENKE" ~	"NOR",
      country	== "RWANDA" &
        province	== "WEST" &	district	== "RUBAVU" ~	"OUE",
      country	== "RWANDA" &
        province	== "EAST" &	district	== "KAYONZA" ~	"EST",
      country	== "RWANDA" &
        province	== "EAST" &	district	== "RWAMAGANA" ~	"EST",
      country	== "RWANDA" &
        province	== "KIGALI_CITY" &	district	== "KICUKIRO" ~	"MVK",
      country	== "RWANDA" &
        province	== "KIGALI_CITY" &	district	== "NYARUGENGE" ~	"MVK",
      country	== "RWANDA" &
        province	== "NORTH" &	district	== "BURERA" ~	"NOR",
      country	== "RWANDA" &
        province	== "NORTH" &	district	== "GICUMBI" ~	"NOR",
      country	== "RWANDA" &
        province	== "NORTH" &	district	== "MUSANZE" ~	"NOR",
      country	== "RWANDA" &
        province	== "SOUTH" &	district	== "GISAGARA" ~	"SUD",
      country	== "RWANDA" &
        province	== "WEST" &	district	== "NYABIHU" ~	"OUE",
      country	== "RWANDA" &
        province	== "WEST" &	district	== "RUTSIRO" ~	"OUE",
      country	== "BOTSWANA" &
        province	== "CHARLESHILL" &	district	== "GANTSI" ~	"GANTSI",
      country	== "BOTSWANA" &
        province	== "PALAPYE" &
        district	== "SEROWE/PALAPYE" ~	"SEROWE/PALAPYE",
      country	== "BOTSWANA" &
        province	== "SOUTH EAST" &	district	== "SOUTH EAST" ~	"SOUTH EAST",
      country	== "BOTSWANA" &
        province	== "KGALAGADI NORTH" &
        district	== "KGALAGADI" ~	"KGALAGADI SOUTH",
      country	== "BOTSWANA" &
        province	== "KGATLENG" &	district	== "KGATLENG" ~	"KGATLENG",
      country	== "BOTSWANA" &
        province	== "KWENENG WEST" &
        district	== "KWENENG WEST" ~	"KWENENG WEST",
      country	== "BOTSWANA" &
        province	== "LOBATSE" &	district	== "LOBATSE" ~	"LOBATSE",
      country	== "BOTSWANA" &
        province	== "MOSHUPA" &	district	== "KANYE/MOSHUPA" ~	"KANYE/MOSHUPA",
      country	== "BOTSWANA" &
        province	== "TUTUME" &	district	== "TUTUME" ~	"TUTUME",
      country	== "BOTSWANA" &
        province	== "BOBIRWA" &	district	== "BOBIRWA" ~	"BOBIRWA",
      country	== "BOTSWANA" &
        province	== "GABORONE" &	district	== "GABORONE" ~	"GABORONE",
      country	== "BOTSWANA" &
        province	== "GOODHOPE" &	district	== "GOODHOPE" ~	"GOODHOPE",
      country	== "BOTSWANA" &
        province	== "MABUTSANE" &	district	== "MABUTSANE" ~	"MABUTSANE",
      country	== "BOTSWANA" &
        province	== "SELIBE PHIKWE" &
        district	== "SELIBE PHIKWE" ~	"SELIBE PHIKWE",
      country	== "BOTSWANA" &
        province	== "BOTETI" &	district	== "BOTETI" ~	"BOTETI",
      country	== "BOTSWANA" &
        province	== "CHOBE" &	district	== "CHOBE" ~	"CHOBE",
      country	== "BOTSWANA" &
        province	== "GREATER FRANCISTOWN" &
        district	== "FRANCISTOWN" ~	"FRANCISTOWN",
      country	== "BOTSWANA" &
        province	== "MAHALAPYE" &	district	== "MAHALAPYE" ~	"MAHALAPYE",
      country	== "BOTSWANA" &
        province	== "NORTH EAST" &	district	== "NORTH EAST" ~	"NORTH EAST",
      country	== "BOTSWANA" &
        province	== "JWANENG" &	district	== "JWANENG" ~	"JWANENG",
      country	== "BOTSWANA" &
        province	== "KWENENG EAST" &
        district	== "KWENENG EAST" ~	"KWENENG EAST",
      country	== "BOTSWANA" &
        province	== "NGAMI" &	district	== "NGAMI" ~	"NGAMI",
      country	== "BOTSWANA" &
        province	== "OKAVANGO" &	district	== "OKAVANGO" ~	"OKAVANGO",
      country	== "LIBERIA" &
        province	== "GRAND KRU" &	district	== "DORBOR" ~	"GRAND KRU",
      country	== "LIBERIA" &
        province	== "BONG" &	district	== "JORQUELLEH" ~	"BONG",
      country	== "LIBERIA" &
        province	== "LOFA" &	district	== "ZORZOR" ~	"LOFA",
      country	== "LIBERIA" &
        province	== "NIMBA" &	district	== "TAPPITA" ~	"NIMBA",
      country	== "LIBERIA" &
        province	== "BONG" &	district	== "KOKOYAH" ~	"BONG",
      country	== "LIBERIA" &
        province	== "GBARPOLU" &	district	== "GBARMA" ~	"GBARPOLU",
      country	== "LIBERIA" &
        province	== "GRAND CAPE MOUNT" &
        district	== "GARWULA" ~	"GRAND CAPE MOUNT",
      country	== "LIBERIA" &
        province	== "GRAND GEDEH" &	district	== "GBAO" ~	"GRAND GEDEH",
      country	== "LIBERIA" &
        province	== "GRAND GEDEH" &	district	== "KONOBO" ~	"GRAND GEDEH",
      country	== "LIBERIA" &
        province	== "LOFA" &	district	== "FOYA" ~	"LOFA",
      country	== "LIBERIA" &
        province	== "LOFA" &	district	== "SALAYEA" ~	"LOFA",
      country	== "LIBERIA" &
        province	== "LOFA" &	district	== "VOINJAMA" ~	"LOFA",
      country	== "LIBERIA" &
        province	== "MONTSERRADO" &
        district	== "CENTRAL MONROVIA" ~	"MONTSERRADO",
      country	== "LIBERIA" &
        province	== "RIVERCESS" &	district	== "DOEDAIN" ~	"RIVERCESS",
      country	== "LIBERIA" &
        province	== "SINOE" &	district	== "DUGBE RIVER" ~	"SINOE",
      country	== "LIBERIA" &
        province	== "BOMI" &	district	== "DOWEIN" ~	"BOMI",
      country	== "LIBERIA" &
        province	== "BOMI" &	district	== "KLAY" ~	"BOMI",
      country	== "LIBERIA" &
        province	== "BOMI" &	district	== "SENJEH" ~	"BOMI",
      country	== "LIBERIA" &
        province	== "BOMI" &	district	== "SUEHN MECCA" ~	"BOMI",
      country	== "LIBERIA" &
        province	== "GBARPOLU" &	district	== "BOKOMU" ~	"GBARPOLU",
      country	== "LIBERIA" &
        province	== "GBARPOLU" &	district	== "BOPOLU" ~	"GBARPOLU",
      country	== "LIBERIA" &
        province	== "GBARPOLU" &	district	== "KONGBA" ~	"GBARPOLU",
      country	== "LIBERIA" &
        province	== "GRAND BASSA" &
        district	== "district # 3A & 3B" ~	"GRAND BASSA",
      country	== "LIBERIA" &
        province	== "GRAND BASSA" &
        district	== "district # 4" ~	"GRAND BASSA",
      country	== "LIBERIA" &
        province	== "GRAND CAPE MOUNT" &
        district	== "COMMONWEALTH-C" ~	"GRAND CAPE MOUNT",
      country	== "LIBERIA" &
        province	== "GRAND CAPE MOUNT" &
        district	== "GOLAKONNEH" ~	"GRAND CAPE MOUNT",
      country	== "LIBERIA" &
        province	== "GRAND CAPE MOUNT" &
        district	== "PORKPA" ~	"GRAND CAPE MOUNT",
      country	== "LIBERIA" &
        province	== "GRAND CAPE MOUNT" &
        district	== "TEWOR" ~	"GRAND CAPE MOUNT",
      country	== "LIBERIA" &
        province	== "GRAND GEDEH" &	district	== "B'HAI" ~	"GRAND GEDEH",
      country	== "LIBERIA" &
        province	== "GRAND GEDEH" &	district	== "CAVALLA" ~	"GRAND GEDEH",
      country	== "LIBERIA" &
        province	== "GRAND GEDEH" &	district	== "PUTU" ~	"GRAND GEDEH",
      country	== "LIBERIA" &
        province	== "GRAND GEDEH" &	district	== "TCHIEN" ~	"GRAND GEDEH",
      country	== "LIBERIA" &
        province	== "GRAND KRU" &	district	== "BARCLAYVILLE" ~	"GRAND KRU",
      country	== "LIBERIA" &
        province	== "GRAND KRU" &	district	== "JRAOH" ~	"GRAND KRU",
      country	== "LIBERIA" &
        province	== "GRAND KRU" &	district	== "TREHN" ~	"GRAND KRU",
      country	== "LIBERIA" &
        province	== "MARYLAND" &	district	== "HARPER" ~	"MARYLAND",
      country	== "LIBERIA" &
        province	== "MARYLAND" &	district	== "KARLUWAY 2" ~	"MARYLAND",
      country	== "LIBERIA" &
        province	== "MONTSERRADO" &	district	== "BUSHROD" ~	"MONTSERRADO",
      country	== "LIBERIA" &
        province	== "NIMBA" &	district	== "SANNIQUELLEH MAHN" ~	"NIMBA",
      country	== "LIBERIA" &
        province	== "RIVER-GEE" &	district	== "SARBO" ~	"RIVER GEE",
      country	== "LIBERIA" &
        province	== "RIVER-GEE" &	district	== "TIENPO" ~	"RIVER GEE",
      country	== "LIBERIA" &
        province	== "RIVER-GEE" &	district	== "WEBBO" ~	"RIVER GEE",
      country	== "LIBERIA" &
        province	== "RIVERCESS" &	district	== "CENTRAL C" ~	"RIVERCESS",
      country	== "LIBERIA" &
        province	== "RIVERCESS" &	district	== "JOE RIVER" ~	"RIVERCESS",
      country	== "LIBERIA" &
        province	== "RIVERCESS" &	district	== "JOWEIN" ~	"RIVERCESS",
      country	== "LIBERIA" &
        province	== "RIVERCESS" &	district	== "TIMBO" ~	"RIVERCESS",
      country	== "LIBERIA" &
        province	== "RIVERCESS" &	district	== "YARNIE" ~	"RIVERCESS",
      country	== "LIBERIA" &
        province	== "SINOE" &	district	== "BUTAW" ~	"SINOE",
      country	== "LIBERIA" &
        province	== "SINOE" &	district	== "GBLONEE" ~	"SINOE",
      country	== "LIBERIA" &
        province	== "SINOE" &	district	== "GREENVILLE" ~	"SINOE",
      country	== "LIBERIA" &
        province	== "SINOE" &	district	== "JEADE" ~	"SINOE",
      country	== "LIBERIA" &
        province	== "SINOE" &	district	== "JEDEPO" ~	"SINOE",
      country	== "LIBERIA" &
        province	== "SINOE" &	district	== "KPANYAN" ~	"SINOE",
      country	== "LIBERIA" &
        province	== "SINOE" &	district	== "PYNES TOWN" ~	"SINOE",
      country	== "LIBERIA" &
        province	== "SINOE" &	district	== "TARJUWON" ~	"SINOE",
      country	== "LIBERIA" &
        province	== "SINOE" &	district	== "TARSUE" ~	"SINOE",
      country	== "LIBERIA" &
        province	== "BONG" &	district	== "PANTA" ~	"BONG",
      country	== "LIBERIA" &
        province	== "BONG" &	district	== "SALALA" ~	"BONG",
      country	== "LIBERIA" &
        province	== "BONG" &	district	== "SANOYEAH" ~	"BONG",
      country	== "LIBERIA" &
        province	== "GRAND BASSA" &	district	== "BUCHANAN" ~	"GRAND BASSA",
      country	== "LIBERIA" &
        province	== "GRAND BASSA" &	district	== "CAMP WOOD" ~	"GRAND BASSA",
      country	== "LIBERIA" &
        province	== "GRAND BASSA" &
        district	== "district # 1" ~	"GRAND BASSA",
      country	== "LIBERIA" &
        province	== "GRAND BASSA" &
        district	== "district # 2" ~	"GRAND BASSA",
      country	== "LIBERIA" &
        province	== "GRAND BASSA" &
        district	== "district # 3C" ~	"GRAND BASSA",
      country	== "LIBERIA" &
        province	== "GRAND BASSA" &	district	== "OWENSGROVE" ~	"GRAND BASSA",
      country	== "LIBERIA" &
        province	== "LOFA" &	district	== "VAHUN" ~	"LOFA",
      country	== "LIBERIA" &
        province	== "MONTSERRADO" &	district	== "TODEE" ~	"MONTSERRADO",
      country	== "LIBERIA" &
        province	== "NIMBA" &	district	== "SACLEPEA-MAHN" ~	"NIMBA",
      country	== "LIBERIA" &
        province	== "NIMBA" &	district	== "YARWEIN MEHNSOHNNEH" ~	"NIMBA",
      country	== "LIBERIA" &
        province	== "NIMBA" &	district	== "ZOE-GEH" ~	"NIMBA",
      country	== "LIBERIA" &
        province	== "BONG" &	district	== "SUAKOKO" ~	"BONG",
      country	== "LIBERIA" &
        province	== "MARGIBI" &	district	== "FIRESTONE" ~	"MARGIBI",
      country	== "LIBERIA" &
        province	== "MONTSERRADO" &	district	== "CAREYSBURG" ~	"MONTSERRADO",
      country	== "LIBERIA" &
        province	== "MONTSERRADO" &
        district	== "COMMONWEALTH" ~	"MONTSERRADO",
      country	== "LIBERIA" &
        province	== "MONTSERRADO" &
        district	== "SOMALIA DRIVE" ~	"MONTSERRADO",
      country	== "LIBERIA" &
        province	== "MONTSERRADO" &
        district	== "ST. PAUL RIVER" ~	"MONTSERRADO",
      country	== "LIBERIA" &
        province	== "MARGIBI" &	district	== "KAKATA" ~	"MARGIBI",
      country	== "LIBERIA" &
        province	== "RIVER GEE" &	district	== "CHEDEPO" ~	"RIVER GEE",
      country	== "LIBERIA" &
        province	== "RIVER GEE" &	district	== "POTUPO" ~	"RIVER GEE",
      country	== "LIBERIA" &
        province	== "BONG" &	district	== "KPAAI" ~	"BONG",
      country	== "LIBERIA" &
        province	== "GBARPOLU" &	district	== "BELLEH" ~	"GBARPOLU",
      country	== "LIBERIA" &
        province	== "GRAND KRU" &	district	== "BUAH" ~	"GRAND KRU",
      country	== "LIBERIA" &
        province	== "LOFA" &	district	== "KOLAHUN" ~	"LOFA",
      country	== "LIBERIA" &
        province	== "MARGIBI" &	district	== "MAMBAH-KABA" ~	"MARGIBI",
      country	== "LIBERIA" &
        province	== "MARYLAND" &	district	== "BAROBO FAJAH" ~	"MARYLAND",
      country	== "LIBERIA" &
        province	== "MARYLAND" &	district	== "BARROBO WHOJAH" ~	"MARYLAND",
      country	== "LIBERIA" &
        province	== "MARYLAND" &	district	== "KARLUWAY 1" ~	"MARYLAND",
      country	== "LIBERIA" &
        province	== "BONG" &	district	== "ZOTA" ~	"BONG",
      country	== "LIBERIA" &
        province	== "RIVER GEE" &	district	== "GBEAPO" ~	"RIVER GEE",
      country	== "LIBERIA" &
        province	== "NIMBA" &	district	== "GBEHLAY-GEH" ~	"NIMBA",
      country	== "LIBERIA" &
        province	== "MARGIBI" &	district	== "GIBI" ~	"MARGIBI",
      
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "KALENDA" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "KALONDA-EST" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "KAMIJI" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "KANDA KANDA" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "LUDIMBI-LUKULA" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "MULUMBA" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "TSHOFA" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "WIKONG" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "KAMANA" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "KALAMBAYI KABANGA" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "LUPUTA" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "MAKOTA" ~	"KASAI-ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "MWENE DITU" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "NGANDAJIKA" ~	"KASAI-ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "LUBAO" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LOMAMI" &	district	== "KABINDA" ~	"LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "BUTA" ~	"BAS UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "PANDA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "TITULE" ~	"BAS UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KAMBOVE" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KAMPEMBA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KIKULA" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KOWE" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "LIKASI" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "RWASHI" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "VANGU" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "KANZALA" ~	"KASAI-OCCIDENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "KALONDA-OUEST" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "TSHIKAPA" ~	"KASAI-OCCIDENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "KATOKA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "LUIZA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "LUKONGA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &
        district	== "NDESHA" ~	"KASAI-OCCIDENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &
        district	== "TSHIKAJI" ~	"KASAI-OCCIDENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "DILALA" ~	"LUALABA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "FUNGURUME" ~	"LUALABA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "KANZENZE" ~	"LUALABA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "LUALABA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "MANIKA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "NTAND EMBELO" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD UBANGI" &	district	== "GBADOLITE" ~	"NORD UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "BAGIRA-KASHA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "FIZI" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "IBANDA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "KABARE" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "KADUTU" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "KALONGE" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "KATANA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "KAZIBA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "MUBUMBANO" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "NUNDU" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "NYANGEZI" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "NYANTENDE" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "UVIRA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "WALUNGU" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "BOGOSE NUBEA" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "KABONDO" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "MANGOBO" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "TSHOPO" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "AKETI" ~	"BAS UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "GANGA" ~	"ORIENTALE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "BASANKUSU" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "BIKORO" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "BOLENGE" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "BOLOMBA" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "BOMONGO" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "DJOMBO" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "IBOKO" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "INGENDE" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "LILANGA BOBANGI" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "MBANDAKA" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "NTONDO" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "WANGATA" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KAMALONDO" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KATUBA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KISANGA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "LUBUMBASHI" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "MUMBUNDA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "TSHAMILEMBA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "BAKA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "KAMINA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "KINDA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "KITENGE" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &
        district	== "KABONDO-DIANDA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &
        district	== "MALEMBA-NKULU" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "SONGA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "DUNGU" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "ISIRO" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "PAWA" ~	"ORIENTALE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "RUNGU" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "WATSA" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "ADJA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "ARIWARA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "ARU" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "BAMBU-MINES" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "BUNIA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "KOMANDA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "LITA" ~	"ORIENTALE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "LOGO" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "LOLWA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "MAHAGI" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "MANGALA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "NIZI" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "NYANKUNDE" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "RWAMPARA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "BOBOZO" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "KANANGA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "BIPEMBA" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "BONZOLA" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "DIBINDI" ~	"KASAI-ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &	district	== "DIULU" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "KANSELE" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "LUBILANJI" ~	"KASAI-ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "LUKELENGE" ~	"KASAI-ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "MPOKOLO" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &	district	== "MUYA" ~	"KASAI-ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &	district	== "NZABA" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "BANDALUNGWA" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "BARUMBU" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "BINZA-METEO" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "BINZA-OZONE" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "BIYELA" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "BUMBU" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "GOMBE" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "KASA-VUBU" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "KIKIMI" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "KIMBANSEKE" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "KINGABWA" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "KINGASANI" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "KINSHASA" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "KINTAMBO" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "KISENSO" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "KOKOLO" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "KALAMU I" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "KALAMU II" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "LEMBA" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "LIMETE" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "LINGWALA" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "MAKALA" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "MATETE" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "MALUKU I" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "MASINA I" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "MASINA II" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "MONT-NGAFULA I" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "MONT-NGAFULA II" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "NDJILI" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "NGABA" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "NGIRI-NGIRI" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "NSELE" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "POLICE" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "SELEMBAO" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &
        district	== "BOKO-KIVULU" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "BOMA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS-CONGO" &	district	== "BOMA BUNGU" ~	"BAS-CONGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &
        district	== "GOMBE-MATADI" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "INGA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "KANGU" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "KIBUNZI" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "KIMPESE" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "KIMVULA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &
        district	== "KINKONZI" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "KISANTU" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "KITONA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "KIZU" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "MATADI" ~	"BAS-CONGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &
        district	== "MBANZA-NGUNGU" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "MUANDA" ~	"BAS-CONGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "NSONA-PANGU" ~	"BAS-CONGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "NZANZA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &
        district	== "SEKEBANZA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "TSHELA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "BANDUNDU" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "BULUNGU" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "IDIOFA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "KIKWIT-NORD" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "KIKWIT-SUD" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "MOSANGO" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "BOKORO" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "BANJOW MOKE" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "INONGO" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "KIRI" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "NIOKI" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "YUMBI" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "ALUNGULI" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "KAILO" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "KALIMA" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "KAMPENE" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "KASONGO" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "KIBOMBO" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "KINDU" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "LUBUTU" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "BINGA" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "LISALA" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "YAMONGILI" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "BENI" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "BUTEMBO" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "GOMA" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "KALUNGUTA" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "KARISIMBI" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "KATWA" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "MABALAKO" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "MUSIENENE" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "NYIRAGONGO" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "OICHA" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "VUHOVI" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD UBANGI" &	district	== "BOSOBOLO" ~	"NORD UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD UBANGI" &	district	== "BUSINGA" ~	"NORD UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD UBANGI" &	district	== "KARAWA" ~	"NORD UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD UBANGI" &	district	== "LOKO" ~	"NORD UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD UBANGI" &	district	== "MOBAYI" ~	"NORD UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "KOLE" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "LODJA" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "OTOTO" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "TSHUMBE" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "VANGA-KETE" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "GEMENA" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TANGANYIKA" &	district	== "ANKORO" ~	"TANGANYIKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TANGANYIKA" &	district	== "KABALO" ~	"TANGANYIKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TANGANYIKA" &	district	== "KALEMIE" ~	"TANGANYIKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TANGANYIKA" &	district	== "KANSIMBA" ~	"TANGANYIKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TANGANYIKA" &	district	== "KIAMBI" ~	"TANGANYIKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TANGANYIKA" &	district	== "KONGOLO" ~	"TANGANYIKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TANGANYIKA" &	district	== "MANONO" ~	"TANGANYIKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TANGANYIKA" &	district	== "MBULULA" ~	"TANGANYIKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TANGANYIKA" &	district	== "MOBA" ~	"TANGANYIKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TANGANYIKA" &	district	== "NYEMBA" ~	"TANGANYIKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TANGANYIKA" &	district	== "NYUNZU" ~	"TANGANYIKA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "BANALIA" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "BASALI" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "BASOKO" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "BENGAMISA" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "MAKISO-KISANGANI" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "OPALA" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "BEFALE" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "BOENDE" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "DJOLU" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "IKELA" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "WEMA" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "YALIFAFU" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "BOKO" ~	"KWANGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "BAMBO" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "KIROTSHE" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "LOLANGA MAMPOKO" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "YANGALA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "FEREKENI" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "ITOMBWE" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "YAHISULI" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "LIKATI" ~	"BAS UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "LOTUMBE" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KENYA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "MULONGO" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "FARADJE" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "WAMBA" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "RIMBA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &
        district	== "BENA-LEKA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &
        district	== "BENA-TSHIADI" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &
        district	== "BUNKONDE" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "KALOMBA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &
        district	== "LUBONDAYI" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "LUBUNGA" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "MUTOTO" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &
        district	== "TSHIBALA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "BIBANGA" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "CILUNDU" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "KABEYA KAMWANGA" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &	district	== "MIABI" ~	"KASAI-ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "MUKUMBI" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "TSHILENGE" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "TSHISHIMBI" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "KUIMBA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "LUOZI" ~	"BAS-CONGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "MANGEMBO" ~	"BAS-CONGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "MASA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &
        district	== "NGIDINGA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "NSELO" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "ILEBO" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "KAMUESHA" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "LUEBO" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "NDJOKO PUNDA" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "NYANGA" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "FESHI" ~	"KWANGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "KIMBAU" ~	"KWANGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "POPOKABAKA" ~	"KWANGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "TEMBO" ~	"BANDUNDU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "BAGATA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "SIA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "BOSOBE" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "KWAMOUTH" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "MIMIA" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "OSHWE" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "PENDJUA" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "LUSANGI" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "PANGI" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "BIENA" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "BIRAMBIZO" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "KAYNA" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "KYONDO" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "LUBERO" ~	"NORD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "MASEREKA" ~	"NORD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "PINGA" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "WALIKALE" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &
        district	== "HAUTS PLATEAUX UVIRA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "IDJWI" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "KALOLE" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "KAMITUGA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "KANIOLA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "KIMBI LULENGE" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "KITUTU" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "LEMERA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "MINEMBWE" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "MITI-MURRHESA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "MULUNGU" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "MWANA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "SHABUNDA CENTRE" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "BONDO" ~	"BAS UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "MONGA" ~	"ORIENTALE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "VIADANA" ~	"BAS UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "LUKOLELA" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "MONIEKA" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &
        district	== "MUFUNGA-SAMPWE" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "BUTUMBA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "KANIAMA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "KAYAMBA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "KINKONDJA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "LWAMBA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "MUKANGA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "ABA" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "BOMA MANGBETU" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &
        district	== "KILELA BALANDA" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "MITWABA" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "AUNGBA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "BIRINGI" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "BOGA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "GETHY" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "JIBA" ~	"ORIENTALE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "KAMBALA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "KILO" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "MANDIMA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "MONGBWALU" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "NIA-NIA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "NYARAMBE" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "RETHY" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "BILOMBA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "DEMBA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "DIBAYA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "KATENDE" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "MASUIKA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &
        district	== "MIKALAYI" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "MUETSHI" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &
        district	== "NDEKESHA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &
        district	== "TSHIKULA" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "CITENGE" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI-ORIENTAL" &
        district	== "KASANSA" ~	"KASAI ORIENTAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &
        district	== "KIMPANGU" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &
        district	== "KWILU-NGONGO" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "LUKULA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &
        district	== "SONA-BATA" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KONGO-CENTRAL" &	district	== "VAKU" ~	"KONGO CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "BANGA LUBAKA" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "BULAPE" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "DEKESE" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "KAKENGE" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "KAMONIA" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "KITANGWA" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "MIKOPE" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "MUSHENGE" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "MUTENA" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI" &	district	== "MWEKA" ~	"KASAI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KINSHASA" &	district	== "MALUKU II" ~	"KINSHASA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "KAHEMBA" ~	"BANDUNDU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "KAJIJI" ~	"KWANGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "KASONGOLUNDA" ~	"KWANGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "KENGE" ~	"KWANGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "KISANDJI" ~	"KWANGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "KITENDA" ~	"BANDUNDU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "PANZI" ~	"KWANGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "WAMBA LUADI" ~	"KWANGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "DJUMA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "GUNGU" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "IPAMU" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "KIKONGO" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "KIMPUTU" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "KINGANDU" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "LUSANGA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "MASI-MANIMBA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "MOANZA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "MOKALA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "MUKEDI" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "MUNGINDU" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "PAY KONGILA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "VANGA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "YASA-BONGA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "BOLOBO" ~	"BANDUNDU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MAINDOMBE" &	district	== "MUSHIE" ~	"MAINDOMBE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "KABAMBARE" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "KUNDA" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "PUNIA" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "SALAMABILA" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "TUNDA" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "BOSOMODANDA" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "LOLO" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "ALIMBONGO" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "KAMANGO" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "KIBUA" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "MUTWANGA" ~	"NORD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "MWESO" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "RUTSHURU" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "RWANGUBA" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "BUNYAKIRI" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "LULINGU" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "RUZIZI" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "BAFWAGBOGBO" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "BAFWASENDE" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "OPIENGE" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "YAKUSU" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "BOKUNGU" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "MOMPONO" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "MONDOMBE" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "ANGO" ~	"BAS UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "BILI" ~	"BAS UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "BAS UELE" &	district	== "POKO" ~	"BAS UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "IREBU" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "EQUATEUR" &	district	== "MAKANZA" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KAFUBU" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KASENGA" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KILWA" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "LUKAFU" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "PWETO" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "SAKANIA" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "BUKAMA" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT-LOMAMI" &	district	== "KABONGO" ~	"HAUT LOMAMI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "DORUMA" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "GOMBARI" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KASHOBWE" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "ADI" ~	"ORIENTALE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "ANGUMU" ~	"ORIENTALE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "FATAKI" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "LAYBO" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "MAMBASA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KASAI CENTRAL" &	district	== "LUAMBO" ~	"KASAI CENTRAL",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWANGO" &	district	== "MWELA LEMBWA" ~	"KWANGO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "KWILU" &	district	== "KOSHIBANDA" ~	"KWILU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "DILOLO" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "BUNKEYA" ~	"LUALABA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "KAFAKUMBA" ~	"LUALABA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "KALAMBA" ~	"LUALABA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "KAPANGA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "KASAJI" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "LUBUDI" ~	"LUALABA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "MUTSHATSHA" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "OBOKOTE" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MANIEMA" &	district	== "SAMBA" ~	"MANIEMA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "BONGANDANGA" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "BOSOMANZI" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "BOSONDJO" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "BUMBA" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "PIMU" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "YAMALUKA" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "MONGALA" &	district	== "YAMBUKU" ~	"MONGALA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "ITEBERO" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "MANGUREDJIPA" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "MASISI" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD UBANGI" &	district	== "ABUZI" ~	"NORD UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD UBANGI" &	district	== "WAPINDA" ~	"NORD UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD UBANGI" &	district	== "WASOLO" ~	"NORD UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD UBANGI" &	district	== "YAKOMA" ~	"EQUATEUR",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "KALEHE" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "MINOVA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-KIVU" &	district	== "MWENGA" ~	"SUD-KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "BENA-DIBELE" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "DIKUNGU" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "KATAKO-KOMBE" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "LOMELA" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "LUSAMBO" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "MINGA" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "PANIA MUTOMBO" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "TSHUDI-LOTO" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "WEMBO-NYAMA" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "ISANGI" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "LOWA" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "UBUNDU" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "WANIE-RUKULA" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "YABAONDO" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "YAHUMA" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "BUSANGA" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KAPOLOWE" ~	"KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT KATANGA" &	district	== "KIPUSHI" ~	"HAUT KATANGA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "MAKORO" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "HAUT UELE" &	district	== "NIANGARA" ~	"HAUT UELE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "TCHOMIA" ~	"ORIENTALE",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "LUALABA" &	district	== "SANDOA" ~	"LUALABA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "BINZA" ~	"NORD KIVU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "BANGABOLA" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "BOKONZI" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "BOMINENGE" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "BOTO" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "BUDJALA" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "BULU" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "BWAMANDA" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "KUNGU" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "LIBENGE" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "MAWUYA" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "MBAYA" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "NDAGE" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "TANDALA" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SUD-UBANGI" &	district	== "ZONGO" ~	"SUD-UBANGI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "DJALO-NDJEKA" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "SANKURU" &	district	== "OMENDJADI" ~	"SANKURU",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "YALEKO" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHOPO" &	district	== "YALIMBONGO" ~	"TSHOPO",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "LINGOMO" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "TSHUAPA" &	district	== "MONKOTO" ~	"TSHUAPA",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "DAMAS" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "ITURI" &	district	== "LINGA" ~	"ITURI",
      country	== "DEMOCRATIC REPUBLIC OF THE CONGO" &
        province	== "NORD KIVU" &	district	== "KIBIRIZI" ~	"NORD KIVU",
      TRUE ~ province),
    AFRO_block = case_when(
      country	=="NIGERIA" ~	"LCB",
      country	=="NIGER" ~	"LCB",
      country	=="CAMEROON" ~	"LCB", 
      country	=="CHAD" ~	"LCB",
      country	=="CENTRAL AFRICAN REPUBLIC" ~	"LCB",
      country	=="ALGERIA" ~	"WA",
      country	=="BURKINA FASO" ~	"WA",
      country	=="MAURITANIA" ~	"WA",
      country	=="MALI" ~	"WA",
      country	=="GUINEA" ~	"WA",
      country	=="GHANA" ~	"WA",
      country	=="TOGO" ~	"WA",
      country	=="BENIN" ~	"WA",
      country	=="COTE D IVOIRE" ~	"WA",
      country	=="SIERRA LEONE" ~	"WA",
      country	=="LIBERIA" ~	"WA",
      country	=="GUINEA-BISSAU" ~	"WA",
      country	=="GAMBIA" ~	"WA",
      country	=="SENEGAL" ~	"WA",
      country	=="DEMOCRATIC REPUBLIC OF THE CONGO" ~	"CEA",
      country	=="CONGO" ~	"CEA",
      country	=="GABON" ~	"CEA",
      country	=="EQUATORIAL GUINEA" ~	"CEA",
      country	=="RWANDA" ~	"CEA",
      country	=="BURUNDI" ~	"CEA",
      country	=="ANGOLA" ~	"CEA",
      country	=="KENYA" ~	"ES",
      country	=="ERITREA" ~	"ES",
      country	=="ETHIOPIA" ~	"ES",
      country	=="SOUTH SUDAN" ~	"ES",
      country	=="UGANDA" ~	"ES",
      country	=="TANZANIA" ~	"ES",
      country	=="MALAWI" ~	"SNA",
      country	=="ZAMBIA" ~	"SNA",
      country	=="MOZAMBIQUE" ~	"SNA",
      country	=="ZIMBABWE" ~	"SNA",
      country	=="BOTSWANA" ~	"SNA",
      country	=="NAMIBIA" ~	"SNA",
      country	=="ESWATINI (KINGDOM)" ~	"SNA",
      country	=="LESOTHO(KINGDOM" ~	"SNA",
      country	=="MADAGASCAR" ~	"ISD",
      country	=="COMOROS" ~	"ISD",
      country	=="SEYCHELLES" ~	"ISD",
      country	=="MAURITIUS" ~	"ISD",
      country	=="CAPE VERDE" ~	"ISD",
      country	=="SAO TOME AND PRINCIPE" ~	"ISD"))

LQAS_result<-G |> 
  mutate(start_date = as_date(start_date),
         year = year(start_date)) |> 
  select(AFRO_block, country, province, district , response , vaccine.type , roundNumber, numbercluster , start_date, end_date, year, male_sampled, female_sampled, total_sampled, male_vaccinated, female_vaccinated , total_vaccinated, total_missed, status , performance, r_non_compliance, r_house_not_visited, r_childabsent, r_child_was_asleep, r_child_is_a_visitor, r_vaccinated_but_not_FM, other_r, prct_care_giver_informed_SIA, prct_non_compliance, prct_house_not_visited, prct_childabsent, prct_child_was_asleep, prct_child_is_a_visitor, prct_vaccinated_but_not_FM, prct_security, prct_childnotborn, prct_other_r) |>
  arrange(start_date) |> 
  mutate(start_date = case_when(
    response == "NIE-2024_nOPV2" & roundNumber == "Rnd1" ~ as_date("2024-03-06"),
    response == "NIE-2024_nOPV2" & roundNumber == "Rnd3" ~ as_date("2024-10-02"),
    response == "MOZ-2024-04-01_bOPV" & roundNumber == "Rnd3" ~ as_date("2024-08-03"),
    TRUE~start_date))
LQAS_result$rnd_distinct <- paste(LQAS_result$country,LQAS_result$province, LQAS_result$district, LQAS_result$response, LQAS_result$roundNumber, LQAS_result$start_date, sep = "_")

LQAS_result<-LQAS_result |> 
  distinct(rnd_distinct, .keep_all = TRUE)


# scpe <- read_excel("C:/Users/TOURE/Mes documents/REPOSITORIES/SCOPE/data.xlsx")
# 
# scope<-scpe |>
#   rename(roundn = `Round Number`,
#          response = `OBR Name`,
#          country = country,
#          round_start_date = `Round Start Date`) |> 
#   mutate(roundNumber = case_when(
#     roundn =="Round 0"~ "Rnd0",
#     roundn =="Round 1"~ "Rnd1",
#     roundn =="Round 2"~ "Rnd2",
#     roundn =="Round 3"~ "Rnd3",
#     roundn =="Round 4"~ "Rnd4",
#     roundn =="Round 5"~ "Rnd5",
#     roundn =="Round 6"~ "Rnd6")) |> 
#   select(country, response, roundNumber, round_start_date)
# LQAS_rep <- left_join(LQAS_result, scope,  
#                       by=c("country" = "country",
#                            "response" = "response",
#                            "roundNumber" = "roundNumber")) |> 
#   select(AFRO_block, country, province, district , response , vaccine.type , roundNumber, round_start_date, numbercluster , start_date, end_date, year, male_sampled, female_sampled, total_sampled, male_vaccinated, female_vaccinated , total_vaccinated, total_missed, status , performance, r_non_compliance, r_house_not_visited, r_childabsent, r_child_was_asleep, r_child_is_a_visitor, r_vaccinated_but_not_FM, other_r, prct_care_giver_informed_SIA, prct_non_compliance, prct_house_not_visited, prct_childabsent, prct_child_was_asleep, prct_child_is_a_visitor, prct_vaccinated_but_not_FM, prct_security, prct_childnotborn, prct_other_r) |> 
#   rename(lqas_start_date = start_date,
#          lqas_end_date = end_date) |>
#   mutate(lqas_start_date = as_date(lqas_start_date),
#          round_start_date = as_date(round_start_date)) |> 
#   mutate(lqas_start_date = ifelse(round_start_date > lqas_start_date, (round_start_date + 4), lqas_start_date)) |>
#   mutate(lqas_end_date = ifelse(round_start_date > lqas_start_date, (round_start_date + 4), lqas_start_date)) |> 
#   mutate(round_start_date = as_date(round_start_date),
#          lqas_start_date = as_date(lqas_start_date),
#          lqas_end_date = as_date(lqas_end_date)) |> 
#   mutate(round_start_date = as_date(round_start_date)) |> 
#   mutate(month_yr = format(as.Date(round_start_date), "%Y-%m"))

write_csv(LQAS_result,"C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/AFRO_LQAS_data_c.csv")











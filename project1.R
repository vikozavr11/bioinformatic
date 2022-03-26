library(readr)
library(dplyr)
library(ggplot2)
install.packages("stringr")
library(stringr)


clinic1<-read.delim("clinical_aaa.txt", skip=4)
clinic2<-read.delim("clinical_afaff.txt", skip=4)
clinic3<-read.delim("clinical456.txt",skip=4)
clinic4<-read.delim("clinucal_qwerty.txt",skip=4)
mut1<-read.delim("mut2.txt")
mut2<-read.delim("mutations_1.txt")
mut3<-read.delim("muts_1234.txt")
mut4<-read.delim("muts.txt")

cancer_genes<-function(clinical_table, mut_table) {
  
   mut_table<- mut_table%>% 
      filter(Hugo_Symbol %in% c("TP53", "TTN", "PTEN")) %>%
        group_by(Tumor_Sample_Barcode)%>%
         summarise(count=n())
   
   mut_table$Tumor_Sample_Barcode<-str_sub(mut_table$Tumor_Sample_Barcode,start = 1, end=12)
   
   cancer <- clinical_table %>%
     select(PATIENT_ID, OS_STATUS) %>%
     right_join(mut_table, by = c("PATIENT_ID" = "Tumor_Sample_Barcode"))
   
   cancer
}

cancer1<-cancer_genes(clinic1, mut1)
#рак молочных желез
cancer2<-cancer_genes(clinic2, mut3)
#рак кишечника
cancer3<-cancer_genes(clinic3, mut4)
#рак почек

cancer1 <- cancer1 %>% mutate(Study = 1)
cancer2 <- cancer2 %>% mutate(Study = 2)
cancer3 <- cancer3 %>% mutate(Study = 3)

cancer <- bind_rows(cancer1, cancer2, cancer3)

plot1 <- ggplot(cancer, aes(x=OS_STATUS, y=count)) +
  geom_boxplot() +
  facet_grid(~Study)

ggsave("C:/Users/vikoz/Desktop/plot.png", plot1, width = 20, height = 40, units = "cm")


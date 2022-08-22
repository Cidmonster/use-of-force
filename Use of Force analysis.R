library(readxl)
library(dplyr)
library(stringr)

setwd("~/Dropbox/Police records/Use of Force Reports/")

uof19 <- read_xlsx("2019 UOF Reports.xlsx")
uof20 <- read_xlsx("2020 UOF Reports.xlsx")
uof21 <- read_xlsx("UOF_2021_REDACTED.xlsx")
uof22 <- read_xlsx("UOF_2022_REDACTED.xlsx")

uofs2122narrow <- rbind(uof21, uof22) %>% select(1:12,14:17,19:24) %>% distinct() %>% write.csv("UOFS 2021 and 2022 narrow.csv")
uofs2122 <- rbind(uof21, uof22) %>% write.csv("UOFS 2021 and 2022.csv")
View(uof21)
write.csv(uofs2122, "UOFS 2021 and 2022.csv")

uofs <- rbind(uof19, uof20) %>% rbind(uof21)

dates <- uof19 %>% select(6) %>% distinct() %>% summarise(count = n())

uof19less <- uof19 %>% select(1:7, 10:31, 34:40)
uof20less <- uof20 %>% select(1:7, 10:13, 20:31, 34:40)

uofsmod <- rbind(uof20less, uof21less)

violations2122 <- uofs2122 %>%  filter(`Inc: Disposition` != "Within Policy"|`Alg: Finding` != "Within Policy") %>% distinct()

names(uof21less) <- names(uof20less)

badgecount <- uofs %>% select(2, 3, 7, 29, 30, 31) %>% distinct() %>% group_by(`Off: Badge/ID number`) %>% summarise(records = n()) %>% arrange(desc(records))

roster <- uofs %>% select(2, 3, 29, 30, 31) %>% distinct() %>% left_join(badgecount, by = "Off: Badge/ID number")

enforcers <- uofs %>% select(2, 3, 29, 30, 31) %>% distinct() %>%
  filter(`Off: Badge/ID number` == 1103 
         |`Off: Badge/ID number` == 497 
         |`Off: Badge/ID number` == 240 
         |`Off: Badge/ID number` == 1021 
         |`Off: Badge/ID number` == 1949
         |`Off: Badge/ID number` == 2504)

payroll <- read.csv("~/Dropbox/Police records/City payroll.csv")
allcops <- payroll %>% filter(Division == "Police") %>% filter(str_detect(Job.Description, "Officer"))

coplist <- roster %>% full_join(allcops, by =c("Off: Last name" = "Last.Name", "Off: First name" = "First.Name")) %>% distinct()
write.csv(coplist, "Use of Force records v payroll.csv")


uofs %>% select(6, 7, 12) %>% distinct() %>% 
  group_by(`Inc: Service rendered`) %>% 
  summarise(records = n()) %>% 
  arrange(desc(records))

moonlighting <- uofs %>% filter(`Inc: Service rendered` == "Secondary Employment")

foreign <- uofs %>% filter(`Inc: Service rendered` == "Foreign Agency")

uofscount <- select(uofs, -21, -26, -35, -36, -37, -38) %>% distinct() 

sust <- uofs %>% filter(`Alleg: Finding` != "Within Policy" | `Inc: Disposition` !="Within Policy") %>% 
  select(-21, -26, -36, -37) %>% distinct()

distinct <- uofs %>% select(`Inc: Incident Number`,`Inc: Occurred date`) %>% distinct() 

distinctcrisis <- uofs %>% select(`Inc: Incident Number`, `Inc: Narrative`, `Inc: Occurred date`, `Inc: Category`, `Inc: Officer assement of cit condition`) %>%
  filter(`Inc: Officer assement of cit condition` == "Behavioral Crisis Event") %>% distinct() 

                                                                                                               #uofscount %>% group_by(`Off: Badge/ID number`) %>% summarise(records = n()) %>% arrange(desc(records))

off277 <- uofs %>% filter(`Off: Badge/ID number` == 277)
off497 <- uofs %>% filter(`Off: Badge/ID number` == 497)
off1103 <- uofs %>% filter(`Off: Badge/ID number` == 1103)

numbers <- uofs %>% distinct(`Inc: File Number`)

noresist <- uofs %>% select(`Inc: Incident Number`, `Inc: UOF: Citizen resistance`, `Inc: Use-of-force: Reason`) %>% 
 # filter(`Inc: UOF: Citizen resistance` == "0-No Resistance") %>%
  distinct()


distinct <- uofs %>% select(`Inc: Incident Number`, `Inc: UOF: Citizen resistance`, `Inc: Use-of-force: Reason`) %>% 
  # filter(`Inc: UOF: Citizen resistance` == "0-No Resistance") %>%
  distinct()

selleny <- uofs %>% select('Inc: Incident Number', 'Inc: Occurred date', 'Off: Badge/ID number', 'Off: Last name', 'Off: First name', 'Inc: Narrative', 'Alleg: Allegation', 'Alleg: Finding', 'Inc: Disposition') %>% 
  distinct() %>% filter(str_detect(`Off: Badge/ID number`,"1873"))

donitzen <- uofs %>% select('Inc: Incident Number', 'Inc: Occurred date', 'Off: Badge/ID number', 'Off: Last name', 'Off: First name', 'Inc: Narrative', 'Alleg: Allegation', 'Alleg: Finding', 'Inc: Disposition') %>% 
  distinct() %>% filter(str_detect(`Off: Last name`,"Donitzen"))

sowders <- uofs %>% select('Inc: Incident Number', 'Inc: Occurred date', 'Off: Badge/ID number', 'Off: Last name', 'Off: First name', 
                           'Inc: Narrative', 'Alleg: Allegation', 'Alleg: Finding', 'Inc: Disposition', 'Inc: Category', 'Inc: Officer assement of cit condition') %>% 
  distinct() %>% filter(str_detect(`Off: Last name`,"Sowders"))

behavioral <- uofs %>% select('Inc: Incident Number', 'Inc: Occurred date', 'Off: Badge/ID number', 'Off: Last name', 'Off: First name', 
                           'Inc: Narrative', 'Inc: Officer assement of cit condition', 'Inc: Citizen went to hospital', 'Inc: Citizen was arrested') %>% 
              filter(str_detect(`Inc: Officer assement of cit condition`,"Behavior")) %>% distinct()

level3 <- uofs %>% select('Inc: Incident Number', 'Off: Badge/ID number', 'Off: Last name', 'Off: First name', 'Inc: Narrative', 'Inc: Category', 'Inc: UOF: Type force used',
                'Inc: Occurred date', 'Inc: Occurred time', 'Alleg: Allegation', 'Alleg: Finding', 'Inc: Disposition') %>% 
  distinct() %>% filter(str_detect(`Inc: Category`,"Level 3"))

shooting <- uofs %>% select('Inc: Incident Number', 'Off: Badge/ID number', 'Off: Last name', 'Off: First name', 'Inc: Narrative', 'Inc: Category', 'Inc: UOF: Type force used',
                          'Inc: Occurred date', 'Inc: Occurred time', 'Alleg: Allegation', 'Alleg: Finding', 'Inc: Disposition') %>% 
  distinct() %>% filter(str_detect(`Inc: UOF: Type force used`,"FIT"))


places <- uofs %>% select('Inc: Incident Number', 'Off: Badge/ID number', 'Off: Last name', 'Off: First name', 'Inc: Occurred date', 'Inc: Occurred time', 'Cit: Race', 'Addr: Street number', 'Addr: Street name', 'Cit: Race') %>% distinct()

bydate <- uofs %>% 
  select(`Inc: Incident Number`, `Inc: Narrative`, `Inc: Occurred date`, `Cit: First name`, 
         `Cit: Last name`, `Off: Last name`, `Off: First name`, `Off: Badge/ID number`) %>% distinct() 

uofs %>%  select(`Inc: Incident Number`, `Inc: Narrative`, `Inc: Occurred date`, `Cit: First name`, 
                 `Cit: Last name`, `Off: Last name`, `Off: First name`, `Off: Badge/ID number`, `Cit: Race`, `Off: Race`) %>% 
                  distinct() %>% group_by(`Cit: Race`, `Off: Race`) %>% summarise(count = n())

uofs %>% filter(`Cit: Last name` == "Jones" & `Cit: First name` == "Gregory") %>% 
  select(`Inc: Incident Number`, `Inc: Narrative`, `Inc: Occurred date`, `Cit: First name`, 
                `Cit: Last name`, `Off: Last name`, `Off: First name`, `Off: Badge/ID number`, `Cit: Race`, `Off: Race`) %>% 
  distinct()

num <- uofs %>%  select(`Inc: Incident Number`, `Inc: Narrative`, `Inc: Occurred date`, 
                        `Off: Last name`, `Off: First name`, `Off: Badge/ID number`) %>% 
  distinct()

noresist <- uofs %>%  select(`Inc: Incident Number`, `Inc: Narrative`, `Inc: Occurred date`, `Inc: Occurred time`, `Inc: UOF: Citizen resistance`, 
                             `Inc: UOF: Type force used`) %>% 
  distinct() %>% filter(str_detect(`Inc: UOF: Citizen resistance`,"No "))

# `Inc: Disposition`, `Alleg: Finding`
'[1] "Inc: Incident type"                               "Inc: District/Bureau"                            
[3] "Inc: Unit/Platoon"                                "Inc: Occurred date"                              
[5] "Inc: Occurred time"                               "Inc: Incident Number"                            
[7] "Inc: File Number"                               #  "Addr: Street number"                             
# [9] "Addr: Street name"                                "Inc: Narrative"                                  
[11] "Inc: Use-of-force: Reason"                        "Inc: Service rendered"                           
[13] "Inc: UOF: involved citizen distance from officer" "Cit: Last name"                                  
[15] "Cit: First name"                                  "Cit: Race"                                       
[17] "Cit: Sex"                                         "Cit: Citizen link type"                          
[19] "Cit: Citizen role"                                "Inc: Citizen was injured"                        
[21] "Inc: UOF: Citizen condition/injury"               "Inc: Citizen went to hospital"                   
[23] "Inc: UOF: involved citizen build"                 "Inc: UOF: involved citizen height"               
[25] "Inc: Citizen was arrested"                        "Inc: UOF: Citizen resistance"                    
[27] "Inc: Officer assement of cit condition"           "Inc: Officer was injured"                        
[29] "Off: Last name"                                   "Off: First name"                                 
[31] "Off: Badge/ID number"                             #"Off: Race"                                       
# [33] "Off: Sex"                                         "Inc: Officer went to hospital"                   
[35] "Inc: UOF: Effective or not"                       "Inc: UOF: Type force used"                       
[37] "Inc: Category"                                    "Alleg: Allegation"                               
[39] "Alleg: Finding"                                   "Inc: Disposition" '

uof21less <- uof21 %>% select(1:24, 26:31)
rbind(uof21, uof2)

#"UOF: Member condition/injury" 

names(uofs)<-str_replace_all(names(uofs), c(" " = "" , "," = "" ))
names(uofs)<-str_replace_all(names(uofs), c(":" = "" , "," = "" ))
names(uofs)<-str_replace_all(names(uofs), c("." = "" , "," = "" ))

setnames(uof21less, old = c('a','d'), new = c('anew','dnew'))

uof21less %>% rename_at(vars(names(uof21less)), ~ names(uof20less))

as.vector(names(uof20less))
uof21less %>% everything() %>% rename(as.vector(names(uof21less)))

#names uofs2122
#[1] "Inc: Incident type"                      "Inc: Org:  District/Bureau"
#[3] "Inc: Org:  Unit/Platoon"                 "Inc: Occurred date"                     
#[5] "Inc: Occurred time"                      "Inc: Incident Number"                   
#[7] "Inc:  File Number"                       "Inc: Narrative"                         
#[9] "UOF: Reason for using force"             "Inc: Service rendered"                  
#[11] "UOF: Citizen's distance from member"     "UOF: Citizen was injured (y/n)"         
#[13] "UOF: Citizen condition/injury"           "UOF: Citizen went to hospital (y/n)"    
#[15] "UOF: Citizen's build"                    "UOF: Citizen's height"                  
#[17] "UOF: Citizen was arrested (y/n)"         "UOF: Citizen resistance"                
#[19] "UOF: Member assessment of cit condition" "UOF: Member was injured (y/n)"          
#[21] "Mem: Last name"                          "Mem: First name"                        
#[23] "Mem: Badge/ID number"                    "UOF: Member went to hospital (y/n)"     
#[25] "UOF: Member condition/injury"            "UOF: ECD: Effective (y/n/limited)"      
#[27] "UOF: Type of force used"                 "Inc: Category"                          
#[29] "Alg: Allegation"                         "Alg: Finding"                           
#[31] "Inc: Disposition"                       

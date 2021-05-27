
library(tidyverse)


# Preenchimento das bandeiras


bandeira <- data.frame("N_Est" = c("0050781988","0056608334",'0060210697','0061098091','0066376146','0066376153','0066376161','0028308146','0024614729','0037935194','0037948320',
                                   '0037948759','0035025808','0035025816','0035025808','0035025816','0037935699',
                                   '0050770007','0050770015','0050770023',
                                   "0030044887",'0037935434',
                                   '0035025345','0035025352','0037946787',
                                   '0039247440',
                                   '0039247424',
                                   '0046626826'),
           
                       "Estabelecimento" = c(rep("Esperiencias Cerveceras SA",7),rep("Patagonia Brewing Company",4),
                                             rep("Parque Quilmes", 6),
                                             rep("Cap. Federal",3),
                                             rep("Food Trucks | Paragonia",2),
                                             rep("Patagonia Brewing Company",3),
                                             'Patagonia City Bell',
                                             'Patagonia 20 Y 50',
                                             'Clasicos de Quilmes'),
                       
                       "Nome" = c(rep("Bariloche",11),
                                  rep("Qilmes Este",6),
                                  rep("Cap. Federal",3),
                                  rep('Cap. Federal',2),
                                  rep('Cap. Federal',3),
                                  'Prov BS Aires',
                                  'Prov BS Aires',
                                  'Cap. Federal'
                                  ),
                       
                       "Banco" = c(rep("Santander", 3),"",rep("Santander",3),rep("CitiBanck",4),
                                   rep("CitiBank",6),
                                   rep("Santander",3),
                                   rep("CitiBank",2),
                                   rep("CitiBank",3),
                                   'Santander',
                                   'Santander',
                                   'Santander'),
                       
                       "Bandeira" = c("Visa","Visa",'Mastercard','Cabral','Visa','Cabral','Mastercard','Cabral','Visa','Mastercard','Visa',
                                      "Visa",'Cabral','Visa','Cabral','Visa','Mastercard',
                                      "Visa",'Cabral','Mastercard',
                                      "Visa",'Martercard',
                                      'Cabral','Visa','Martercard',
                                      'Visa',
                                      'Visa',
                                      'Visa'))



d1 <- merge(d1, bandeira, by.x = "x2",by.y = "N_Est", all.x = c("Nome",'Banco'))


end <- rbind(data.frame(x1 = c("Nome"),
                        x2 = c(d1[,4])),
             end, data.frame(x1 = c("Banco","Bandeira"),
                             x2 = c(d1[,5], d1[,6])))

data.frame(x1 = c("Banco","Bandeira"),
          x2 = c(d1[,5], d1[,6]))





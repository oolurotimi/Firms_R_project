

 import excel "/Users/OsaretinOlurotimi2/Dropbox/Mac/Desktop/Climate and Micro firms/Firms_R_project/Data/World bank climate data/No of days with heat index above 35.xlsx", sheet("all (2)") firstrow clear

 rename A statename
 
  merge 1:m statename using "/Users/OsaretinOlurotimi2/Dropbox/Mac/Desktop/Climate and Micro firms/Firms_R_project/climateandfirms.dta"
  
  
  //Preliminary regressions
  
  reghdfe logb6 daysabv35_16 daysabv35_17 daysabv35_15 daysabv35_18 daysabv35_19 sector a20b a20a a18 latitude longitude

  

 
 
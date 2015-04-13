## This part generates the complete dataframe.
#### two global variable:
## colname: columns I want to keep for further analysis
colname = c("place","div/tot","name","hometown","ag","gun",
            "net","time")
## paths: generate all the filenames
paths = list.files("~/Desktop/STA242/homework1/data")

# There are 4 main functions in this project. In each main function,
# several short functions would be called and they are listed in the 
# end.
###############  Step1: use read.fwf to read one file ########################
## This function will use read.fwf function to read in all the files
Readin_one_table = function(filename){
  ## use "process_file" function to delete space line,
  ## footnote and *# symbol
  ## save the new file named "file_adj"
  process_file(filename)
  file = readLines("file_adj")
  ### special case for women2001: no "==" line
  if(grepl("women10Mile_2001",filename)){
    ## the width of women2001 is the same as men2001. use men2001
    ## width as width for women2001. 
    index2001 = grep("- WOMEN",file)
    width = width_get(readLines("men10Mile_2001"),index2001+12)
    raw_data = read.fwf("file_adj",widths=width,skip=index2001+1)
    names(raw_data) = c("place","num","name","ag","hometown","net","gun")
  }  
  else{
      index_equ = line_skip(file)
       ## obtain the width to partition all the columns for each variable
       width = width_get(file,index_equ)
       raw_data = read.fwf("file_adj",widths=width,skip=index_equ-2)
       col_name = tolower(unlist(raw_data[1,]))
      ## get rid of all the blank space
       col_name = gsub(" {1,}","",col_name)
       names(raw_data) = col_name
      ## first two lines are headers and "="
       raw_data = raw_data[-c(1,2),]
  }
  raw_data
}
###########################################

################step2:formatting the dataframe from step 1################
##############Goal:1.keep columns we need;2.process special cases:
### 2006 split two columns; 2001: without female header
format_table = function(data,colname,filename){
  ## special case for women2001:same format as men2001,use men2001's format
  if (grepl("women10Mile_2001",filename)){
    ## for women2001,remove "num" columns,create four new columns:
    ## div/tot,year,time and sex
      men_sub = cbind(data[,-2],"div/tot" = rep(NA,nrow(data)),
                   time=rep(NA,nrow(data)),year=rep(2001,nrow(data)),
                    sex=rep("female",nrow(data)))  
  }
  else{
      process_file(filename)
      file = readLines("file_adj")
      ## index to find the header line
      index = line_skip(file)
      ## get the header line
      header = tolower(file[index-1])
      header = gsub(" ","",header)
     ## find which columns we want are in the dataframe.
     head_split = match_header(header,colname)
     ## column are in the dataframe
     head_yes = head_split[[1]]
     ## column names are not in the dataframe
     head_no = head_split[[2]]
     ## subset the original dataset,which contains columns we want already
     var_name = data_sub(head_split,data)
     men_sub = data[,which(colnames(data) %in% var_name)]
     ## columnbind columns doesn't exist in original dataset, but we want in
     ## colname vector.we set those columns with values NA.
     men_sub = bind_data(head_no,men_sub)
     ## generate year and gender columns
     men_sub = year_sex(men_sub,filename)
     ## split the special case of 2006:hometown and netime in one column 
     if(grepl("(^women|^men)10Mile_2006",filename)){
       men_sub = split_home_nettime(men_sub)    
     }   
  }
  men_sub
}
##############################################################################

################### step3: get one formatted table ############################
#####combine step1 and step2. Format the column names of one table
get_one_table = function(filename){
  colname = c("place","div/tot","name","hometown","ag","gun",
              "net","time")
  ## readin the raw data with all columns
  raw_data = Readin_one_table(filename)
  ## subset the data with columns we want
  formatted_data = format_table(raw_data,colname,filename)
  ## modify columns names to standard format
  names(formatted_data) = gsub("[[:space:]]", "", names(formatted_data))
  names(formatted_data)[names(formatted_data) == 'guntim'] = "gun"
  names(formatted_data)[names(formatted_data) == 'nettim'] = "net"
  names(formatted_data)[names(formatted_data) == 'netti'] = "net" 
  ## change variable names to the same order
  formatted_data = formatted_data[c(colname,"sex","year")] 
}
##################################################

########### step4: loop over all the filenames to get the final dataframe######
Get_all_table = function(paths){
  data_final = data.frame()
  for(i in 1:length(paths)){
    format_one_table = get_one_table(paths[i])
    data_final = rbind(data_final,format_one_table)   
  }
  data_final
}
########## step 5: transform variables and generate new variables to analyze # 
options(stringsAsFactors = FALSE)
data = Get_all_table(paths)
## transform some columns into approporiate data type
data[,c("place","ag")] = sapply(data[,c("place","ag")],as.numeric)
data[,c("name","hometown")] = sapply(data[,c("name","hometown")],as.character)
data$year = as.numeric(paste(data$year))
## some years don't have net_time or gun_time,using time variable as
## substitute.Create a new varaible:race_time.For years having gun_time:
## race_time=gun_time. For years without gun_time: race_time = time.
race_time=c()
race_time = sapply(1:nrow(data),function(i){
  if(is.na(data[i,"gun"])) race_time[i]= data[i,"time"]
  else race_time[i] = data[i,"gun"]})
data = cbind(data,race_time)
## convert time into numeric value using "convert_time" function
data[,c("time","gun","net","race_time")] = 
  sapply(data[,c("time","gun","net","race_time")],convert_time)

################## functions to call in each step ##########################

############## 1.process_file ###############
##get rid of "#","*" ,lines that are all blank and footnote.Then save 
##as a new temporary file in order to generate dataframe for
## the origin file.
process_file = function(filename){
  ## read the file in
  file = readLines(filename)
  ## get rid of footnote
  if(length(grep("^[#\\*] U[A-z].|^[0-9]/.",file))!=0){
    file = file[-c(grep("^[#\\*] U[A-z].|^[0-9]/.",file))]
    ## get rid of # and * in data
    file = gsub("[#*]"," ",file)
    ## get rid of blank lines
    blanks = grep("^[[:blank:]]*$",file)
    file = file[-c(blanks)]   
  }
  else {
    file = gsub("[#*]"," ",file)
    # get rid of blank lines
    blanks = grep("^[[:blank:]]*$",file)
    file = file[-c(blanks)]
  }  
  writeLines(file,"file_adj")
}
##############
############# 2. line_skip ######################
#####find the "==" row in each file
line_skip = function(file){
  grep("==",file)
}
#############
################ 3.width_get #########################
######### generate width for read.fwf function 
width_get = function(data,index){
  column_split = data[index]
  split = strsplit(column_split," {1,}")
  w = sapply(1:length(split[[1]]),function(x) nchar(split[[1]][x]))
  w = w+1
  w  
}
###################
############### 4. match_header #########################
## match each dataframe's header with colnames. Get the column names that
## exist in the table and column names that don't exist in the table.
match_header = function(headRow,col_name){
  no_name=character()
  have_name=character()
  for (i in 1: length(col_name)){
    match = regexpr(col_name[i],headRow)[[1]]
    if(match!=-1) {
      have_name = c(have_name,col_name[i])
    }
    else no_name = c(no_name,col_name[i])
  }
  
  list(have_name,no_name) 
}
####################
############ 5.data_sub ##############
## get the names from each dataframe's column names(these names represent that
## these columns are in the original dataframe already)
data_sub = function(head_split,data){
  save_name = character()
  head_yes = head_split
  var_name = colnames(data)
  for(i in 1:length(var_name)){
    match = regexpr(substring(var_name[i],1,2),head_yes)
    if(length(unique(match))==2) save_name = c(save_name,var_name[i])
  }
  save_name
}
#################
############# 6. bind_data #############
###function to bind specified vector with existed dataframe
bind_data = function(head_no,men_sub){
  non_matrix = matrix(NA,nrow=nrow(men_sub),ncol=length(head_no))
  non_matrix = as.data.frame(non_matrix)
  colnames(non_matrix) = head_no
  cbind(men_sub,non_matrix)  
}
#############
########## 7.year_sex ##############
#### function to add year and sex
year_sex = function(data,filename){
  if(grepl("^men",filename)) data = cbind(data,sex=rep("male",nrow(data)))
  else data = cbind(data,sex=rep("female",nrow(data)))
  years = gsub("[A-z]+[0-9][0-9][A-z]+_","",filename)
  data = cbind(data,year=rep(years,nrow(data)))
}

##########
########## 8.split_home_nettime #############
## split hometown and net_time in year2006
split_home_nettime = function(data){
  vector = as.character(data$hometownnettim)
  result = strsplit( gsub("([0-9]*:*[0-9]*:[0-9]*)","~\\1~",vector), "~" )
  net = c()
  hometown =c()
  for(i in 1:length(result)){
    hometown[i] = result[[i]][1]
    net[i] = result[[i]][2]
  }
  cbind(data[,-5],hometown,net)
}
##################
######## 8. convert_time ################
## function to convert time into minute
convert_time = function(time_input){
  time_input = as.character(time_input)
  con_time = rep(0,length(time_input))
  for(i in 1: length(time_input)){
    split_time = as.numeric(strsplit(time_input[i],':')[[1]])
    if(length(split_time) >2){
      con_time[i] = split_time[1]*60 + split_time[2] + split_time[3]/60 
    }
    else{
      con_time[i] = split_time[1] + split_time[2]/60
    }
  }
  con_time  
}






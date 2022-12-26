# collated rolling average 3, 5, and 10 yr data

tmpdir = 'tmp' #point to directories

if(!dir.exists(tmpdir)){dir.create(tmpdir)} #create temporary directory - this creates a directory in your working directory called ‘tmp’

drive_auth() # authorize googledrive to access your Google Drive -- follow browser instructions or the prompts in the console.

did = shared_drive_find('EPSCoR_SWAT')$id #find shared drive id

folder1_id = drive_ls(path = as_id(did), pattern = 'SWAT_collatedFiles')$id

folder2_id = drive_ls(path = as_id(folder1_id), pattern = 'rollingave')$id #get folder id

csv_files <- drive_ls(folder2_id, type = "csv")

#downloads all files in the "collated" folder into temporary folder in R
csv_files %>% 
  split(csv_files$id) %>% 
  walk(~drive_download(.$id, path = file.path("tmp", .$name), overwrite = TRUE))

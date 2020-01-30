# chia.R --------------------------------------------------------------------------------------
#' By Fabrício Kury -- fab {at) kury.dev | fk2374 [at} cumc.columbia.edu
#' 
#' This script performs various manipulations on the chia dataset as required for post-processing
#' as well as for producing statistics for the manuscript.
#' 
#' Coding start: 2019/2/~28
#' 
#' Margin column is at 100 characters.
##


# Packages ------------------------------------------------------------------------------------
require(tibble)
require(stringr)
require(dplyr)
require(readr)


# Backbone functions --------------------------------------------------------------------------
global_variables_cleanup_prev <- ls() # The location of this is very important. Has to be before
 # anything is declared.

curtime <- function() format(Sys.time(), "%Y-%m-%d %H-%M")
curdate <- function() format(Sys.time(), '%Y-%m-%d')
scope <- function(expr) { 
  # Evaluates expression within a temporary scope/environment.
  eval(substitute(expr))
}
rdsWrap <- function(varname, exprs, pass_val = F,
  assign_val = T, rds_dir = def_rds_dir, override = global_rds_override) {
  # Check if varname exists in the RDS dir. If it does, read it from there. If it does
  # not, evaluate exprs and assign the result to varname.
  # If pass_val is true, this function returns the value of varname at the end.
  varname <- deparse(substitute(varname))
  rds_file <- paste0(rds_dir, varname, '.rds')
  if(!override && file.exists(rds_file)) {
    console("Reading '", varname, "' from file '", rds_file, "'.")
    var_val <- readRDS(rds_file)
  } else {
    var_val <- eval.parent(substitute(exprs), 1)
    console("Saving '", varname, "' to file '", rds_file, "'.")
    if(!dir.exists(rds_dir))
      dir.create(rds_dir, recursive = T)
    saveRDS(var_val, rds_file)
  }
  if(assign_val)
    assign(varname, var_val, envir = parent.frame(n = 1))
  if(pass_val | !assign_val)
    var_val
}
console <- function(...) cat(paste0(..., '\n'))
ensureDir <- function(...) {
  dir_path <- paste0(...)
  if(!dir.exists(dir_path))
    dir.create(dir_path, recursive = T)
  dir_path
}
trim <- function(x) {
  # Thanks to https://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace.
  gsub("^\\s+|\\s+$", "", x)
}
beginProgressReport <- function(job_size, frequency = def_prog_report_freq) {
  assign('progress_report_iterator', 0, envir = .GlobalEnv)
  assign('progress_report_job_size', job_size, envir = .GlobalEnv)
  assign('progress_report_frequency', frequency, envir = .GlobalEnv)
}
iterateProgress <- function() {
  if(!((progress_report_iterator <<- progress_report_iterator+1)%%floor(
    progress_report_job_size*progress_report_frequency)))
    console(round(100*progress_report_iterator/progress_report_job_size), '%')
}
tblClipboard <- function(a) {
  # Writes the table to the clipboard.
  write.table(a, 'clipboard', row.names = F, col.names = T, sep = ',')
}

# Global variables ----------------------------------------------------------------------------
setwd(paste0('C:/Users/', Sys.getenv("USERNAME"), '/OneDrive/IB/DBMI/Projetos/chia'))

exec_timestamp <- curdate()
exec_time <- curtime()

global_rds_override = F # If on, will ignore existing RDS cache and rebuild them.
if(global_rds_override)
  message('Global RDS override is on. All .rds files will be recreated.')

def_prog_report_freq <- 0.05
debug_limit <- Inf # Set to less than Inf to cap the number of documents for debugging
data_dir <- 'Dados/'

chia_version <- 's7'
output_version <- 's10'

def_rds_dir <- paste0(data_dir, 'rds/', output_version, '/')

output_dir <- ensureDir('Saída/', output_version, '/')
brat_dir <- ensureDir(output_dir, 'brat/')
sample_dir <- paste0(data_dir, 'Sample/')
chia_s_filepath <- paste0(sample_dir, 'chia - s1.txt')
chia_dir <- paste0(data_dir, 'ann/', chia_version, '/')

s1_seed <- 575 # Generated randomly from [1, 1000] by random.org

chia_entities <- c(
  'Scope', 'Person', 'Condition', 'Drug', 'Observation', 'Measurement',
  'Procedure', 'Device', 'Visit', 'Negation', 'Qualifier', 'Temporal',
  'Value', 'Multiplier', 'Reference_point', 'Line', 'Mood', 'Non-query-able',
  'Post-eligibility', 'Pregnancy_considerations', 'Competing_trial', 'Informed_consent',
  'Intoxication_considerations', 'Non-representable', 'Subjective_judgement', 'Context_Error')
scope_rel_name <- 'scope_relationship'
scope_rel_type <- 'Relationship'
chia_v_rel <- c('v-AND', 'v-OR', scope_rel_name)
chia_h_rel <- c('h-OR', 'multi')
chia_directional_rel <- c('v-AND', 'v-OR', 'multi')
chia_nondirectional_rel <- c('h-OR')
chia_attributes <- 'Optional'
chia_relationships <- c(chia_v_rel, chia_h_rel, 'Optional')
chia_entrels <- c(chia_entities, chia_relationships)


# Application-specific functions --------------------------------------------------------------
downloadTrial <- function(nct) {
  require(stringr)
  require(readr)
  require(xml2)
  
  to_brat <- function(of, body)
    cat(file=of, sep = '', append=TRUE, paste0(clean_text(body), '\n'))
  get_criteria <- function(m)
    str_match_all(m, '<li style="margin-top:0\\.7ex;">(.*)</li>')[[1]][,2]
  unescape_html <- function(str)
    xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
  clean_text <- function(m) {
    m <- unescape_html(str_replace_all(m, '\n|\\|\\n', ' '))
    while(grepl('  ', m))
      m <- str_replace_all(m, '  ', ' ')
    m
  }
  
  inc_crit_start_string <- '<p style="margin-top:1ex; margin-bottom:1ex;">Inclusion Criteria:</p>'
  inc_crit_end_string <- '<p style="margin-top:0ex; margin-bottom:1ex;">Exclusion Criteria:</p>'
  exc_crit_start_string <- '<p style="margin-top:0ex; margin-bottom:1ex;">Exclusion Criteria:'
  exc_crit_end_string <- '</td>'
  all_crit_start_string <- '<th class="header3 banner_color rowHeader">Eligibility Criteria&nbsp;<sup style="color:blue">'
  all_crit_end_string <- '<th class="header3 banner_color rowHeader">Sex/Gender </th>'
  
  inclusion_outfile <- paste0(brat_dir, nct, '_inc.txt')
  if(file.exists(inclusion_outfile))
    file.remove(inclusion_outfile)
  exclusion_outfile <- paste0(brat_dir, nct, '_exc.txt')
  if(file.exists(exclusion_outfile))
    file.remove(exclusion_outfile)
  
  html_file <- read_file(paste0('https://clinicaltrials.gov/ct2/show/record/', nct))
  
  inclusion_criteria_tb <- substring(html_file,
    regexpr(inc_crit_start_string, html_file, ignore.case = T)+nchar(inc_crit_start_string),
    regexpr(inc_crit_end_string, html_file, ignore.case = T))
  inclusion_criteria <- get_criteria(inclusion_criteria_tb)
  
  exclusion_criteria_tb <- substring(html_file,
    regexpr(exc_crit_start_string, html_file, ignore.case = T)+nchar(exc_crit_start_string))
  exclusion_criteria_tb <- substring(exclusion_criteria_tb, 1,
    regexpr(exc_crit_end_string, exclusion_criteria_tb, ignore.case = T))
  exclusion_criteria <- get_criteria(exclusion_criteria_tb)
  
  if(length(inclusion_criteria)) {
    for(c in inclusion_criteria)
      to_brat(inclusion_outfile, c)
    for(c in exclusion_criteria)
      to_brat(exclusion_outfile, c)
  } else {
    message('Unable to distinguish inclusion vs exclusion criteria in ', nct, '.')
    all_criteria_tb <- substring(html_file,
      regexpr(all_crit_start_string, html_file, ignore.case = T),
      regexpr(all_crit_end_string, html_file, ignore.case = T))
    for(c in get_criteria(all_criteria_tb))
      to_brat(inclusion_outfile, c)
    to_brat(exclusion_outfile, 'NA')
  }
  
  inclusion_ann_outfile <- paste0(brat_dir, nct, '_inc.ann')
  file.create(inclusion_ann_outfile)
  exclusion_ann_outfile <- paste0(brat_dir, nct, '_exc.ann')
  file.create(exclusion_ann_outfile)
  TRUE
}

readANN <- function(ann_filename) {
  if(!exists('rows_missing_coordinates', envir = .GlobalEnv))
    assign('rows_missing_coordinates', tibble(), envir = .GlobalEnv)
  
  ann_lines <- readLines(paste0(chia_dir, ann_filename))
  if(!length(ann_lines)) {
    # File ann_filename is empty.
    warning('File ', ann_filename, ' is empty.')
    empty_files <<- c(empty_files, ann_filename)
    return(NULL)
  }

  ann_tbl <- str_match(ann_lines, '(\\S*)\\t(?:(\\S*) ?([^\\t]*))(?:\\t([^\\t]*))?')
  ann_tbl <-  as_tibble(ann_tbl)[,-1]
  colnames(ann_tbl) <- c('t', 'entrel', 'coords', 'text')
  ann_tbl <- ann_tbl[ann_tbl$entrel %in% chia_entrels,]
  if(!nrow(ann_tbl)) {
    # No entities found (maybe the only one that existed got filtered out)
    no_entity_files <<- c(no_entity_files, ann_filename)
    return(NULL)
  }
  
  ## TODO: Revise the lines below.
  ann_tbl$t[ann_tbl$t=="*"] <- NA
  ann_tbl$coord.start <- as.integer(NA)
  ann_tbl$coord.end <- as.integer(NA)
  ann_tbl$arg1 <- as.character(NA)
  ann_tbl$arg2 <- as.character(NA)
  ann_tbl$text[ann_tbl$text==''] <- as.character(NA)
  
  ## Parse the start and end coordinates from the ANN syntax.
  # The coordinates of the relationships are specified by the entities they connect, so
  # first we fill the coordinates of the entities, then the relationships.
  
  # Entities.
  for(i in which(ann_tbl$entrel %in% chia_entities)) {
    # Parse the potentially multiple frags into a simple vector of coordinates. We
    # only need the smallest and the biggest numbers.
    all_coords <- as.integer(unlist(str_match_all(ann_tbl$coords[i], '[^ ;]+')))
    ann_tbl$coord.start[i] <- min(all_coords)
    ann_tbl$coord.end[i] <- max(all_coords)
  }
  
  # Add IDs to the ORs
  if(any(row_selection <- ann_tbl$entrel=='OR'))
    ann_tbl$t[row_selection] <- paste0('OR', 1:sum(row_selection))
  
  # Non-directional relationships -- the OR. The difference is that it can contain
  # an arbitrary number ([2:Inf)) of arguments (and don't use the "ArgX:" prefix).
  # "Melt" into multiple rows the coords variable, then fill their coord.start/end.
  if(any(row_selection <- ann_tbl$entrel %in% chia_nondirectional_rel)) {
    calc_rows <- apply(ann_tbl[row_selection,], 1,
      function(e) {
        # This function splits the contents of the coords field into joined pairs.
        # It's actually kind of amazing that this mess works.
        # Filter out the "ArgX:" that sometimes appears. This seems to be a bug in brat.
        coords <- gsub('Arg\\d:', '', e['coords'])
        
        # Split the contents.
        e_tbl <- data.frame(as.list(e))
        e_tbl <- e_tbl[, !(names(e_tbl) %in% c('arg1', 'arg2'))]
        molten_coords <- str_match_all(coords, '\\S+')[[1]]
        
        # Assemble te pairs.
        molten_coords_df <- cbind(1:(length(molten_coords)-1), 2:length(molten_coords))
        mcdfv <- adply(molten_coords_df, 1, function(v) molten_coords[v])[,-1]
        cbind(e_tbl[rep(1, nrow(mcdfv)),], mcdfv)
      })
    calc_rows <- do.call(rbind, calc_rows)
    
    # Clean some mess.
    colnames(calc_rows) <- c('t', 'entrel', 'coords', 'text', 'coord.start', 'coord.end', 'arg1', 'arg2')
    calc_rows$coord.start <- as.integer(calc_rows$coord.start)
    calc_rows$coord.end <- as.integer(calc_rows$coord.end)
    calc_rows$text <- ''
    
    # Add the calculated rows to the rest of the data
    ann_tbl <- rbind(ann_tbl[!row_selection,], calc_rows)
    remove(calc_rows)
    remove(row_selection)
  }
  
  # Parse the arguments from directional relationships.
  for(i in which(ann_tbl$entrel %in% chia_directional_rel)) {
    ann_tbl$arg1[i] <- str_match(ann_tbl$coords[i], 'Arg1:(\\w+)')[-1]
    ann_tbl$arg2[i] <- str_match(ann_tbl$coords[i], 'Arg2:(\\w+)')[-1]
  }
  
  # # TODO: Remove these two variable below.
  # nct <- substr(ann_filename, 1, 11)
  # ie <- substr(ann_filename, 13, 15)
  # if(nct == 'NCT00122070' && ie == 'inc')
  #   browser()
  
  # Now treat all directional and non-directonal relationships like directional relationships.
  # Directional relationships have 2 and only 2 arguments. We enforced that in our pre-treatment
  # ("melting") of the non-directional relationships.
  for(i in which(ann_tbl$entrel %in% c(chia_directional_rel, chia_nondirectional_rel))) {
    # Fetch the row (entity) indicated by the arguments.
    arg1 <- ann_tbl[!is.na(ann_tbl$t) & ann_tbl$t == ann_tbl$arg1[i], c('coord.start', 'coord.end')]
    arg2 <- ann_tbl[!is.na(ann_tbl$t) & ann_tbl$t == ann_tbl$arg2[i], c('coord.start', 'coord.end')]
  
    # Find the correct order, then store. Notice the little trick in the "as.integer()". It
    # works because arg1/2 contain only the coordinate columns.
    arg1 <- as.integer(arg1)
    arg2 <- as.integer(arg2)
    ann_tbl$coord.start[i] <- min(arg1, arg2)
    ann_tbl$coord.end[i] <- max(arg1, arg2)
  }
  
  
  # Attributes -- have only one argument.
  for(i in which(ann_tbl$entrel %in% chia_attributes)) {
    # Put the argument into its field for consistency's sake.
    ann_tbl$arg1[i] <- ann_tbl$coords[i]
    
    # Fetch the row (entity) indicated by the arguments
    arg <- ann_tbl[!is.na(ann_tbl$t) & ann_tbl$t == ann_tbl$arg1[i], c('coord.start', 'coord.end')]
    
    # Store the coordinates in the correct order. Notice the little trick in the "as.integer()". It
    # works because arg1/2 contain only the coordinate columns.
    arg <- as.integer(arg)
    ann_tbl$coord.start[i] <- min(arg)
    ann_tbl$coord.end[i] <- max(arg)
  }
  
  # Add extra info
  ann_tbl$nct <- substr(ann_filename, 1, 11)
  ann_tbl$ie <- substr(ann_filename, 13, 15)
  
  # Reorder the columns after all the mess, and convert values
  ann_tbl <- ann_tbl[, c('nct', 'ie',  't', 'entrel', 'coords', 'text',
                       'coord.start', 'coord.end', 'arg1', 'arg2')]

  # Do some error checking: missing coordinates.
  if(!exists('rows_missing_coordinates', envir = .GlobalEnv))
    assign('rows_missing_coordinates', tibble(), envir = .GlobalEnv)
  if(any(row_selection <- is.na(ann_tbl$coord.start) | is.na(ann_tbl$coord.end))) {
    message('Found missing coordinates at ', ann_filename, ' at ', ifelse(sum(row_selection) > 1,
      'rows ', 'row '), paste0(which(row_selection), collapse = ', '), '.')
    message('These might be relationships pointing to entities that do not/no longer exist.',
            ' Probably a bug in brat.')
    message('These rows (annotations) will be removed:')
    print(ann_tbl[row_selection,])
    rows_missing_coordinates <<- bind_rows(rows_missing_coordinates, ann_tbl[row_selection,])
    ann_tbl <- ann_tbl[-which(row_selection),]
  }
  
  for(i in which(is.na(ann_tbl$coord.start) | is.na(ann_tbl$coord.end)))
    stop('Still found missing coordinates at ', ann_filename, ', ann_tbl[', i, ']. Fatal error.')
  
  # Check for swapped coordinates in the wrong order (coord.start greater than coord.end) in
  # the most archaic, hardcoded, failproof way.
  for(i in which(ann_tbl$coord.start[i] > ann_tbl$coord.end[i]))
    stop('Found swapped coordinates at ', ann_filename, ', ann_tbl[', i, ']. Fatal error.')
  
  # Return a sorted tibble.
  ann_tbl[order(ann_tbl$coord.start),]
}

produceChiaTibble <- function(in_dir) {
  require(plyr)
  
  console('Searching for ANN files in folder ', in_dir, '.')
  ann_list <- list.files(in_dir, '.*\\.ann', recursive = T)
  console('Found ', length(ann_list), ' ANN files.')
  
  if(debug_limit < Inf & length(ann_list) > debug_limit) {
    message('Will subset to ', debug_limit, ' files for debugging purposes.')
    ann_list <- ann_list[1:debug_limit]
  }

  console('Will read and process ', length(ann_list), ' ANN files.')
  chia <- vector('list', length(ann_list))
  names(chia) <- ann_list
  
  # Empty the variable 'rows_missing_coordinates' (and ensure it exists).
  assign('rows_missing_coordinates', tibble(), envir = .GlobalEnv)
  
  empty_files <<- vector('list', length(ann_list))
  no_entity_files <<- vector('list', length(ann_list))
  beginProgressReport(length(ann_list))
  for(ann_file in ann_list) {
    chia[[ann_file]] <- readANN(ann_file)
    iterateProgress()
  }
  chia <- do.call(rbind, chia)
  chia <- as_tibble(chia)
  console('Completed.')
  
  console('Will read the criteria text files, to parse their line (criteria) breaks.')
  txt_list <- sub('ann', 'txt', ann_list)
  beginProgressReport(length(txt_list))
  line_breaks <- vector("list", length = length(txt_list))
  names(line_breaks) <- txt_list
  criteria_text <<- vector("list", length = length(txt_list))
  names(criteria_text) <<- txt_list
  for(txt_file in txt_list) {
    # Parse the line breaks and store for later.
    free_text <- read_file(paste0(chia_dir, txt_file))
    lb <- gregexpr('(\\r\\n)|\\n', free_text)[[1]]
    line_breaks[[txt_file]] <- lb
    
    # Store the free text to be exported later.
    criteria_text_df <- tibble(
      nct = substr(txt_file, 1, 11),
      ie = substr(txt_file, 13, 15),
      line = 1:length(lb),
      criterion = character(length(lb)))
    for(i in 1:length(lb)) {
      start_index <- ifelse(i==1, 0, lb[i-1]+1)
      end_index <- lb[i]-1
      criteria_text_df[i, 'criterion'] <- trim(substr(free_text, start_index, end_index))
    }
    
    # Remove empty rows at the end of the data.frame, but only at the end, so as to not affect row indexes.
      while(nrow(criteria_text_df) > 0
        && !nchar(criteria_text_df$criterion[nrow(criteria_text_df)]))
        criteria_text_df <- criteria_text_df[-nrow(criteria_text_df),]
    
    # Store it in a global variable to be accessed later.
    criteria_text[[txt_file]] <<- criteria_text_df
    
    iterateProgress()
  }
  console('Completed.')
  console('Will add line (criteria) numbers to the annotations.')
  beginProgressReport(nrow(chia))
  chia$line <- 0
  for(i in 1:nrow(chia)) {
    lb <- line_breaks[[paste0(chia$nct[i], '_', chia$ie[i], '.txt')]]
    coord.min <- min(chia$coord.start[[i]], chia$coord.end[[i]], na.rm = T)
    for(l in length(lb):1) # Find the line where the entity lies.
      if(coord.min >= lb[[l]]) {
        chia[[i, 'line']] <- l+1
        break
      } else if(l == 1)
        chia[[i, 'line']] <- l
    iterateProgress()
  }
  
  # Drop the pesky row names.
  rownames(chia) <- NULL
  
  # Drop empty text -- no time to figure out how to elegantly solve this "at the source"
  chia$text[nchar(chia$text)==0] <- NA
  
  console('Completed.')
  detach(package:plyr)
  chia
}

bratLink <- function(ann_tbl_row) {
  paste0('http://45.76.6.224:8010/index.xhtml#/chia/s1/', ann_tbl_row$nct[1], '_',
         ann_tbl_row$ie[1], '?focus=', ann_tbl_row$t[1])
}

sampleOneAnnotation <- function(ann = chia) {
  i <- sample(1:nrow(ann), 1)
  brat_link <- bratLink(ann[i,])
  console('One random annotation:')
  print(ann[i,])
  console('Link: ', brat_link, ' (copied to clipboard)')
  writeClipboard(brat_link)
}


global_variables_cleanup_after <- ls() # The location of this is very important. Has to be after
 # everything is declared.
# Global export variables ---------------------------------------------------------------------
# empty_files <- NULL
# no_entity_files <- NULL


# Execution starts here -----------------------------------------------------------------------
console('Script execution started at ', curtime(), '.')


# Load and preprocess chia --------------------------------------------------------------------
# Load list of sampled trials.
if(F) {
  chia_s <- readLines(chia_s_filepath)
  beginProgressReport(length(chia_s))
  lapply(chia_s1,
    function(e) {
      downloadTrial(e)
      iterateProgress()
    })
  remove(chia_s)
}

# Load and preprocess chia.
if(T)
  rdsWrap(chia, {
    console('Will read ANN files from "', chia_dir, '" and create Chia tibble.')
    chia <- produceChiaTibble(chia_dir)
    console('Completed.')
    console('Will add types of annotations for reporting purposes.')
    chia <- left_join(chia,
      rdsWrap(assign_val = F, type_map, read_csv('Dados/chia - type_map.csv')),
      by = c('entrel' = 'Annotation'))
    console('Completed.')
    
    # Find the roots.
    findRoots <- function() {
      console('Will identify the roots of the annotation graphs.')
      # Assign all entities as potential roots, the rest as not.
      chia$is_root <<- F
      chia$is_root[chia$entrel %in% chia_entities] <<- T
      chia <<- rdsWrap(override = T, assign_val = F, pass_val = T, chia_roots, {
        # Process by search spaces: lines. Roots pertain to individual lines (individual criteria).
        processRoots <- function(ag) {
          getDescendantsRecursive <- function(ent, traveled_path = NULL) {
            if(is.null(ent) | (ent %in% traveled_path))
              return(NULL)
            traveled_path <- union(traveled_path, ent)
            
            vertical_kins <- subset(ag,
              (entrel %in% chia_v_rel) & arg1 == ent,
              select = arg2)
            horizontal_kins <- subset(ag,
              (entrel %in% chia_h_rel) & (arg1 == ent | arg2 == ent),
              select = c(arg1, arg2))
            
            # Flatten args 1 and 2
            horizontal_kins <- unique(c(as.matrix(horizontal_kins)))
            vertical_kins <- unique(c(as.matrix(vertical_kins)))
            
            # Combine into one vector, without element ent.
            kins <- unique(c(horizontal_kins, vertical_kins))
            kins <- kins[kins!=ent]
            
            # Find next the kins that can still be roots.
            kins <- unique(ag$t[ag$is_root & (ag$t %in% kins)])
            
            # Do the recursion
            descendants <- unique(unlist(lapply(kins, getDescendantsRecursive, traveled_path)))
            
            # Return
            union(traveled_path, descendants)
          }
          
          # Tome todos os destinos das relações verticais.
          children <- unique(ag$arg2[ag$entrel %in% chia_v_rel])
          # Para cada destino (children), inicie a descida pela árvore de anotação
          non_roots <- unique(unlist(lapply(children, getDescendantsRecursive)))
          # Assinale-as como não-raízes
          ag$is_root[ag$t %in% non_roots] <- F
          ag
        }
        
        chia %>% group_by(nct, ie, line) %>% do(processRoots(.))
      })
      console('Completed.')
    }
    if(T)
      findRoots()
    
    # Create scope relationships.
    if(T) {
      console('Will bind scope relationships to the rest of Chia.')
      chia <- bind_rows(chia,
        rdsWrap(override = T, scope_join, assign_val = F, pass_val = T, {
          require(sqldf)
          require(writexl)
          
          # Create scope relationships between the scopes and their roots only.
          console('Will generate scope relationships according to the overlaps.')
          scope_join <- as_tibble(sqldf(paste0('
            select a.nct as nct, a.ie as ie, a.line as line,
            null as t, "', scope_rel_name,'" as entrel, null as text,
            "Arg1:"||a.t||" Arg2:"||b.t as coords,
            a."coord.start" as "coord.start", a."coord.end" as "coord.end",
            a.t as arg1, b.t as arg2, "', scope_rel_type, '" as Type, 0 as "is_root"
            from chia as a inner join chia as b 
            on a.nct = b.nct and a.ie = b.ie and a.line = b.line and a.t <> b.t
            and a."coord.start" <= b."coord.start" and a."coord.end" >= b."coord.end"
            and a.entrel = "Scope"
            and b.is_root
            '))) # Have I told you I love SQL? I mean it. It's true.
          
          # Assign scope relationship t's
          assignScopeRelationshipT <- function(ag) {
            ag$t <- paste0('SR', 1:length(ag$t))
            ag
          }
          scope_join <- scope_join %>% group_by(nct, ie) %>% do(assignScopeRelationshipT(.))
          console('Completed. There are ', nrow(scope_join), ' scope links in chia.')
          
          scope_join
        }))
      chia <- chia[with(chia, order(nct, ie, line, coord.start, t)),]
      console('Completed. Chia now contains ', nrow(chia), ' explicit annotations (rows).')
      
      # Write scope relationships to file
      if(F)
      {
        out_xlsx <- paste0(output_dir, 'scope_join ', exec_timestamp, '.xlsx')
        console('Will write scope relationships separately to file ', out_xlsx)
        write_xlsx(list('scope_join'=scope_join), out_xlsx)
        console('Completed.')
      }
    }
    
    # Find roots again, after the scope relationships have been created.
    if(T)
      findRoots()
    
    # Compute the depth and length of the annotation graphs.
    # TODO: Fix this!
    if(F) {
      console('Will compute the height and length of the annotation graphs.')
      chia$level <- NA
      chia$length <- NA
      
      findLevels <- function(ag) {
        ag_v_rels <- filter(ag, entrel %in% chia_v_rel)
        ag_h_rels <- filter(ag, entrel %in% chia_h_rel)
        
        findLevelRecursion <- function(a, cur_level = 1, cur_length = 1, cur_path = list()) {
          if(a$t %in% cur_path)
            return(data.frame(NA))
          cur_path <- c(cur_path, a$t)
          
          if(is.na(a$level) || cur_level < a$level)
            ag[ag$t == a$t,]$level <<- cur_level
          
          if(is.na(a$length) || cur_length > a$length)
            ag[ag$t == a$t,]$length <<- cur_length
          
          # Take all vertical relationships departing from this node.
          selection <- ag_v_rels$arg1 == a$t
          if(any(selection)) {
            children_nodes <- filter(ag, t %in% ag_v_rels$arg2[selection])
            children_nodes %>% rowwise() %>% do(findLevelRecursion(.,
              cur_level+1, cur_length+1, cur_path))
          }
          
          # Take all horizontal relationships departing from or coming to this node.
          selection <- ag_h_rels$arg1 == a$t | ag_h_rels$arg2 == a$t
          if(any(selection)) {
            sibling_nodes <- filter(ag,
              t %in% ag_h_rels$arg2[selection] | t %in% ag_h_rels$arg1[selection])
            sibling_nodes %>% rowwise() %>% do(findLevelRecursion(.,
              cur_level, cur_length+1, cur_path))
          }
          
          data.frame(NA)
        }
        
        ag[ag$is_root,] %>% rowwise() %>% do(findLevelRecursion(.))
        ag
      }
      
      chia <- chia %>% group_by(nct, ie, line) %>% do(findLevels(.))
      console('Completed.')
    }
    
    chia
  })

# Translate scope relationships.
# TODO: Finish this. Do not run yet.
if(F) {
  require(sqldf)
  
  console('Will translate scope relationships: relationships inciding over scopes are broadcast ',
    'to the roots contained inside them.')
  
  chia_temp <- filter(chia, nct == 'NCT00182520', ie == 'inc', line == '3')
  towards_scope_relationships <- as_tibble(sqldf(paste0('
    select a.nct, a.ie, b.t||a.rowid as t, a.entrel,
    "Arg1:"||a.arg1||" Arg2:"||b.arg2 as coords,
    null as text, a.`coord.start`, b.`coord.end`,
    a.arg1, b.arg2, a.line, a.Type, a.is_root, a.level, a.length,
    a.nct||"-"||a.ie||"-"||b.t||a.rowid as uid,
    a.uid as original_relationship, b.uid as original_scope_relationship
    from chia_temp as a inner join chia_temp as b
    on a.nct = b.nct and a.ie = b.ie and a.line = b.line
      and b.entrel = "scope_relationship"
      and a.entrel <> "scope_relationship"
      and a.arg2 = b.arg1
      and a.t <> b.t
    ')))
  
  chia_temp <- filter(chia, nct == 'NCT00317148', ie == 'exc', line == '6')
  from_scope_relationships <- as_tibble(sqldf(paste0('
    select a.nct, a.ie, b.t||a.rowid as t, a.entrel,
    "Arg1:"||b.arg2||" Arg2:"||a.arg2 as coords,
    null as text, a.`coord.start`, a.`coord.end`,
    b.arg2, a.arg2, a.line, a.Type, a.is_root, a.level, a.length,
    a.nct||"-"||a.ie||"-"||b.t||a.rowid as uid,
    a.uid as original_relationship, b.uid as original_scope_relationship
    from chia_temp as a inner join chia_temp as b
    on a.nct = b.nct and a.ie = b.ie and a.line = b.line
    and b.entrel = "scope_relationship"
    and a.entrel <> "scope_relationship"
    and a.arg1 = b.arg1
    and a.t <> b.t
    ')))
}

# Write chia to XSLX.
if(T) {
  require(writexl)
  outfile_xlsx <- paste0(output_dir, 'chia ', exec_timestamp, '.xlsx')
  console('Will write tibble, and free text of criteria, to file \'', outfile_xlsx, '\'.')
  write_xlsx(list('chia' = chia, 'criteria' = bind_rows(criteria_text)), outfile_xlsx)
  console('Completed.')
}

# Analyze chia --------------------------------------------------------------------------------
# Tally annotation types.
if(T) {
  console('Will tally the types of entities.')
  types_tally <-
    chia %>%
    group_by(Type) %>%
    summarise(
      Count = n(),
      Documents = n_distinct(nct, ie),
      Criteria = n_distinct(nct, ie, line))
  console('Completed.')
}

# Tally entities
if(T) {
  console('Will tally the entities.')
  entities_tally <-
    chia %>%
    filter(entrel %in% chia_entities) %>%
    group_by(entrel) %>%
    summarise(
      Count = n(),
      Documents = n_distinct(nct, ie),
      Criteria = n_distinct(nct, ie, line)) %>%
    rename(Entity = entrel)
  console('Completed.')
}

# Tally relationships
if(T) {
  console('Will tally the relationships.')
  relationships_tally <-
    chia %>%
    filter(entrel %in% chia_relationships) %>%
    group_by(entrel) %>%
    summarise(
      Count = n(),
      Documents = n_distinct(nct, ie),
      Criteria = n_distinct(nct, ie, line)) %>%
    arrange(desc(Count)) %>%
    rename(Relationship = entrel)
  console('Completed.')
}

# Tally multi-labeling.
if(T) {
  require(sqldf)
  require(writexl)
  chia_e <- chia[chia$entrel %in% chia_entities,]
  chia_e <- chia_e %>% select(nct, ie, t, line, entrel, text, coords, coord.start, coord.end)
  chia_multi <- as_tibble(sqldf('
    select a.nct as nct, a.ie as ie, a.line as line,
      a.t as "a.t", a.entrel as "a.entrel", a.text as "a.text", a.coords as "a.coords",
        a."coord.start" as "a.coord.start", a."coord.end" as "a.coord.end",
      b.t as "b.t", b.entrel as "b.entrel", b.text as "b.text", b.coords as "b.coords",
        b."coord.start" as "b.coord.start", b."coord.end" as "b.coord.end"
    from chia_e as a inner join chia_e as b
    on a.nct = b.nct
      and a.ie = b.ie
      and a.line = b.line
      and a.t <> b.t
      and max(a."coord.start", b."coord.start") < min(a."coord.end", b."coord.end")
    '))
  remove(chia_e)
  
  out_xlsx <- paste0(output_dir, 'chia_multi.xlsx')
  console('Will write tally of multi-labeling to ', out_xlsx, '.')
  write_xlsx(list('chia_multi'=chia_multi), out_xlsx)
  console('Completed.')
}

# Tally entity-entity pairs.
if(T) {
  ent2_pairs <- subset(chia, Type == 'Relationship',
    select = c(nct, ie, line, entrel, arg1, arg2))
  ents <- subset(chia, Type != 'Relationship', select = c(nct, ie, t, entrel))
  ent2_pairs <- left_join(ent2_pairs, ents, c('nct'='nct', 'ie'='ie', 'arg1'='t'),
    suffix = c('', '.arg1'))
  ent2_pairs <- left_join(ent2_pairs, ents,
    c('nct'='nct', 'ie'='ie', 'arg2'='t'), suffix = c('', '.arg2'))
  remove(ents)
  
  # Reorder horizontal relationships so the order doesn't matter.
  selected_rows <- with(ent2_pairs, entrel %in% chia_h_rel & entrel.arg1 > entrel.arg2)
  ent2_pairs[selected_rows,] <- ent2_pairs[selected_rows,] %>%
    mutate(
      new.arg1 = arg2, new.arg2 = arg1,
      new.entrel.arg1 = entrel.arg2, new.entrel.arg2 = entrel.arg1,
      arg1 = new.arg1, arg2 = new.arg2,
      entrel.arg1 = new.entrel.arg1, entrel.arg2 = new.entrel.arg2) %>%
    select(-new.arg1, -new.arg2, -new.entrel.arg1, -new.entrel.arg2)
  remove(selected_rows)
  
  ent2_pairs <- ent2_pairs %>%
    group_by(entrel, entrel.arg1, entrel.arg2) %>%
    summarise(Count = length(nct),
      Trials = n_distinct(nct),
      Documents = n_distinct(nct),
      Criteria = n_distinct(nct, ie, line)) %>%
    arrange(desc(Count), desc(Trials), desc(Documents)) %>%
    rename(`Argument 1` = entrel.arg1, `Argument 2` = entrel.arg2, Relationship = entrel)
}

# Analyze vertical-OR-type relationships
if(T) {
  chia_temp <- chia[1:5000,]
  subsumes_relationships <- as_tibble(sqldf(paste0('
    select a.nct, a.ie, a.line, c.entrel as `c.entrel`, c.text as `c.text`,
      a.entrel as `a.entrel`, a.arg1, a.arg2, b.entrel as `b.entrel`, b.text as `b.text`
    from chia_temp as a
    inner join chia_temp as b
    inner join chia_temp as c
    on a.nct = b.nct and a.ie = b.ie and a.line = b.line
      and a.nct = c.nct and a.ie = c.ie and a.line = c.line
    and b.entrel = "Scope"
    and a.entrel = "subsumes"
    and c.entrel = "Scope"
    and a.arg2 = b.t
    and a.arg1 = c.t
    and a.t <> b.t
    and a.t <> c.t
    ')))
}

# Clinical trial metadata ---------------------------------------------------------------------
# Load metadata
if(T)
  rdsWrap(chia_meta, {
    console('Will fetch trial metadata from XMLs from ClinicalTrials.gov.')
    require(XML)
    require(readr)
    require(dplyr)
    ncts <- unique(substr(list.files(chia_dir, 'NCT\\d+.*'), 1, 11))
    beginProgressReport(length(ncts))
    trial_metadata <- lapply(ncts,
      function(nct) {
        iterateProgress()
        xml_address <- paste0('https://clinicaltrials.gov/ct2/show/', nct, '?displayxml=true')
        xml_element_list <- unlist(xmlToList(xmlParse(read_file(xml_address))))
        tibble(nct=nct, element=names(xml_element_list), value=xml_element_list)
      })
    chia_meta <- bind_rows(trial_metadata)
    chia_meta$nct <- as.factor(chia_meta$nct)
    chia_meta$element <- as.factor(chia_meta$element)
    console('Completed.')
    
    outfile_meta <- paste0(output_dir, 'XML elements.csv')
    console('Will write metadata to file', outfile_meta, '.')
    write_csv(chia_meta, outfile_meta)
    console('Completed.')
    
    chia_meta
  })

# Analyze metadata, produce plots
if(T) {
  # sponsors.collaborator.agency
  md1 <- chia_meta %>%
    filter(element == 'sponsors.collaborator.agency') %>%
    group_by(value) %>%
    summarise(ncts = n_distinct(nct)) %>%
    arrange(desc(ncts))
  
  # location_countries.country
  md2 <- chia_meta %>%
    filter(element == 'location_countries.country') %>%
    group_by(value) %>%
    summarise(ncts = n_distinct(nct)) %>%
    arrange(desc(ncts))
  
  require(tidyr)
  md2.2 <- chia_meta %>%
    filter(element %in% c('location_countries.country', 'sponsors.collaborator.agency')) %>%
    group_by(value) %>%
    summarise(ncts = n_distinct(nct))
    
  # overall_official.affiliation
  md3 <- chia_meta %>%
    filter(element == 'overall_official.affiliation') %>%
    group_by(value) %>%
    summarise(ncts = n_distinct(nct)) %>%
    arrange(desc(ncts))
  
  # location.facility.address.city
  md4 <- chia_meta %>%
    filter(element == 'location.facility.address.city') %>%
    group_by(value) %>%
    summarise(ncts = n_distinct(nct)) %>%
    arrange(desc(ncts))
  
  ## Histograms
  # study_first_submitted
  md5 <- chia_meta %>%
    filter(element == 'study_first_submitted') %>%
      mutate(value = as.Date(value, format = "%B %d, %Y"))
  options(scipen = 999999999)
  hist(md5$value, breaks = 'months', main = 'study_first_submitted', xlab = 'Date', ylab = 'Trials')
}


# Execution completion ------------------------------------------------------------------------
console('Script execution completed at ', curtime(), '.')
suppressWarnings(remove(list = c(setdiff(global_variables_cleanup_after,
  global_variables_cleanup_prev), 'global_variables_cleanup_after')))

#' @title GAP Data
#'
#' @description Read observer data from GAP Oracle database. The GAP data is the data repository for all observer
#'              data for the Quebec and Gulf region. Methods for creating a \sQuote{gap} object which represents
#'              observer data from the GAP database.
#'
#' @param x A \sQuote{gap} object.
#' @param year A scalar value specifying the data year to be returned. The default is the current year.
#' @param username Character string specifying name of the user.
#' @param password Character string specifying user password.
#' @param schema Character string specifying Oracle schema.
#' @param table A scalar or vector of numeric values from 1 to 9 specifying which of the set of 9 observer tables is to be loaded.
#' @param raw A logical value specifying whether to keep the GAP format for each table. The default value is \code{FALSE}.
#' @param print A logical value specifying whether to print messages to the R console while the tables are being loaded.
#' @param useRfile A logical value specifying whether to load the local \code{R} version of the GAP rather than query the GAP database itself.
#' @param gear A character vector of fishing gear codes.
#' @param species.caught A numeric vector of species codes. These correspond to the species which was actually caught.
#' @param target.species A numeric vector of target species codes. These correspond to the species targetted by the fishing activity.
#' @param nafo.division A character vector of NAFO division (e.g. \sQuote{4T}) codes.
#' @param nafo.area A character vector of NAFO area (e.g. \sQuote{4Tf}) codes.
#' @param trip.number A character vector of trip numbers (e.g. \sQuote{Q14501}).
#' @param expedition.number A character vector of expedition numbers (e.g. \sQuote{Q14501A01}).
#' @param activity.number A character vector of activity numbers (e.g. \sQuote{Q14501A01025}).
#' @param sentinel A logical value specifying whether to include ONLY sentinel catches or to exclude sentinel
#'                 catches from the output result. This only includes southern Gulf sentinel surveys. See the
#'                 Examples section and the \code{\link[gulf]{is.sentinel}} function for further details.
#' @param by A character vector specifying the variables by which to group the analysis.
#' @param scale A logical value specifying whether to scale the observed frequencies by the sampling ratio. The default is \code{TRUE}.
#' @param sort A logical value specifying whether the result is to be sorted using the variables in the \code{by} argument. The default is \code{TRUE}.
#' @param ... Further arguments by which to subset the \sQuote{gap} object. This may be any variable in any GAP table.
#'

#' @examples
#' # Extract yellowtail data for 2013:
#' x <- read.gap(year = 2013, species = 42)
#'
#' # Calculate length-frequecy table by sex and scale for sampling ratio:
#' f <- freq(x, by = "sex", scale = TRUE)
#'
#' # Display results:
#' layout(matrix(1:2))
#' dbarplot(f[1, 2:dim(f)[2]], main = "Females")
#' dbarplot(f[2, 2:dim(f)[2]], main = "Males")
#'
#' # Isolate yellowtail and winter flounder data from 4T in 2015:
#' x <- read.gap(year = 2015)
#' x <- subset(x, species = c(10, 42, 43), nafo.area = "4T")
#' print(x)  # Display object:
#'
#' # Target particular sample of expedition or activity:
#' y <- subset(x, species = 10, expedition.number = "Q15047K01")
#' y <- subset(x, species = 10, activity.number = "Q15047K01013")
#'
#' # Extract American plaice sentinel observer data for 2014 for NAFO 4T:
#' x <- read.gap(year = 2014)
#' y <- subset(x, sentinel = TRUE, nafo.area = "4T", species = 40)
#' y <- subset(x, sentinel = FALSE, nafo.area = "4T", species = 40)
#' y <- subset(y, sentinel = TRUE, gulf = FALSE, quebec = TRUE)
#'
#' # Isolate Quebec region observer catches.
#' y <- subset(x, sentinel = TRUE, gulf = TRUE, quebec = FALSE, mobile = TRUE, fixed = FALSE) # Only sGSL mobile.
#'
#' # Extract yellowtail data for 2013:
#' x <- read.gap(year = 2013, species = 42)
#'
#' # Calculate length-frequecy table by sex and scale for sampling ratio:
#' f <- freq(x, by = "sex", scale = TRUE)
#'
#' # Display results:
#' layout(matrix(1:2))
#' dbarplot(f[1, 2:dim(f)[2]], main = "Females")
#' dbarplot(f[2, 2:dim(f)[2]], main = "Males")
#'
#' # Convert to user-friendly format:
#' y <- convert(x)
#'
#' # Fetch 2013 data:
#' update.gap(2013)
#'
#' # Store 2013 data in working directory:
#' update.gap(2013, path = getwd())
#'
#' # Update for multiple years:
#' update.gap(2008:2013)
#'
#' @section GAP Tables
#'   Table 1 is a fishing activity comments table.
#'   Table 2 is a fishing activity table, indexed by an activity number.
#'   Table 3 is a total catch for each fishing activity, indexed by an expedition number.
#'   Table 4 is an observer table.
#'   Table 5 is a catch and length-frequency index table.
#'   Table 6 is a fishing activity catch table.
#'   Table 7 is a sample description table
#'   Table 8 is an otolith sample number table.
#'   Table 9 is a length-frequency table.
#'
#' @section GAP table index keys
#'   Table 1 : No key.
#'   Table 2 : 'activity number'
#'   Table 3 : 'expedition.number'
#'   Table 4 : 'trip.number'
#'   Table 5 : 'lf.number'
#'   Table 6 : 'catch.number'
#'   Table 7 : 'lf.number'
#'   Table 8 : 'lf.number.long'
#'   Table 9 : 'lf.number.long'

#' @return A \sQuote{gap} object, a list with nine elements, each a data frame containing observer data at various sampling levels.

#' @export
gap <- function(x, ...) UseMethod("gap")

#' @describeIn gap Create a 'gap' object.
#' @export
gap.default <- function(x, ...){
   if (!("gap" %in% class(x))) class(x) <- c("gap", class(x))
   return(x)
}

#' @describeIn gap Read tables from GAP Oracle database.
#' @export read.gap
read.gap <- function(year, username = .gap.oracle$generic.user, password = .gap.oracle$generic.pass,
                     schema = .gap.oracle$generic.schema, raw = FALSE, print = TRUE, useRfile = TRUE, ...){

   # Set 'useRfile' to FALSE if oracle arguments are used:
   if (!is.null(username) | !is.null(password) | !is.null(schema) | raw == TRUE) useRfile <- FALSE

   # Load local files:
   if (useRfile & file.exists(paste0(.gulf.path$obs, year, "/", "GAP tables ", year, ".Rdata"))){
      load(paste0(.gulf.path$obs, year, "/", "GAP tables ", year, ".Rdata"))
      x <- subset(x, ...)
      return(x)
   }

   # Define default parameter values:
   if (missing(year)) year <- as.numeric(substr(Sys.Date(), 1, 4))
   if (print) cat(paste("Reading data for", year, "\n"))

   if (year %in% c(1987:1995)){
      cat(paste0("Reading local SAS files for year: ", year, "\n"))
      x <- read.gap.sas(year = year)
   }else{
      # Open Oracle channel:
      channel <- oracle.open(database = .gap.oracle$prod.bd, username = username, password = password, schema = schema)

      # Initialize variable to be returned:
      x <- list()

      # Define year query string:
      year.query.str = ""
      if (!is.null(year)) year.query.str <- paste0("substr(act.no_voy, 2, 2) = '", substr(as.character(year), 3, 4), "'")

      # Read fishing activity table comments:
      query <- NULL
      query[1] <- paste("select no_voy                    as no_voy_obsr,
                               no_affec                  as no_affec,
                               no_sorti                  as no_sortie,
                               lpad(act.no_trait,3,'0')  as no_acti,
                               remarque                  as remarque
                        from   prob_rem_poi  act \n   ",
                        ifelse(year.query.str != "", paste("where  ", year.query.str, " \n   "), ""),
                        "order by no_voy,
                                  no_affec,
                                  no_sorti,
                                  no_trait;", sep = "")
      query[1] = gsub("\n", "", query[1])
      if (print) cat("  Reading fishing activity comment table (")
      x[[1]] <- oracle.query(channel = channel, query = query[1])
      if (!raw){
         names(x[[1]]) <- tolower(names(x[[1]]))
         x[[1]]$expedition.number <- paste(x[[1]]$no_voy_obsr, x[[1]]$no_affec,
                                           convert.vector(x[[1]]$no_sortie, to = "A2", fill = "0"),
                                           sep = "")
         x[[1]]$activity.number   <- paste(x[[1]]$no_voy_obsr, x[[1]]$no_affec,
                                           convert.vector(x[[1]]$no_sortie, to = "A2", fill = "0"),
                                           convert.vector(x[[1]]$no_acti,   to = "A3", fill = "0"),
                                           sep = "")
         x[[1]]$comment <- paste(x[[1]]$remarque , sep = "")
         x[[1]] <- x[[1]][c("expedition.number", "activity.number", "comment")]
      }
      if (print) cat(paste(nrow(x[[1]]), "records). \n"))

      # Activity table:
      query[2] <- paste("
            select     act.no_voy     as no_voy_obsr,
                       act.no_affec   as no_affec,
                       act.no_sorti   as no_sortie,
                       lpad(act.no_trait,3,'0')  as no_acti,
                       upper(typ_acti)           as cod_typ_acti,
                       rpad(upper(engin),4,' ')  as cod_engin_prob,
                       to_char(date_deb,'yyyymmddhh24mi')  as dat_deb_acti_pech,
                       to_char(date_fin,'yyyymmddhh24mi')  as dat_fin_acti_pech,
                       decode(dur_acth,null ,'     ',lpad(dur_act * 60,5,'0'))   as val_dur_acti,
                       decode(dur_virh,null ,'   ',lpad(dur_vir  * 60,3,'0'))   as val_dur_vir,
                       decode(div_nafo  ,null ,'    ' ,rpad(substr(div_nafo,1,4),4,' ')) as cod_zone_pech_div_nafo,
                       decode(lat_deb,null,'  ',lat_deb)    as val_lati_deb,
                       decode(lon_deb,null,'  ',lon_deb)    as val_longi_deb,
                       decode(cod_bris,null ,' ',cod_bris)    as cod_bris_filet,
                       decode(vitesse,null ,'  ',lpad(trunc(vitesse * 10),2,'0')) as val_vit_toua,
                       decode(prof,null ,'    ',lpad(prof,4,' ')) as val_profd_moy,
                       decode(s_info,null ,' ',s_info) as cod_sourc_infor,
                       decode(t_eau,null,'   ',lpad(to_char(t_eau*10) ,3,' '))  as temp_eau,
                       decode(esp_rech,null ,'    ',lpad(to_char(esp_rech),4,' '))  as cod_esp_prob,
                       decode(desc_eng,null ,lpad(' ',26,' ') ,rpad(desc_eng,26,' '))as desc_engin_util,
                       decode(mail_cul,null,'   ',lpad(mail_cul,4,' ')) as val_grand_maill_cul,
                       decode(mail_ral,null,'  ',lpad(mail_ral,4,' ')) as val_grand_maill_ral,
                       decode(typ_maic,null ,' ',typ_maic) as cod_typ_maill_cul,
                       decode(typ_mair,null ,' ',typ_mair) as cod_typ_maill_ral,
                       decode(rouleau,null,'    ',rouleau) as val_grand_roul,
                       decode(tab_prot,null ,' ',tab_prot) as cod_tabl_protec,
                       decode(nb_fil,null ,'   ',lpad(nb_fil,3,' '))  as nb_filet_maill_leve,
                       decode(long_fil,null ,'  ',long_fil) as long_moy_filet,
                       decode(nb_hamc,null ,'     ',lpad(nb_hamc,5,' ')) as nb_hamc,
                       decode(date_sai,null ,'        ',to_char(date_sai,'yyyymmdd')) as dat_sais_acti_pech_pfcn,
                       decode(nb_fen,null ,' ',nb_fen) as nb_fent,
                       decode(zone,null ,'    ',lpad(rtrim(ltrim(substr(zone,1,4))),4,' '))  as cod_zone_pech,
                       decode(long_fun,null ,'    ',long_fun) as long_fune,
                       decode(mail_fil,null ,'    ',mail_fil) as val_grand_maill_filet,
                       decode(lat_fin,null ,'      ',lat_fin) as val_lati_fin,
                       decode(lon_fin,null ,'      ',lon_fin) as val_longi_fin,
                       decode(tail_ham,null ,'  ',lpad(tail_ham,2,' '))as tail_hamc,
                       decode(frm_ham,null ,' ',frm_ham) as cod_forme_hamcn,
                       decode(cord_dos,null ,'   ',cord_dos) as long_cord_dos,
                       decode(faux_bou,null ,'   ',faux_bou) as long_faux_bourr,
                       decode(esp_gril,null ,'  ',esp_gril) as val_espac_entre_barr,
                       decode(strate,null ,'   ',lpad(strate,3,' ')) as strate,
                       decode(unit_cha,null ,'   ',lpad(unit_cha,3,' ')) as unit_chalut,
                       decode(meth_pos,null ,' ',lpad(meth_pos,1,' ')) as cod_meth_pos,
                       decode(resultat,null ,' ',lpad(resultat,1,' '))as cod_result_oper_pech,
                       decode(retentio,null ,' ',lpad(retentio,1,' ')) as cod_typ_cabl_reten,
                       decode(typ_maif,null ,' ',lpad(typ_maif,1,' ')) as cod_typ_maill_filet,
                       decode(typtrait,null ,' ',lpad(typtrait,1,' ')) as cod_typ_trait,
                       decode(dur_acth,null ,'    ',lpad(dur_acth*10,4,' ')) as val_dur_acti_hdi_annex_1,
                       decode(dur_virh,null ,'  ',lpad(dur_virh*10,2,'0')) as val_dur_vir_hdi_annex_1
             from        prob_acti_poi    act
             where       act.typ_acti in ('PF','SQ','SG','SL') AND ", year.query.str, "\n ",
                        "order by    act.no_voy,
                         act.no_affec,
                         act.no_sorti,
                         act.no_trait;", sep = "")
      query[2] = gsub("\n", "", query[2])
      if (print) cat("  Reading fishing activity table (")
      x[[2]] <- oracle.query(channel = channel, query = query[2])
      if (!raw){
         # Convert names to lower case:
         names(x[[2]]) <- tolower(names(x[[2]]))

         # Define expedition and activity numbers:
         x[[2]]$expedition.number <- paste(x[[2]]$no_voy_obsr, x[[2]]$no_affec,
                                           convert.vector(x[[2]]$no_sortie, to = "A2", fill = "0"),
                                           sep = "")
         x[[2]]$activity.number   <- paste(x[[2]]$no_voy_obsr, x[[2]]$no_affec,
                                           convert.vector(x[[2]]$no_sortie, to = "A2", fill = "0"),
                                           convert.vector(x[[2]]$no_acti, to = "A3", fill = "0"),
                                           sep = "")

         # Extract subset of data:
         x[[2]] <- x[[2]][c("expedition.number", "activity.number",
                            "val_longi_deb", "val_lati_deb", "val_longi_fin", "val_lati_fin",
                            "dat_deb_acti_pech", "dat_fin_acti_pech", "dat_sais_acti_pech_pfcn",
                            "cod_zone_pech_div_nafo",
                            "cod_typ_acti", "cod_engin_prob", "desc_engin_util",
                            "cod_esp_prob", "val_profd_moy")]

         # Rename columns:
         names(x[[2]]) <- c("expedition.number", "activity.number",
                            "longitude.start", "latitude.start", "longitude.end", "latitude.end",
                            "start.date", "end.date", "entry.date",
                            "nafo.subdivision",
                            "activity.type", "gear", "gear.description",
                            "target.species", "depth")

         # Remove spaces in gear field:
         x[[2]]$gear <- gsub(" ", "", x[[2]]$gear)
         x[[2]]$nafo.subdivision <- gsub(" ", "", x[[2]]$nafo.subdivision)
         x[[2]]$nafo.division    <- toupper(substr(x[[2]]$nafo.subdivision, 1, ifelse(nchar(x[[2]]$nafo.subdivision) == 0, 1, nchar(x[[2]]$nafo.subdivision)-1))                                  )
         x[[2]]$longitude.start  <- -abs(as.numeric(x[[2]]$longitude.start))
         x[[2]]$latitude.start   <- as.numeric(x[[2]]$latitude.start)
         x[[2]]$longitude.end    <- -abs(as.numeric(x[[2]]$longitude.end))
         x[[2]]$latitude.end     <- as.numeric(x[[2]]$latitude.end)
      }
      if (print) cat(paste(dim(x[[2]])[1], "records). \n"))

      # Catch table:
      query[3] <- paste("
      select    sem.no_voy   as no_voy,
                sem.no_affec as no_affec,
                sem.no_sorti as no_sortie,
                rpad(ltrim(rtrim(aff.bpc)),10,' ') as no_bpc_prob,
                decode(sem.communau ,null     ,'     '   ,sem.communau) as cod_comm,
                decode(sem.pdv_deb  ,null     ,'     '   ,sem.pdv_deb) as pdv_deb,
                decode(sem.no_log   ,null     ,'       ' ,rpad(ltrim(rtrim(sem.no_log)),7,' ')) as no_log,
                decode(sem.d_port   ,null     ,decode(sem.communau  ,null   ,lpad(' ',30,' ')
                                              ,rpad(ltrim(rtrim(comm.df_comm)),30,' '))
                                              ,rpad(ltrim(rtrim(d_port)),30,' ')) as d_port,
                decode(sem.no_relev           ,null     ,'     '   ,lpad(no_relev,5,' ')) as no_relev
      from      prob_sortie          sem,
                prob_affect          aff,
      (select   sem.no_voy||sem.no_affec||sem.no_sorti cle
                 from      prob_sortie                   sem,
                           prob_affect                   aff,
                           prob_acti_poi                 act
                 where     sem.no_voy                  = act.no_voy      and
                           sem.no_affec                = act.no_affec         and
                           sem.no_sorti                = act.no_sorti        and
                           act.typ_acti                in ('PF','SQ','SG','SL') and
                           aff.no_voy                  = act.no_voy      and
                           aff.no_affec                = act.no_affec
                 group by  sem.no_voy||sem.no_affec||sem.no_sorti
               UNION
               select    sem.no_voy||sem.no_affec||sem.no_sorti cle
               from      prob_sortie                   sem,
                         prob_affect                   aff,
                         prob_acti_cas                 act
               where     sem.no_voy                  = act.no_voy      and
                         sem.no_affec                = act.no_affec    and
                         sem.no_sorti                = act.no_sorti    and
                         aff.no_voy                  = act.no_voy      and
                         aff.no_affec                = act.no_affec
               group by  sem.no_voy||sem.no_affec||sem.no_sorti
               UNION
               select    sem.no_voy||sem.no_affec||sem.no_sorti cle
               from      prob_sortie                   sem,
                         prob_affect                   aff,
                         prob_acti_dra                 act
               where     sem.no_voy                  = act.no_voy         and
                         sem.no_affec                = act.no_affec       and
                         sem.no_sorti                = act.no_sorti       and
                         aff.no_voy                  = act.no_voy         and
                         aff.no_affec                = act.no_affec
               group by  sem.no_voy||sem.no_affec||sem.no_sorti) sel_sortie,
                        prob_communau                                  comm
         where   sel_sortie.cle = sem.no_voy||sem.no_affec||sem.no_sorti and
            aff.no_voy||aff.no_affec = sem.no_voy||sem.no_affec and
            sem.communau      = comm.communau(+)  and \n", paste("substr(sem.no_voy, 2, 2) = '", substr(as.character(year), 3, 4), "'", sep = ""), " and \n ",
                        "2001 = comm.an_comm(+)
     order by cle ;", sep = "")

      query = gsub("\n", "", query)
      if (print) cat("  Reading catch table (")

      x[[3]] <- oracle.query(channel = channel, query = query[3])
      if (!raw){
         names(x[[3]]) <- tolower(names(x[[3]]))
         x[[3]]$trip.number    <- x[[3]]$no_voy
         x[[3]]$expedition.number <- paste0(x[[3]]$no_voy, x[[3]]$no_affec,
                                            convert.vector(x[[3]]$no_sortie, to = "A2", fill = "0"))
         x[[3]]$cfvn           <- x[[3]]$no_bpc_prob
         x[[3]]$landed.port    <- x[[3]]$cod_comm
         x[[3]]$weight.landed  <- x[[3]]$pdv_deb
         x[[3]]$logbook.number <- x[[3]]$no_log
         x[[3]]$home.port      <- x[[3]]$d_port

         x[[3]] <- x[[3]][c("trip.number", "expedition.number", "cfvn", "landed.port", "weight.landed", "logbook.number", "home.port", "no_relev")]
      }
      if (print) cat(paste(dim(x[[3]])[1], "records). \n"))

      # Observer table:
      query[4] <- paste("
           select     rpad(voy.nom_obs,25,' ')     as nom_obs,
                      voy.no_voy                   as no_voy_pech,
                      decode(org.nom_org   ,null       ,lpad(' ',40,' ')    ,rpad(ltrim(rtrim(org.nom_org)),40,' ')),
                      decode(voy.orga_ext  ,null       ,'   '               ,rpad(ltrim(rtrim(org.orga_ext)),3 ,'   ')),
                      annee
           from       (select    distinct(no_voy)
           from       (select    distinct(act.no_voy)
                       from      prob_acti_poi        act,
                                 prob_affect          aff /*,
                                 parametre_extraction prm*/
                       where     typ_acti <> 'CN'
                                 and aff.no_voy       = act.no_voy
                                 and aff.no_affec     = act.no_affec)
                       UNION
                       select    distinct(act.no_voy)
                       from      prob_acti_cas        act,
                                 prob_affect          aff
                       where     aff.no_voy           = act.no_voy
                                 and aff.no_affec     = act.no_affec
                       UNION
                       select    distinct(act.no_voy)
                       from      prob_acti_dra        act,
                                 prob_affect          aff
                       where     aff.no_voy           = act.no_voy
                                 and aff.no_affec     = act.no_affec    )  sel_voy,
                       prob_voy        voy,
                       prob_org_ext    org
                       where     voy.orga_ext         = org.orga_ext
                       and sel_voy.no_voy = voy.no_voy
           order by  voy.orga_ext,
                     voy.no_voy;", sep = "")

      query[4] <- gsub("\n", "", query[4])
      if (print) cat("  Reading observer table (")
      x[[4]] <- oracle.query(channel = channel, query = query[4])
      if (!raw){
         names(x[[4]]) <- c("observer", "trip.number", "company", "voy.orga_ext", "year")
         x[[4]]$company <- gsub(" ", "", x[[4]]$company)
         x[[4]] <- x[[4]][x[[4]]$year == year, ]
      }
      if (print) cat(paste(dim(x[[4]])[1], "records). \n"))

      # Catch and length-frequency index table
      query[5] <- paste("
               select  lpad(cfl.no_capt,9,' ')       as no_capt,
                       lpad(cfl.no_fl,6,' ')         as no_fl,
                       to_char(act.date_deb, 'yyyy') as date_deb
               from    prob_acti_poi         act,
                       prob_affect           aff,
                       prob_capt_poi         cap,
                       prob_capt_fl          cfl
               where   act.no_voy          = cap.no_voy
                       and act.no_affec    = cap.no_affec
                       and act.no_sorti    = cap.no_sorti
                       and to_char(act.date_deb,'yyyy') = ", year,
                        "and act.no_trait    = cap.no_trait
                       and cap.no_capt     = cfl.no_capt
                       and act.typ_acti    in ('PF','SQ','SG','SL')
                       and aff.no_voy      = act.no_voy
                       and aff.no_affec    = act.no_affec
               order by cfl.no_capt,
                        cfl.no_fl;", sep = "")

      query[5] <- gsub("\n", "", query[5])
      if (print) cat("  Reading catch and length-frequency index table (")
      x[[5]] <- oracle.query(channel = channel, query = query[5])
      if (!raw){
         names(x[[5]]) <- c("catch.number", "lf.number", "year")
      }
      if (print) cat(paste(dim(x[[5]])[1], "records). \n"))

      # Activity catch table:
      query[6] <- paste("
                    select       cap.no_voy                   as no_voy_obsr ,
                                 cap.no_affec                 as no_affec,
                                 lpad(cap.no_sorti,2,'0')     as no_sortie,
                                 lpad(cap.no_trait,3,'0')     as no_acti,
                                 lpad(ltrim(rtrim(cap.no_capt)),9,' ') as id_capt_pfcn,
                                 lpad(cap.cod_esp,4,' ')      as cod_esp_prob,
  			                         to_char(act.date_deb,'yyyy') as dat_deb_acti_pech_pfcn,
                                 cap.pdv_cons                 as pds_vif_estim_cons,
                                 cap.pdv_rej                  as pds_vif_estim_rej,
                                 cap.pdv_capt ,
                                 decode(cap.nb_capt      ,null  ,'    '      ,lpad(cap.nb_capt,4,' ')),
                                 decode(cap.capt_old     ,null  ,'         ' ,lpad(cap.capt_old,9,' '))
                    from         prob_acti_poi                  act,
                                 prob_affect                    aff,
                                 prob_capt_poi                  cap
                    where        act.no_voy                   = cap.no_voy
                             and act.no_affec                 = cap.no_affec
                             and act.no_sorti                 = cap.no_sorti
                             and act.no_trait                 = cap.no_trait
                             and typ_acti                     in ('PF','SG','SQ','SL')
                             and aff.no_voy                   = act.no_voy
                             and aff.no_affec                 = act.no_affec
                           and to_char(act.date_deb,'yyyy') = ", year,
                        " order by     cap.no_voy,
                                 cap.no_affec,
                                 cap.no_sorti,
                                 cap.no_trait,
                                 cap.cod_esp ;", sep = "")

      query[6] <-  gsub("\n", "", query[6])
      if (print) cat("  Reading activity catch table (")
      x[[6]]   <- oracle.query(channel = channel, query = query[6])
      if (!raw){
         names(x[[6]]) <- tolower(names(x[[6]]))
         # Define expedition and activity numbers:
         x[[6]]$expedition.number <- paste(x[[6]]$no_voy_obsr, x[[6]]$no_affec,
                                           convert.vector(x[[6]]$no_sortie, to = "A2", fill = "0"),
                                           sep = "")
         x[[6]]$activity.number   <- paste(x[[6]]$no_voy_obsr, x[[6]]$no_affec,
                                           convert.vector(x[[6]]$no_sortie, to = "A2", fill = "0"),
                                           convert.vector(x[[6]]$no_acti, to = "A3", fill = "0"),
                                           sep = "")
         x[[6]] <- x[[6]][, c("expedition.number", "activity.number",  "id_capt_pfcn",  "cod_esp_prob",
                              "dat_deb_acti_pech_pfcn", "pds_vif_estim_cons", "pds_vif_estim_rej",
                              "pdv_capt", "decode(cap.nb_capt,null,'',lpad(cap.nb_capt,4,''))")]
         names(x[[6]]) <-  c("expedition.number", "activity.number",  "catch.number",  "species",
                             "year", "weight.kept", "weight.discard",
                             "weight.caught", "number.caught")
         index <- is.na(x[[6]]$weight.caught)
         x[[6]]$weight.caught[index] <- x[[6]]$weight.kept[index] + x[[6]]$weight.discard[index]

         # Correct gulf sentinel entries:
         index <- is.na(x[[6]]$weight.caught) & (substr(x[[6]]$expedition.number, 4, 4) == "4") & !is.na(x[[6]]$weight.kept)
         x[[6]]$weight.caught[index] <- x[[6]]$weight.kept[index]
         index <- is.na(x[[6]]$weight.caught) & (substr(x[[6]]$expedition.number, 4, 4) == "4") & !is.na(x[[6]]$weight.discard)
         x[[6]]$weight.caught[index] <- x[[6]]$weight.discard[index]
      }
      if (print) cat(paste(dim(x[[6]])[1], "records). \n"))

      # Sample table:
      query[7] <- paste("
                       select lpad(fl.no_fl,6,' ')  as no_fl,
                              decode(typ_exp,null   ,'  ',lpad(typ_exp,2,' ')) as cod_typ_exper,
                              decode(etat_deb,null   ,' ',lpad(etat_deb,1,' ')) as cod_etat_echan,
                              decode(pds_ech ,null   ,'     ',lpad(pds_ech,5,' ')) as pds_echan,
                              decode(cat_tail,null   ,' ',decode(least(10,cat_tail),10   ,'*'
                                 ,lpad(cat_tail,1,' '))) as cod_categ_tail,
                              decode(nb_cat,null   ,'  '      ,lpad(nb_cat,2,' ')) as nb_cat,
                              decode(pds_cat,null   ,'     '   ,lpad(pds_cat,5,' ')) as pds_cat,
                              decode(dat_sai,null   ,'        ',to_char(dat_sai,'yyyymmdd')) as dat_sais,
  			                      to_char(act.date_deb,'yyyy') as dat_deb_acti_pech_pfcn
                       from   prob_affect                 aff,
                              prob_acti_poi               act,
                              prob_capt_poi               cap,
                              prob_capt_fl                cfl,
                              prob_pois_fl                fl
                       where  act.no_voy                   = cap.no_voy
                          and act.no_affec                 = cap.no_affec
                          and act.no_sorti                 = cap.no_sorti
                          and to_char(act.date_deb,'yyyy') = ", year,
                        "and act.no_trait                 = cap.no_trait
                          and cap.no_capt                  = cfl.no_capt
                          and cfl.no_fl                    = fl.no_fl
                          and typ_acti                     in ('PF','SG','SQ','SL')
                          and aff.no_voy                   = act.no_voy
                          and aff.no_affec                 = act.no_affec
                       order by      fl.no_fl;", sep = "")

      query[7] <- gsub("\n", "", query[7])
      if (print) cat("  Reading sample table (")
      x[[7]] <- oracle.query(channel = channel, query = query[[7]])
      if (!raw){
         names(x[[7]]) <- tolower(names(x[[7]]))
         names(x[[7]]) <- c("lf.number", "experiment.type", "landed.form", "weight.sampled",
                            "tail.category", "category.number", "category.weight", "entry.date",
                            "year")
         x[[7]] <- x[[7]][c("lf.number", "experiment.type", "landed.form", "tail.category",
                            "category.number", "category.weight", "weight.sampled",
                            "entry.date", "year")]
      }
      if (print) cat(paste(dim(x[[7]])[1], "records). \n"))

      # Otolith table:
      query[8] <- paste("
                      select lpad(oto.no_det,9,' ')            as id_det_freq_long,
                             lpad(oto.no_oto,2,' ')            as  no_oto,
                             to_char(act.date_deb,'yyyy')      as dat_deb_acti_pech_pfcn
                      from   prob_affect           aff,
                             prob_acti_poi         act,
                             prob_capt_poi         cap,
                             prob_capt_fl          cfl,
                             prob_pois_fl          fl,
                             prob_grp_flpo         sgr,
                             prob_poi_oto          det,
                             prob_otolithe         oto
                      where  act.no_voy      = cap.no_voy
                         and act.no_affec    = cap.no_affec
                         and act.no_sorti    = cap.no_sorti
                         and to_char(act.date_deb,'yyyy') = ", year,
                        "and act.no_trait    = cap.no_trait
                         and cap.no_capt     = cfl.no_capt
                         and cfl.no_fl       = fl.no_fl
                         and fl.no_fl        = sgr.no_fl
                         and sgr.no_grfl     = det.no_grfl
                         and det.no_det      = oto.no_det
                         and typ_acti       in ('PF','SG','SQ','SL')
                         and aff.no_voy      = act.no_voy
                         and aff.no_affec    = act.no_affec
                      order by  det.no_det,
                                no_oto;", sep = "")

      query[8] <- gsub("\n", "", query[8])
      if (print) cat("  Reading otolith table (")
      x[[8]] <- oracle.query(channel = channel, query = query[[8]])
      if (!raw){
         names(x[[8]]) <- tolower(names(x[[8]]))
         names(x[[8]]) <- c("lf.number.long", "no_oto", "year")
      }
      if (print) cat(paste(dim(x[[8]])[1], "records). \n"))

      # Length frequency table:
      query[9] <- paste("
                       select lpad(fl.no_fl,6,' ') as no_fl,
                              lpad(det.no_det,9,' ')                                        as id_det_freq_long,
                              decode(sgr.sexe    ,null    ,' '   ,lpad(sgr.sexe,1,' '))     as cod_sexe,
                              decode(sgr.grp_mes ,null    ,' '   ,lpad(sgr.grp_mes,1,' '))  as cod_gr_mesu,
                              decode(sgr.typ_mes ,null    ,' '   ,lpad(sgr.typ_mes,1,' '))  as cod_typ_mesu,
                              det.vlong                                                     as val_long_pfcn,
                              decode(det.nb_pois ,null    ,'   '  ,lpad(det.nb_pois,3,' ')) as nb_indiv_pfcn,
                              decode(det.old_det ,null    ,'     ',lpad(det.old_det,7,' ')) as no_det_freq_long,
                              to_char(act.date_deb,'yyyymmddhh24mi') as dat_deb_acti_pech_pfcn
                       from   prob_affect               aff,
                              prob_acti_poi             act,
                              prob_capt_poi             cap,
                              prob_capt_fl              cfl,
                              prob_pois_fl              fl,
                              prob_grp_flpo             sgr,
                              prob_poi_oto              det
                       where  act.no_voy     = cap.no_voy
                          and act.no_affec   = cap.no_affec
                          and act.no_sorti   = cap.no_sorti
                          and act.no_trait   = cap.no_trait
  		                    and to_char(act.date_deb,'yyyy') = ", year,
                        "and cap.no_capt                 = cfl.no_capt
                          and cfl.no_fl                   = fl.no_fl
                          and fl.no_fl                    = sgr.no_fl
                          and sgr.no_grfl                 = det.no_grfl
                          and typ_acti                    in ('PF','SG','SQ','SL')
                          and aff.no_voy                  = act.no_voy
                          and aff.no_affec                = act.no_affec
                       order by  det.no_grfl,
                                 det.old_det;", sep = "")

      query[9] <- gsub("\n", "", query[9], fixed = TRUE)
      if (print) cat("  Reading length-frequency table (")
      x[[9]] <- oracle.query(channel = channel, query = query[9])
      if (!raw){
         names(x[[9]]) <- tolower(names(x[[9]]))
         names(x[[9]]) <- c("lf.number", "lf.number.long", "sex", "group", "measurement.type",
                            "fish.length", "number.fish", "no_det_freq_long", "start.date")
         x[[9]]$fish.length <- 100 * x[[9]]$fish.length
         x[[9]]$sex <- match(x[[9]]$sex, c("I", "M", "F"))-1
         x[[9]]$sex[x[[9]]$sex == 0] <- 9
      }
      if (print) cat(paste(dim(x[[9]])[1], "records). \n\n"))

      # Close channel:
      oracle.close(channel)
   }

   # Add 'gap' class identifier if 'raw' is FALSE:
   if (!raw) x <- gap(x)

   # Perform 'subset' operation:
   if (!raw & (length(list(...)) > 0)) x <- subset(x, ...)

   return(x)
}

#' @describeIn gap Scale observer length-frequencies by sample to total weights ratios.
#' @export
adjust.gap <- function(x, correct.weight.sampled = TRUE, by, ...){
   # Define commercial length card index key:
   key <- c("species", "catch.number")
   if (!missing(by)) key <- unique(c(by, key))

   # Convert 'sex' to character values:
   if (is.character(x[[9]]$sex)){
      x[[9]]$sex <- match(x[[9]]$sex, c("I", "M", "F"))-1
      x[[9]]$sex[x[[9]]$sex == 0] <- 9
   }

   # Correct 'weight.sampled' field:
   if (correct.weight.sampled){
      f <- freq(x, by = key)
      res <- weight(f, ...)            # year = unique(f$year),
      fvars <- names(res)[grep("^[ 0-9]+$", names(res))]
      f$weight.sampled <- apply(res[fvars], 1, sum) # * 2.20462  in kg
      tab <- convert(x)$catch
      x[[7]]$catch.number <- x[[5]]$catch.number[match(x[[7]]$lf.number, x[[5]]$lf.number)]

      temp <- stats::aggregate(x[[7]]["weight.sampled"], by = x[[7]]["catch.number"], sum, na.rm = TRUE)

      x[[6]]$weight.sampled <- temp$weight.sampled[match(x[[6]]$catch.number, temp$catch.number)]
      index <- which(x[[6]]$weight.sampled == x[[6]]$weight.caught)

      # Calculate estimated sample weight by catch:
      temp <- stats::aggregate(f["weight.sampled"], by = f["catch.number"], sum)

      # Inport into approriate tables:
      x[[6]]$weight.sampled <- temp$weight.sampled[match(x[[6]]$catch.number, temp$catch.number)]
      x[[7]]$weight.sampled <- temp$weight.sampled[match(x[[7]]$catch.number, temp$catch.number)]

      # Correct weight caught if less than sample weight:
      index <- unique(c(index, which(x[[6]]$weight.caught < x[[6]]$weight.sampled)))
      x[[6]]$weight.caught[index] <- x[[6]]$weight.sampled[index]

      # Restore original table variables:
      x[[6]] <- x[[6]][, setdiff(names(x[[6]]), "weight.sampled")]
      x[[7]] <- x[[7]][, setdiff(names(x[[7]]), "catch.number")]

      cat("Observer sample weights were re-estimated from observed length-frequencies.\n")
   }

   return(x)
}

#' @describeIn gap Calculate catch-at-length for a 'gap' object.
#' @rawNamespace S3method(catch.at.length,gap)
catch.at.length.gap <- function(x, by, landings, adjust = TRUE, ...){
   # Parse 'by' argument:
   if (!missing(by)) by.old <- by else by.old <- NULL
   by <- unique(c("year", "species", "expedition.number", "activity.number", by.old))

   # Correct sample weights:
   if (adjust){
      by.adjust <- intersect(by, c("year", "sex"))
      if (length(by.adjust) == 0) x <- adjust(x) else x <- adjust(x, by = by.adjust)
   }

   # Calculate length-frequency:
   f <- freq(x, by = by, scale = TRUE, ...)

   # Convert to weights:
   w <- weight(f, ...)

   # Define frequency variable:
   fvars <- names(w)[grep("^[ 0-9]+$", names(w))]
   w <- stats::aggregate(w[fvars], by = w[by.old], sum)

   return(w)
}

#' @export
convert.gap <- function(x, correct.weight.sampled = TRUE, ...){
   # CONVERT.GAP - Convert 'gap' object to user-friendly format.

   # Complete index keys:
   x[[7]] <- merge(x[[7]], x[[5]], by = "lf.number", names = "catch.number", all.x = TRUE)

   # Merge length-frequency summary table into catch table:
   x[[6]] <- merge(x[[6]], stats::aggregate(x[[7]]["weight.sampled"], by = x[[7]]["catch.number"], mean), all.x = TRUE)
   vars <- c("experiment.type", "landed.form", "tail.category", "category.number", "category.weight")
   x[[6]] <- merge(x[[6]], stats::aggregate(x[[7]][vars], by = x[[7]]["catch.number"], function(x) unique(x)[1]), all.x = TRUE)
   x[[9]] <- merge(x[[9]], x[[5]], by = "lf.number", names = "catch.number", all.x = TRUE)

   # Initialize tables to be returned:
   v <- list()
   v$expedition <- x[[3]]
   v$activity   <- x[[2]]
   v$catch      <- x[[6]]
   v$length     <- x[[9]]

   # Build 'expedition' table:
   v$expedition <- merge(v$expedition, x[[4]], by = "trip.number", all.x = TRUE)
   v$expedition$sentinel <- is.sentinel(v$expedition)

   # Build 'activity' table:
   v$activity$trip.number     <-  substr(v$activity$activity.number, 1, 6)
   v$activity$longitude.start <- -abs(as.numeric(v$activity$longitude.start))
   v$activity$longitude.end   <- -abs(as.numeric(v$activity$longitude.end))
   v$activity$latitude.start  <-  as.numeric(v$activity$latitude.start)
   v$activity$latitude.end    <-  as.numeric(v$activity$latitude.end)
   v$activity$cfvn            <- v$expedition$cfvn[match(v$activity$expedition.number, v$expedition$expedition.number)]
   v$activity$year            <- as.numeric(substr(v$activity$start.date, 1, 4))
   v$activity$month           <- as.numeric(substr(v$activity$start.date, 5, 6))
   v$activity$day             <- as.numeric(substr(v$activity$start.date, 7, 8))
   v$activity$sentinel        <- is.sentinel(v$activity)

   # Build 'catch' table:
   v$catch$trip.number <- substr(v$catch$activity.number, 1, 6)
   v$catch <- merge(v$catch, v$activity, by = "activity.number", names = setdiff(names(v$activity), names(v$catch)), all.x = TRUE)
   index <- !is.na(v$catch$weight.sampled) & !is.na(v$catch$weight.kept) &
      is.na(v$catch$weight.caught) & (v$catch$weight.sampled == v$catch$weight.kept)
   v$catch$weight.caught[index] <- v$catch$weight.kept[index]
   v$catch$cfvn <- gsub(" ", "", v$catch$cfvn)

   # Build 'length' table:
   v$length <- merge(v$length, v$catch, by = "catch.number", names = setdiff(names(v$catch), names(v$length)), all.x = TRUE)
   if (is.character(v$length$sex)){
      v$length$sex <- match(toupper(v$length$sex), c("I", "M", "F"))-1
      v$length$sex[v$length$sex == 0] <- 9
   }

   # Merge variables across tables:
   key <- c("expedition.number", "activity.number", "catch.number")
   for (i in 1:3){
      for (j in (i+1):4){
         vars <- setdiff(names(v[[i]]), names(v[[j]]))
         if (length(vars) > 0) v[[j]] <- merge(v[[j]], v[[i]], by = key[i], names = vars, all.x = TRUE)
      }
   }

   #species <- 40
   #lengths <- v$length$fish.length[v$length$species == species]
   #sex     <- v$length$sex[v$length$species == species]

   # Estimate sample weights from length and frequency:
   #if (correct.weight.sampled){
   #   10  11  12  14  23  30  31  40  41  42  43  50  51  52  60  64  70  71 118 143 209 210 400
   #   v$length$number.fish[v$length$species == 10] * weight(v$length$fish.length[v$length$species == 10], species = 10, sex = v$length$sex[v$length$species == 10])
   #   v$length$number.fish[v$length$species == 11] * weight(v$length$fish.length[v$length$species == 11], species = 11, sex = v$length$sex[v$length$species == 11])
   #   v$length$number.fish[v$length$species == 12] * weight(v$length$fish.length[v$length$species == 12], species = 12, sex = sex12)
   #   v$length$number.fish[v$length$species == 14] * weight(v$length$fish.length[v$length$species == 14], species = 14, sex = v$length$sex[v$length$species == 14])
   #   v$length$number.fish[v$length$species == 23] * weight(v$length$fish.length[v$length$species == 23], species = 23, sex = v$length$sex[v$length$species == 23])
   #   w <- v$length$number.fish * weight(v$length$fish.length, species = v$length$species, sex = v$length$sex)
   #}



   return(v)
}

#' @describeIn gap Return the total observed or sampling-ratio adjusted length frequecies for a \sQuote{gap} object.
#' @export
freq.gap <- function(x, by, scale = FALSE, sort = TRUE, na.action, ...){
   # FREQ.GAP - Length-frequency values for a 'gap' object.

   # Extract subset of GAP data:
   x <- subset(x, ...)

   # Convert 'gap' object to usable format:
   y <- convert(x)

   # Define 'by' argument:
   if (missing(by)) by <- NULL
   by.old <- by
   if (!is.character(by)) by <- names(by)
   by <- union("fish.length", by)
   if (scale) by <- union("catch.number", by)

   # Calculate summary frequencies:
   temp <- stats::aggregate(y$length["number.fish"], by = y$length[by], sum)

   vars <- setdiff(by, "fish.length")
   len <- sort(unique(temp[, "fish.length"]))
   bin <- seq(min(len), max(len), by = min(diff(len)))
   if (length(vars) > 0){
      r <- unique(temp[vars])
      r <- cbind(r, matrix(0, nrow = dim(r)[1], ncol = length(bin)))
      str <- format(bin)
      dimnames(r) <- list(1:nrow(r), c(vars, str))
   }else{
      r <- rep(0, length(bin))
      names(r) <- format(bin)
      r[format(temp$fish.length)] <- temp$number.fish
   }

   # Reformat frequency results to table form:
   if (!is.null(dim(r))){
      for (i in 1:nrow(r)){
         index <- rep(TRUE, dim(temp)[1])
         for (j in 1:length(vars)) index <- index & (temp[vars[j]] == r[i, vars[j]])
         r[i, format(temp$fish.length[index])] <- temp[index, "number.fish"]
      }
   }

   # Scale frequency by sample weight:
   if (scale){
      if (!all(unique(y$length$catch.number) %in% y$catch$catch.number))
         stop("Some length frequencies have no matching catch. Unable to scale length frequency.")

      # Calculate sampling ratio:
      y$catch$ratio <- y$catch$weight.sampled / y$catch$weight.caught
      index <- which(y$catch$ratio > 1)
      y$catch$ratio[index] <- 1  # Weight sampled cannot exceed weight caught.
      y$catch$weight.caught[index] <- y$catch$weight.sampled[index]

      # Scale length-frequency by sampling ratio:
      r$ratio <- y$catch$ratio[match(r$catch.number, y$catch$catch.number)]
      if (!missing(na.action)){
         if (na.action %in% c("rm", "remove")) r <- r[!is.na(r$ratio), ]
      }
      if (any(is.na(r$ratio))) stop("Some catch weights are NA.")

      # Scale by catch sampling ratio:
      r[str] <- r[str] / repvec(r$ratio, ncol = length(str))

      # Collapse to original specification:
      if (is.null(by.old)){
         ratio <- sum(y$catch$weight.caught[!is.na(y$catch$weight.sampled)]) / sum(y$catch$weight.caught) # Sampled versus total catches.
         r <- (1 / ratio) * apply(r[str], 2, sum)
      }else{
         # Correct missing "weight.caught" variable:
         y$catch$weight.discard[is.na(y$catch$weight.discard)] <- 0
         index <- is.na(y$catch$weight.caught)
         y$catch$weight.caught[index] <- y$catch$weight.kept[index] + y$catch$weight.discard[index]

         # Calculate trip sampling ratio:
         index <- y$catch$expedition.number %in% y$catch$expedition.number[y$catch$catch.number %in% r$catch.number]
         a <- stats::aggregate(list(total = y$catch$weight.caught[index]), by = y$catch[index, by.old[by.old %in% names(y$catch)], drop = FALSE], sum, na.rm = TRUE)
         index <- y$catch$catch.number %in% r$catch.number
         b <- stats::aggregate(list(sampled = y$catch$weight.caught[index]), by = y$catch[index, by.old[by.old %in% names(y$catch)], drop = FALSE], sum)
         a <- merge(a, b, all.x = TRUE)
         a$sampled[is.na(a$sampled)] <- 0
         a$ratio <- a$sampled / a$total

         # Add trip sampling ratio to calculated length-frequencies:
         index <- match(r[, by.old[by.old %in% names(y$catch)]], a[, by.old[by.old %in% names(y$catch)]])
         r$ratio <- a$ratio[index]
         r[str] <- r[str] / repvec(r$ratio, ncol = length(str))
         r <- stats::aggregate(r[str], by = r[by.old], sum)
      }
   }

   if (sort) if (length(by.old) > 0) r <- sort(r, by = by.old)

   return(r)
}

#' @describeIn gap Display summary information about a 'gap' object on the R console.
#' @export
print.gap <- function(x){
   # Function to generate properly formatted strings:
   bracket <- function(v){
      if (is.numeric(v)) v <- sort(v)
      if (length(v) > 6){
         n <- length(v)
         if (is.character(v)) v <- paste("'", v[c(1, length(v))], "'", sep = "")
         v <- paste("[", v[1], ",..., ", v[length(v)], "]", "(n = ", n,")", sep = "")
      }else{
         if (is.character(v)) v <- paste("'", v, "'", sep = "")
         if (length(v) > 1) v <- paste("[", paste(v, collapse = ", "), "]", sep = "")
      }
      return(v)
   }

   # Display informations:
   cat("\n'gap' object:\n\n")
   cat(paste("   Year(s)          :", bracket(unique(x[[6]]$year)), "\n"))
   cat(paste("   Sampled Species  :", bracket(unique(x[[6]]$species)), "\n"))
   cat(paste("   Target Species   :", bracket(unique(x[[2]]$target.species)), "\n"))
   cat(paste("   Trips            :", bracket(unique(x[[3]]$trip.number)), "\n"))
   cat(paste("   Expeditions      :", bracket(unique(x[[6]]$expedition.number)), "\n"))
   cat(paste("   Activities       :", bracket(unique(x[[6]]$activity.number)), "\n"))
   cat(paste("   CFVN             :", bracket(sort(as.numeric(unique(x[[3]]$cfvn)))), "\n"))
   cat(paste("   Gear             :", bracket(unique(x[[2]]$gear)), "\n"))
   cat(paste("   NAFO division    :", bracket(unique(x[[2]]$nafo.division)), "\n"))
   cat(paste("   NAFO subdivision :", bracket(unique(x[[2]]$nafo.subdivision)), "\n"))
   cat(paste("   Catches          :", nrow(x[[6]]), "\n"))
   cat(paste("   Samples          :", length(unique(x[[7]]$lf.number)), "\n"))
   cat(paste("   Fish sampled     :", sum(x[[9]]$number.fish), "\n\n"))
}

#' @describeIn gap Return a subset of a 'gap' dataset.
#' @export
subset.gap <- function(x, gear, species.caught, target.species, nafo.division, nafo.subdivision, trip.number, expedition.number, activity.number, sentinel, ...){
   # SUBSET.GAP - Subset of a 'gap' object.

   # Parse 'sentinel' agument:
   if (!missing(sentinel)) sentinel <- match(sentinel, TRUE, FALSE)

   # Save table names:
   names <- lapply(x, names)

   # Table index keys:
   #   Table 1 : No key.
   #   Table 2 : 'activity number'
   #   Table 3 : 'expedition.number'
   #   Table 4 : 'trip.number'
   #   Table 5 : 'lf.number'
   #   Table 6 : 'catch.number'
   #   Table 7 : 'lf.number'
   #   Table 8 : 'lf.number.long'
   #   Table 9 : 'lf.number.long'

   # Merge variables across tables:
   for (i in 1:length(x)) if ("expedition.number" %in % names(x[[i]])) x[[i]]$trip.number <- substr(x[[i]]$expedition.number, 1, 6)

   x[[8]] <- merge(x[[8]], x[[9]], by = "lf.number.long", names = "lf.number", all.x = TRUE)

   x[[7]] <- merge(x[[7]], x[[5]], by = "lf.number", names = "catch.number", all.x = TRUE)
   x[[8]] <- merge(x[[8]], x[[5]], by = "lf.number", names = "catch.number", all.x = TRUE)
   x[[9]] <- merge(x[[9]], x[[5]], by = "lf.number", names = "catch.number", all.x = TRUE)

   x[[5]] <- merge(x[[5]], x[[6]], by = "catch.number", names = c("trip.number", "expedition.number", "activity.number", "species"), all.x = TRUE)
   x[[7]] <- merge(x[[7]], x[[6]], by = "catch.number", names = c("trip.number", "expedition.number", "activity.number", "species"), all.x = TRUE)
   x[[8]] <- merge(x[[8]], x[[6]], by = "catch.number", names = c("trip.number", "expedition.number", "activity.number", "species"), all.x = TRUE)
   x[[9]] <- merge(x[[9]], x[[6]], by = "catch.number", names = c("trip.number", "expedition.number", "activity.number", "species"), all.x = TRUE)

   #
   for (j in 2:3){
      for (i in 5:9){
         vars <- c(setdiff(names(x[[j]]), c(names(x[[i]]), c("longitude.start", "latitude.start", "longitude.end", "latitude.end",
                                                             "start.date", "end.date", "no_relev", "weight.landed", "entry.date",
                                                             "depth", "gear.description", "target.species", "nafo.division", "nafo.subdivision"))))
         x[[i]] <- merge(x[[i]],
                         x[[j]][!duplicated(x[[j]]$expedition.number), ],
                         by = ifelse(j == 2, "activity.number", "expedition.number"),
                         names = vars,
                         all.x = TRUE)
      }
   }

   # Subset 'gap' object by species type:
   for (i in 1:length(x)){
      if (!missing(species.caught))   if ("species" %in% names(x[[i]]))          x[[i]] <- x[[i]][which(x[[i]]$species %in% species.caught), ]
      if (!missing(target.species))   if ("target.species" %in% names(x[[i]]))   x[[i]] <- x[[i]][which(x[[i]]$target.species %in% target.species), ]
      if (!missing(gear))             if ("gear" %in% names(x[[i]]))             x[[i]] <- x[[i]][which(toupper(gsub(" ", "", x[[i]]$gear)) %in% gsub(" ", "", toupper(gear))), ]
      if (!missing(nafo.division))    if ("nafo.division" %in% names(x[[i]]))    x[[i]] <- x[[i]][which(substr(toupper(gsub(" ", "", x[[i]]$nafo.division)), 1, 2) %in% gsub(" ", "", toupper(nafo.division))), ]
      if (!missing(nafo.subdivision)) if ("nafo.subdivision" %in% names(x[[i]])) x[[i]] <- x[[i]][which(toupper(gsub(" ", "", x[[i]]$nafo.subdivision)) %in% gsub(" ", "", toupper(nafo.subdivision))), ]
   }

   # Subset 'gap' object other variables:
   if (length(list(...)) > 0){
      args <- list(...)
      for (i in 1:length(args)){
         for (j in 1:length(x)){
            if (names(args)[i] %in% names(x[[j]])){
               x[[j]] <- x[[j]][which(x[[j]][, names(args)[i]] %in% args[[i]]), ]
            }
         }
      }
   }

   # Subset 'sentinel' data via the expedition number::
   if (!missing(sentinel)){
      if (sentinel)  for (i in 1:length(x)) x[[i]] <- x[[i]][is.sentinel(x[[i]], ...), ]
      if (!sentinel) for (i in 1:length(x)) x[[i]] <- x[[i]][!is.sentinel(x[[i]], ...), ]
   }

   # Define which table by which to subset by table index keys:
   if ("lf.number" %in% names(list(...))) tab <- 9 else tab <- 6

   # Subset by activity, expedition and trip number:
   if (missing(activity.number))   activity.number   <- unique(x[[tab]]$activity.number)
   if (missing(expedition.number)) expedition.number <- unique(x[[tab]]$expedition.number)
   if (missing(trip.number))       trip.number       <- unique(substr(x[[tab]]$expedition.number, 1, 6))
   for (i in 1:length(x)){
      if ("activity.number" %in% names(x[[i]])) x[[i]] <- x[[i]][which(x[[i]]$activity.number %in% activity.number), ]
      if ("expedition.number" %in% names(x[[i]])) x[[i]] <- x[[i]][which(x[[i]]$expedition.number %in% expedition.number), ]
      if ("trip.number" %in% names(x[[i]])) x[[i]] <- x[[i]][which(x[[i]]$trip.number %in% trip.number), ]
   }

   # Restore original format:
   for (i in 1:length(x)) x[[i]] <- x[[i]][names[[i]]]

   return(x)
}

#' @describeIn gap Queries the GAP observer database and updates locally-stored \code{R} versions of the data.
#' @export
update.gap <- function(year, path, ...){
   # UPDATE.GAP - Update GAP data and create R-readable versions.

   # Check input argument:
   if (!is.numeric(year) | (length(year) == 0)) stop("'year' must be a numeric vector.")
   if (any((year %% 1) != 0 )) stop("'year' must be integers.")
   if (any(year < 1996)) stop ("'year' must be 1996 or later.")

   # Loop over years:
   for (i in 1:length(year)){
      if (missing(path)) path <- .gulf.path$obs
      writeable <- Sys.chmod(path = paste0(path, year[i]))
      if (!writeable){
         stop(paste("Unable to update local GAP files: no write access rights on", paste0(path, year[i])))
      }else{
         x <- read.gap(year = year[i], raw = FALSE, useRfile = FALSE, ...)
         save(x, file = paste0(path, year[i], "/", "GAP tables ", year[i], ".Rdata"))
      }
   }
}

#' @describeIn gap Summary of a 'gap' object.
#' @export
summary.gap <- function(x){
   y <- convert(x)

   vars <- c("gear", "month")
   res <- stats::aggregate(list(trips = y$length$expedition.number), by = y$length[vars], function(x) length(unique(x)))
   res <- cbind(res, stats::aggregate(list(samples = y$length$activity.number), by = y$length[vars], function(x) length(unique(x)))["samples"])
   res <- cbind(res, stats::aggregate(list(fish = y$length$number.fish), by = y$length[vars], sum)["fish"])

   print(res)
   invisible(res)
}

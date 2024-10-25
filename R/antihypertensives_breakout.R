#' Antihypertensives breakout
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/antihypertensives_breakout.csv
#' 
#' @details Medication list:
#' anti_platelet: aspirin, clopidogrel, prasugrel, ticagrelor, plavix, effient, brilinta
#' beta_blockers: 
#'            propranolol, inderal, atenolol, tenormin, bisoprolol, zebeta, metoprolol, lopressor, 
#'            nadolol, corgard, nebivolol, bystolic, carvedilol, coreg, metoprolol tartrate, lopressor,
#'            metoprolol succinate, toprol xl, labetalol, trandate, tenoretic, ziac, toprol, tenoretic,
#'            ziac, coreg cr, acebutolol, sectral, pindolol, visken, penbutolol, levatol, betaxolol,
#'            kerlone, celiprolol, selectol, corzide, inderide, timolide
#' ace_inhibitors: 
#'            fosinopril, fosinopril sodium, monopril, ramipril, altace, captopril, capoten, moexipril,
#'            univasc, lisinopril, prinivil, zestril, enalapril, vasotec, epaned, quinapril, accupril,
#'            trandolapril, mavik , gopten, odrik, benazepril, lotensin, perindopril, aceon
#' calcium_channel_blockers:
#'            amlodipine, norvasc, diltiazem, cardizem, tiazac, felodipine, isradipine, nicardipine,
#'            cardenesr, cardene sr, cardene-sr, nifedipine, procardia, nisoldipine, sular, verapamil, 
#'            calan, verelan, covera-hs, covera hs, coverahs, lotrel, caduet, bepridil, vascor, clevidipine, 
#'            cleviprex, isoptin, plendil, lacidipine, caldine, lacimen, lacipil, midotens, motens, lercanidipine, 
#'            lercadip, zanadip, dynacirc, adalat, nifediac, nifedical, nimodipine, nimotop, prestalia, teczem, 
#'            lexxel, carmen ace, coripren, tarka, azor, tribenzor, twynsta, exforge, exforge hct
#' angiotenstive_ii_receptor_antagonists:
#'            candesartan, atacand, eprosartan, teveten, irbesartan, avapro, losartan, cozaar, 
#'            olmesartan, benicar, telmisartan, micardis, valsartan, diovan, hyzaar, diovanhct, 
#'            diovan-hct, diovan hct, entresto, sacubatril-valsartan, azilsartan, edarbi, avalide, 
#'            azor, tribenzor, twynsta, exforge, exforge hct
#' non_loop_diuretics: 
#'            hydrochlorothiazide, hctz, chlorthiazide, diuril, triamterene & hydrochlorothiazide, triamterene and hydrochlorothiazide, 
#'            triamterene / hydrochlorothiazide, maxide, metolazone, zaroxolyn, lotensin hct, lotensin-hct, lotensinhct, capozide, 
#'            vaseretic, prinizide, zestoretic, hyzaar, diovan hct, diovanhct, diovan-hct, tenoretic, ziac, amiloride, midamore, 
#'            diuril, chlorthalidone, hygroton, indapamide, lozol, hydrodiuril, diulo, mykrox, tribenzor, exforge hct, moduretic, 
#'            dyazide, maxzide, combipres, apresazide, aldoril, minizide
#' mineralocorticoid_antagonists: spironolactone, aldactone, eplerenone, inspra
#' statins: atorvastatin, lipitor, torvast, lovastatin, altocor, pravastatin, pravachol, rosuvastatin, crestor, simvastatin, zocor), 
#' cholesterol_absorption_inhibitors: ezetimibe, vytorin
#' loop_diuretics: furosemide, lasix, bumetanide, bumex, ethacrynic acid, edecrin, torsemide, demadex 
#' alpha_blockers: prazosin, doxazosin, terazosin, minipress, cardura, hytrin, carduran, minipress xl
#' alpha_ii_agonists:
#'            clonidine, methyldopa, guanabenz, guanfacine, lofexidine, catapres, dixarit, duraclon, jenloga, kapvay, nexiclon xr, 
#'            wytensin, intuniv, tenex, aldomet, britlofex, aldoril, diupres, hydropres
#' bile_acid_sequestrants: 
#'            cholestyramine, colestopil, colesevelam
#' nicotinic_acids: niacin
#' fibric_acid_derivatives: c(gemfibrozil, lopid, triglide, fibricor, lipofen, tricor, bezafibrate 
#' oral_antihypertensives: 
#'            warfarin, dabigatran, apixaban, rivaroxaban, edoxaban, coumadin, pradaxa, eliquis, xarelto, savaysa)
#' subcutaneous_antihypertensives: dalteparin, enoxaparin
#' nitrates: 
#'            isosorbide mononitrate, isosorbide dinitrate, nitroglycerin, imdur, isordil, bidil, nitro-bid, nitro-dur, 
#'            transderm-nitro, nitro-time, dilatrate-sr, ismo
#' other_antihypertensives : 
#'            hydralazine, clonidine, minoxidil, reserpine, serpasil, aliskiren, tekturna, apresoline, dralzine, loniten
#' @export
antihypertensives_breakout <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
	med_list <- list(
        'anti_platelet' = 
            c('aspirin', 'clopidogrel', 'prasugrel', 'ticagrelor', 'plavix', 'effient', 'brilinta'), 
        'beta_blockers' = 
            c('propranolol', 'inderal', 'atenolol', 'tenormin', 'bisoprolol', 'zebeta', 'metoprolol', 'lopressor', 
            'nadolol', 'corgard', 'nebivolol', 'bystolic', 'carvedilol', 'coreg', 'metoprolol tartrate', 'lopressor',
            'metoprolol succinate', 'toprol xl', 'labetalol', 'trandate', 'tenoretic', 'ziac', 'toprol', 'tenoretic',
            'ziac', 'coreg cr', 'acebutolol', 'sectral', 'pindolol', 'visken', 'penbutolol', 'levatol', 'betaxolol',
            'kerlone', 'celiprolol', 'selectol', 'corzide', 'inderide', 'timolide'), 
        'ace_inhibitors' = 
            c('fosinopril', 'fosinopril sodium', 'monopril', 'ramipril', 'altace', 'captopril', 'capoten', 'moexipril',
            'univasc', 'lisinopril', 'prinivil', 'zestril', 'enalapril', 'vasotec', 'epaned', 'quinapril', 'accupril',
            'trandolapril', 'mavik ', 'gopten', 'odrik', 'benazepril', 'lotensin', 'perindopril', 'aceon'), 
        'calcium_channel_blockers' = 
            c('amlodipine', 'norvasc', 'diltiazem', 'cardizem', 'tiazac', 'felodipine', 'isradipine', 'nicardipine',
            'cardenesr', 'cardene sr', 'cardene-sr', 'nifedipine', 'procardia', 'nisoldipine', 'sular', 'verapamil', 
            'calan', 'verelan', 'covera-hs', 'covera hs', 'coverahs', 'lotrel', 'caduet', 'bepridil', 'vascor', 'clevidipine', 
            'cleviprex', 'isoptin', 'plendil', 'lacidipine', 'caldine', 'lacimen', 'lacipil', 'midotens', 'motens', 'lercanidipine', 
            'lercadip', 'zanadip', 'dynacirc', 'adalat', 'nifediac', 'nifedical', 'nimodipine', 'nimotop', 'prestalia', 'teczem', 
            'lexxel', 'carmen ace', 'coripren', 'tarka', 'azor', 'tribenzor', 'twynsta', 'exforge', 'exforge hct'), 
        'angiotenstive_ii_receptor_antagonists' = 
            c('candesartan', 'atacand', 'eprosartan', 'teveten', 'irbesartan', 'avapro', 'losartan', 'cozaar', 
            'olmesartan', 'benicar', 'telmisartan', 'micardis', 'valsartan', 'diovan', 'hyzaar', 'diovanhct', 
            'diovan-hct', 'diovan hct', 'entresto', 'sacubatril-valsartan', 'azilsartan', 'edarbi', 'avalide', 
            'azor', 'tribenzor', 'twynsta', 'exforge', 'exforge hct'), 
        'non_loop_diuretics' = 
            c('hydrochlorothiazide', 'hctz', 'chlorthiazide', 'diuril', 'triamterene & hydrochlorothiazide', 'triamterene and hydrochlorothiazide', 
            'triamterene / hydrochlorothiazide', 'maxide', 'metolazone', 'zaroxolyn', 'lotensin hct', 'lotensin-hct', 'lotensinhct', 'capozide', 
            'vaseretic', 'prinizide', 'zestoretic', 'hyzaar', 'diovan hct', 'diovanhct', 'diovan-hct', 'tenoretic', 'ziac', 'amiloride', 'midamore', 
            'diuril', 'chlorthalidone', 'hygroton', 'indapamide', 'lozol', 'hydrodiuril', 'diulo', 'mykrox', 'tribenzor', 'exforge hct', 'moduretic', 
            'dyazide', 'maxzide', 'combipres', 'apresazide', 'aldoril', 'minizide'), 
        'mineralocorticoid_antagonists' = 
            c('spironolactone', 'aldactone', 'eplerenone', 'inspra'), 
        'statins' = 
            c('atorvastatin', 'lipitor', 'torvast', 'lovastatin', 'altocor', 'pravastatin', 'pravachol', 'rosuvastatin', 'crestor', 'simvastatin', 'zocor'), 
        'cholesterol_absorption_inhibitors' = 
            c('ezetimibe', 'vytorin'), 
        'loop_diuretics' = 
            c('furosemide', 'lasix', 'bumetanide', 'bumex', 'ethacrynic acid', 'edecrin', 'torsemide', 'demadex'), 
        'alpha_blockers' = c('prazosin', 'doxazosin', 'terazosin', 'minipress', 'cardura', 'hytrin', 'carduran', 'minipress xl'), 
        'alpha_ii_agonists' = 
            c('clonidine', 'methyldopa', 'guanabenz', 'guanfacine', 'lofexidine', 'catapres', 'dixarit', 'duraclon', 'jenloga', 'kapvay', 'nexiclon xr', 
            'wytensin', 'intuniv', 'tenex', 'aldomet', 'britlofex', 'aldoril', 'diupres', 'hydropres'), 
        'bile_acid_sequestrants' = 
            c('cholestyramine', 'colestopil', 'colesevelam'), 
        'nicotinic_acids' = 
            c('niacin'), 
        'fibric_acid_derivatives' = 
            c('gemfibrozil', 'lopid', 'triglide', 'fibricor', 'lipofen', 'tricor', 'bezafibrate'), 
        'oral_antihypertensives' = 
            c('warfarin', 'dabigatran', 'apixaban', 'rivaroxaban', 'edoxaban', 'coumadin', 'pradaxa', 'eliquis', 'xarelto', 'savaysa'), 
        'subcutaneous_antihypertensives' = 
            c('dalteparin', 'enoxaparin'), 
        'nitrates' = 
            c('isosorbide mononitrate', 'isosorbide dinitrate', 'nitroglycerin', 'imdur', 'isordil', 'bidil', 'nitro-bid', 'nitro-dur', 
            'transderm-nitro', 'nitro-time', 'dilatrate-sr', 'ismo'), 
        'other_antihypertensives' = 
            c('hydralazine', 'clonidine', 'minoxidil', 'reserpine', 'serpasil', 'aliskiren', 'tekturna', 'apresoline', 'dralzine', 'loniten')
    )
    med_classes <- names(med_list)
	dt_list <- lapply(med_list, aou.reader::med_query, anchor_date_table, before, after)
    dt_list <- Map(cbind, dt_list, med_class = med_classes)
    dt_list <- lapply(dt_list, function(x) x[, drug_exposure_start_date := as.Date(drug_exposure_start_date)])
    dt <- rbindlist(dt_list)
    dt <- dt[order(drug_exposure_start_date)]
    dt <- dt[, row_num := 1:.N, .(person_id, med_class)]
    dt <- dt[row_num == 1]
    dt[, row_num := NULL]
    dt[, status := TRUE]
    dt_cast <- dcast(dt, person_id ~ med_class, value.var = c("drug_exposure_start_date","status"))
    med_date_cols <- paste0("drug_exposure_start_date_", med_classes)
    med_status_cols <- paste0("status_", med_classes)
    dt_cast[, antihypertensives_breakout_any_entry_date := apply(.SD, 1, min, na.rm = T), .SDcols = med_date_cols]
    dt_cast[, antihypertensives_breakout_any_status := TRUE]
    for (c in med_status_cols) dt_cast[, (c) := ifelse(is.na(get(c)), FALSE, get(c))]
    setnames(dt_cast, med_date_cols, paste0("antihypertensives_breakout_", med_classes, "_entry_date"))
    setnames(dt_cast, med_status_cols, paste0("antihypertensives_breakout_", med_classes, "_status"))
	.write_to_bucket(dt_cast, output_folder, "antihypertensives_breakout")
}
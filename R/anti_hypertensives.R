#' Anti Hypertensives
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/anti_hypertensives.csv
#' @details Returns persons on anti hypertensives
#'
#' Meds: "acebutolol","atenolol","betaxolol","bisoprolol","carvedilol","esmolol","labetalol","metoprolol","nadolol","nebivolol"
#' ,"pindolol","propranolol","sotalol","timolol","acetazolamide","dichlorphenamide","methazolamide","adenosine","amiodarone",
#' "bretylium","dextrose%lidocaine","disopyramide","dofetilide","dronedarone","flecainide","ibutilide","lidocaine","mexiletine",
#' "procainamide","propafenone","quinidine","alfuzosin","doxazosin","prazosin","silodosin","tamsulosin","terazosin","alirocumab",
#' "atorvastatin","bempedoic acid","bempedoic acid%ezetimibe","cholestyramine","colesevelam","colestipol","evinacumab-dgnb","evolocumab",
#' "ezetimibe","ezetimibe%rosuvastatin","ezetimibe%simvastatin","fenofibrate","fenofibric acid","fluvastatin","gemfibrozil",
#' "icosapent ethyl","inv-cirb#20-18 (preventable) atorvastatin 40mg%pbo","lomitapide","lovastatin","omega-3 acid","pitavastatin",
#' "pravastatin","rosuvastatin","simvastatin","aliskiren","aliskiren%hydrochlorothiazide","amlodipine%benazepril",
#' "amlodipine%hydrochlorothiazide%olmesartan","amlodipine%hydrochlorothiazide%valsartan","amlodipine%olmesartan","amlodipine%perindopril",
#' "amlodipine%telmisartan","amlodipine%valsartan","atenolol%chlorthalidone","azilsartan%chlorthalidone","benazepril%hydrochlorothiazide",
#' "bendroflumethiazide%nadolol","bisoprolol%hydrochlorothiazide","candesartan%hydrochlorothiazide","captopril%hydrochlorothiazide",
#' "enalapril%hydrochlorothiazide","eprosartan%hydrochlorothiazide","fosinopril%hydrochlorothiazide","hydralazine%isosorbide",
#' "hydrochlorothiazide%irbesartan","hydrochlorothiazide%lisinopril","hydrochlorothiazide%losartan","hydrochlorothiazide%methyldopa",
#' "hydrochlorothiazide%metoprolol","hydrochlorothiazide%moexipril","hydrochlorothiazide%olmesartan","hydrochlorothiazide%propranolol",
#' "hydrochlorothiazide%quinapril","hydrochlorothiazide%telmisartan","hydrochlorothiazide%valsartan","sacubitril%valsartan",
#' "trandolapril%verapamil","amiloride","amiloride%hydrochlorothiazide","eplerenone","finerenone","hydrochlorothiazide%spironolactone",
#' "hydrochlorothiazide%triamterene","spironolactone","triamterene","amlodipine","amlodipine%atorvastatin","amlodipine%celecoxib",
#' "clevidipine","diltiazem","felodipine","isradipine","nicardipine","nifedipine","nimodipine","nisoldipine","verapamil","amyl nitrite",
#' "isosorbide dinitrate","isosorbide mononitrate","nitroglycerin","ranolazine","azilsartan","candesartan","eprosartan","irbesartan",
#' "losartan","olmesartan","telmisartan","valsartan","benazepril","captopril","enalapril","enalaprilat","fosinopril","lisinopril",
#' "moexipril","perindopril","quinapril","ramipril","trandolapril","bumetanide","ethacrynic acid","furosemide","torsemide","
#' caffeine%magnesium salicylate","mannitol","pamabrom","spironolactone","chlorothiazide","chlorthalidone","hydrochlorothiazide",
#' "indapamide","metolazone","clonidine","guanabenz","guanfacine","hydralazine","iloprost","lofexidine","macitentan","mecamylamine",
#' "methyldopa","methyldopate","metyrosine","minoxidil","riociguat","sildenafil","sodium nitroprusside","tadalafil","ethanolamine oleate",
#' "morrhuate sodium","polidocanol","sodium tetradecyl sulfate","isoxsuprine","papaverine"
#' @export
anti_hypertensives <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    meds <- c("acebutolol","atenolol","betaxolol","bisoprolol","carvedilol","esmolol","labetalol","metoprolol","nadolol","nebivolol","pindolol","propranolol","sotalol","timolol","acetazolamide","dichlorphenamide","methazolamide","adenosine","amiodarone","bretylium","dextrose%lidocaine","disopyramide","dofetilide","dronedarone","flecainide","ibutilide","lidocaine","mexiletine","procainamide","propafenone","quinidine","alfuzosin","doxazosin","prazosin","silodosin","tamsulosin","terazosin","alirocumab","atorvastatin","bempedoic acid","bempedoic acid%ezetimibe","cholestyramine","colesevelam","colestipol","evinacumab-dgnb","evolocumab","ezetimibe","ezetimibe%rosuvastatin","ezetimibe%simvastatin","fenofibrate","fenofibric acid","fluvastatin","gemfibrozil","icosapent ethyl","inv-cirb#20-18 (preventable) atorvastatin 40mg%pbo","lomitapide","lovastatin","omega-3 acid","pitavastatin","pravastatin","rosuvastatin","simvastatin","aliskiren","aliskiren%hydrochlorothiazide","amlodipine%benazepril","amlodipine%hydrochlorothiazide%olmesartan","amlodipine%hydrochlorothiazide%valsartan","amlodipine%olmesartan","amlodipine%perindopril","amlodipine%telmisartan","amlodipine%valsartan","atenolol%chlorthalidone","azilsartan%chlorthalidone","benazepril%hydrochlorothiazide","bendroflumethiazide%nadolol","bisoprolol%hydrochlorothiazide","candesartan%hydrochlorothiazide","captopril%hydrochlorothiazide","enalapril%hydrochlorothiazide","eprosartan%hydrochlorothiazide","fosinopril%hydrochlorothiazide","hydralazine%isosorbide","hydrochlorothiazide%irbesartan","hydrochlorothiazide%lisinopril","hydrochlorothiazide%losartan","hydrochlorothiazide%methyldopa","hydrochlorothiazide%metoprolol","hydrochlorothiazide%moexipril","hydrochlorothiazide%olmesartan","hydrochlorothiazide%propranolol","hydrochlorothiazide%quinapril","hydrochlorothiazide%telmisartan","hydrochlorothiazide%valsartan","sacubitril%valsartan","trandolapril%verapamil","amiloride","amiloride%hydrochlorothiazide","eplerenone","finerenone","hydrochlorothiazide%spironolactone","hydrochlorothiazide%triamterene","spironolactone","triamterene","amlodipine","amlodipine%atorvastatin","amlodipine%celecoxib","clevidipine","diltiazem","felodipine","isradipine","nicardipine","nifedipine","nimodipine","nisoldipine","verapamil","amyl nitrite","isosorbide dinitrate","isosorbide mononitrate","nitroglycerin","ranolazine","azilsartan","candesartan","eprosartan","irbesartan","losartan","olmesartan","telmisartan","valsartan","benazepril","captopril","enalapril","enalaprilat","fosinopril","lisinopril","moexipril","perindopril","quinapril","ramipril","trandolapril","bumetanide","ethacrynic acid","furosemide","torsemide","caffeine%magnesium salicylate","mannitol","pamabrom","spironolactone","chlorothiazide","chlorthalidone","hydrochlorothiazide","indapamide","metolazone","clonidine","guanabenz","guanfacine","hydralazine","iloprost","lofexidine","macitentan","mecamylamine","methyldopa","methyldopate","metyrosine","minoxidil","riociguat","sildenafil","sodium nitroprusside","tadalafil","ethanolamine oleate","morrhuate sodium","polidocanol","sodium tetradecyl sulfate","isoxsuprine","papaverine")
    bp_meds <- aou.reader::med_query(meds,anchor_date_table,before,after)
    result_all <- bp_meds[,.(anti_hypertensives_status = length(drug_exposure_start_date) > 0,
                                anti_hypertensives_entry_date = min(drug_exposure_start_date)),
                                .(person_id)]
    .write_to_bucket(result_all,output_folder,"anti_hypertensives")
}

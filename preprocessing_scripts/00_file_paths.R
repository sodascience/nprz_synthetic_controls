# Lists containing file paths used by preprocessing scripts
# This file should be sourced before running the preprocessing steps
# Sourcing this file also gives an error if files do not exist / have changed
# Last edited 20230817 by @vankesteren
PATHS = list(
  INSCHRWPO = list(
    "2009" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2008V3.SAV",
    "2010" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2009V4.SAV",
    "2011" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2010V4.SAV",
    "2012" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2011V3.SAV",
    "2013" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2012V2.SAV",
    "2014" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2013V3.SAV",
    "2015" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2014V4.SAV",
    "2016" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2015V5.SAV",
    "2017" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2016V4.SAV",
    "2018" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2017V5.SAV",
    "2019" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2018V3.SAV",
    "2020" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2019V3.SAV",
    "2021" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2020V2.SAV",
    "2022" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2021V2.SAV",
    "2023" = "G:/Onderwijs/INSCHRWPOTAB/INSCHRWPOTAB2022V2.SAV"
  ),
  SCHOOL_INTERVENTION = "L:/8913_NPRZCBKV1.sav",
  GBA = "G:/Bevolking/GBAPERSOONTAB/2022/GBAPERSOON2022TABV2.sav",
  # unused GBAADRESBUS = "G:/Bevolking/GBAADRESOBJECTBUS/GBAADRESOBJECT2022BUSV1.sav",
  # unused VSLGWB = "G:/BouwenWonen/VSLGWBTAB/VSLGWB2022TAB03V2.sav",
  HHBUS = "G:/Bevolking/GBAHUISHOUDENSBUS/GBAHUISHOUDENS2022BUSV1.sav",
  KOPPELHH = list(
    "2008" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/Koppeltabel_IPI_IHI_IVB2008.sav",
    "2009" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/Koppeltabel_IPI_IHI_IVB2009.sav",
    "2010" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/Koppeltabel_IPI_IHI_IVB2010.sav",
    "2011" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/KOPPELPERSOONHUISHOUDEN2011.sav",
    "2012" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/KOPPELPERSOONHUISHOUDEN2012.sav",
    "2013" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/KOPPELPERSOONHUISHOUDEN2013.sav",
    "2014" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/KOPPELPERSOONHUISHOUDEN2014.sav",
    "2015" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/KOPPELPERSOONHUISHOUDEN2015.sav",
    "2016" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/KOPPELPERSOONHUISHOUDEN2016.sav",
    "2017" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/KOPPELPERSOONHUISHOUDEN2017.sav",
    "2018" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/KOPPELPERSOONHUISHOUDEN2018.sav",
    "2019" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/KOPPELPERSOONHUISHOUDEN2019V2.sav",
    "2020" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/KOPPELPERSOONHUISHOUDEN2020V2.sav",
    "2021" = "G:/InkomenBestedingen/KOPPELPERSOONHUISHOUDEN/KOPPELPERSOONHUISHOUDEN2021V2.sav"
  ),
  IHI = list(
    "2008" = "G:/InkomenBestedingen/INTEGRAAL HUISHOUDENS INKOMEN/2008/HUISHBVRINK2008TABV3.sav",
    "2009" = "G:/InkomenBestedingen/INTEGRAAL HUISHOUDENS INKOMEN/2009/HUISHBVRINK2009TABV3.sav",
    "2010" = "G:/InkomenBestedingen/INTEGRAAL HUISHOUDENS INKOMEN/2010/HUISHBVRINK2010TABV3.sav"
  ),
  INHA = list(
    "2011" = "G:/InkomenBestedingen/INHATAB/INHA2011TABV2.sav",
    "2012" = "G:/InkomenBestedingen/INHATAB/INHA2012TABV2.sav",
    "2013" = "G:/InkomenBestedingen/INHATAB/INHA2013TABV2.sav",
    "2014" = "G:/InkomenBestedingen/INHATAB/INHA2014TABV1.sav",
    "2015" = "G:/InkomenBestedingen/INHATAB/INHA2015TABV1.sav",
    "2016" = "G:/InkomenBestedingen/INHATAB/INHA2016TABV2.sav",
    "2017" = "G:/InkomenBestedingen/INHATAB/INHA2017TABV2.sav",
    "2018" = "G:/InkomenBestedingen/INHATAB/INHA2018TABV2.sav",
    "2019" = "G:/InkomenBestedingen/INHATAB/INHA2019TABV2.sav",
    "2020" = "G:/InkomenBestedingen/INHATAB/INHA2020TABV2.sav",
    "2021" = "G:/InkomenBestedingen/INHATAB/INHA2021TABV2.sav"
  ),
  VEHTAB = list(
    "2009" = "G:/InkomenBestedingen/VEHTAB/VEH2009TABV3.sav",
    "2010" = "G:/InkomenBestedingen/VEHTAB/VEH2010TABV3.sav",
    "2011" = "G:/InkomenBestedingen/VEHTAB/VEH2011TABV2.sav",
    "2012" = "G:/InkomenBestedingen/VEHTAB/VEH2012TABV2.sav",
    "2013" = "G:/InkomenBestedingen/VEHTAB/VEH2013TABV2.sav",
    "2014" = "G:/InkomenBestedingen/VEHTAB/VEH2014TABV2.sav",
    "2015" = "G:/InkomenBestedingen/VEHTAB/VEH2015TABV2.sav",
    "2016" = "G:/InkomenBestedingen/VEHTAB/VEH2016TABV3.sav",
    "2017" = "G:/InkomenBestedingen/VEHTAB/VEH2017TABV3.sav",
    "2018" = "G:/InkomenBestedingen/VEHTAB/VEH2018TABV3.sav",
    "2019" = "G:/InkomenBestedingen/VEHTAB/VEH2019TABV4.sav",
    "2020" = "G:/InkomenBestedingen/VEHTAB/VEH2020TABV3.sav",
    "2021" = "G:/InkomenBestedingen/VEHTAB/VEH2021TABV3.sav"
  ),
  PARENT_TAB = "G:/Bevolking/KINDOUDERTAB/KINDOUDER2022TABV1.sav",
  HIGHEST_EDUCATION = list(
    "2008" = "G:/Onderwijs/HOOGSTEOPLTAB/2008/120619 HOOGSTEOPLTAB 2008V1.sav",
    "2009" = "G:/Onderwijs/HOOGSTEOPLTAB/2009/120619 HOOGSTEOPLTAB 2009V1.sav",
    "2010" = "G:/Onderwijs/HOOGSTEOPLTAB/2010/120918 HOOGSTEOPLTAB 2010V1.sav",
    "2011" = "G:/Onderwijs/HOOGSTEOPLTAB/2011/130924 HOOGSTEOPLTAB 2011V1.SAV",
    "2012" = "G:/Onderwijs/HOOGSTEOPLTAB/2012/141020 HOOGSTEOPLTAB 2012V1.sav",
    "2013" = "G:/Onderwijs/HOOGSTEOPLTAB/2013/HOOGSTEOPL2013TABV3.sav",
    "2014" = "G:/Onderwijs/HOOGSTEOPLTAB/2014/HOOGSTEOPL2014TABV3.sav",
    "2015" = "G:/Onderwijs/HOOGSTEOPLTAB/2015/HOOGSTEOPL2015TABV3.sav",
    "2016" = "G:/Onderwijs/HOOGSTEOPLTAB/2016/HOOGSTEOPL2016TABV2.sav",
    "2017" = "G:/Onderwijs/HOOGSTEOPLTAB/2017/HOOGSTEOPL2017TABV3.sav",
    "2018" = "G:/Onderwijs/HOOGSTEOPLTAB/2018/HOOGSTEOPL2018TABV3.sav",
    "2019" = "G:/Onderwijs/HOOGSTEOPLTAB/2019/HOOGSTEOPL2019TABV2.sav",
    "2020" = "G:/Onderwijs/HOOGSTEOPLTAB/2020/HOOGSTEOPL2020TABV2.sav",
    "2021" = "G:/Onderwijs/HOOGSTEOPLTAB/2021/HOOGSTEOPL2021TABV2.sav"
  ),
  EDUCATION_LUT = "K:/Utilities/Code_Listings/SSBreferentiebestanden/OPLEIDINGSNRREFV32.SAV"
)

# flatten list and check if files exist
flat_paths <- unlist(PATHS, use.names = FALSE)
files_exist <- vapply(flat_paths, file.exists, TRUE, USE.NAMES = TRUE)
if (!all(files_exist)) {
  stop(
    "Files not found: \n - ", 
    paste(names(files_exist)[!files_exist], collapse = "\n - "),
    "\n\nCheck and update the resource location in preprocessing_scripts/00_file_paths.R"
  )
}
rm(flat_paths, files_exist)
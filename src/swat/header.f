      subroutine header

!!    ~ ~ ~ PURPOSE ~ ~ ~                                               
!!    This subroutine defines header titles for the different output files

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~                                    
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    hedb(:)     |NA            |column titles in subbasin output file
!!    hedr(:)     |NA            |column titles in reach output file
!!    hedrsv(:)   |NA            |column titles in reservoir output file
!!    heds(:)     |NA            |column titles in HRU output file
!!    hedwtr(:)   |NA            |column titles in HRU impoundment output 
!!                               |file
!!    icolb(:)    |none          |space number for beginning of column in
!!                               |subbasin output file
!!    icolr(:)    |none          |space number for beginning of column in
!!                               |reach output file
!!    icolrsv(:)  |none          |space number for beginning of column in
!!                               |reservoir output file
!!    icols(:)    |none          |space number for beginning of column in
!!                               |HRU output file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~                        

      use parm

!!    column headers for HRU output file
      heds = (/"  PRECIPmm"," SNOFALLmm"," SNOMELTmm","     IRRmm", !4   
     &         "     PETmm","      ETmm"," SW_INITmm","  SW_ENDmm", !8   
     &         "    PERCmm"," GW_RCHGmm"," DA_RCHGmm","   REVAPmm", !12  
     &         "  SA_IRRmm","  DA_IRRmm","   SA_STmm","   DA_STmm", !16  
     &         "SURQ_GENmm","SURQ_CNTmm","   TLOSSmm"," LATQGENmm", !20  
     &         "    GW_Qmm","    WYLDmm","   DAILYCN"," TMP_AVdgC", !24  
     &         " TMP_MXdgC"," TMP_MNdgC","SOL_TMPdgC","SOLARMJ/m2", !28  
     &         "  SYLDt/ha","  USLEt/ha","N_APPkg/ha","P_APPkg/ha", !32  
     &         "NAUTOkg/ha","PAUTOkg/ha"," NGRZkg/ha"," PGRZkg/ha", !36  
     &         "NCFRTkg/ha","PCFRTkg/ha","NRAINkg/ha"," NFIXkg/ha", !40  
     &         " F-MNkg/ha"," A-MNkg/ha"," A-SNkg/ha"," F-MPkg/ha", !44  
     &         "AO-LPkg/ha"," L-APkg/ha"," A-SPkg/ha"," DNITkg/ha", !48  
     &         "  NUPkg/ha","  PUPkg/ha"," ORGNkg/ha"," ORGPkg/ha", !52  
     &         " SEDPkg/ha","NSURQkg/ha","NLATQkg/ha"," NO3Lkg/ha", !56  
     &         "NO3GWkg/ha"," SOLPkg/ha"," P_GWkg/ha","    W_STRS", !60  
     &         "  TMP_STRS","    N_STRS","    P_STRS","  BIOMt/ha", !64  
     &         "       LAI","   YLDt/ha","  BACTPct ","  BACTLPct", !68 
     &         " WTAB CLIm"," WTAB SOLm","     SNOmm"," CMUPkg/ha", !72 
     &         "CMTOTkg/ha","   QTILEmm"," TNO3kg/ha"," LNO3kg/ha", !76 
     &         "  GW_Q_Dmm"," LATQCNTmm"," TVAPkg/ha",                !79
     &     "  Hg2ToDsDry","  Hg2ToPtDry","  Hg2LfDsDry","  Hg2DrPtDry",   !83 
     &     "  Hg2ToDsWet","  Hg2DrDsWet","  Hg2ThDsWet","  Hg0LfFalDs",   !87  
     &     "  Hg2LfFalDs","  Hg2LfFalPt","  Hg0LfStoDs","  Hg2LfStoDs",   !91  
     &     "  Hg2LfStoPt","  Hg0RsStoDs","  Hg2RsStoDs","  Hg2RsStoPt",   !95  
     &     "  Hg0SoStoDs","  Hg2SoStoDs","  MHgSoStoDs","  Hg0SoStoPt",   !99  
     &     "  Hg2SoStoPt","  MHgSoStoPt","  Hg0ShStoDs","  Hg2ShStoDs",   !103  
     &     "  MHgShStoDs","  Hg0SurfqDs","  Hg2SurfqDs","  MHgSurfqDs",   !107  
     &     "  Hg0LatlqDs","  Hg2LatlqDs","  MHgLatlqDs","  Hg0PercqDs",   !111  
     &     "  Hg2PercqDs","  MHgPercqDs","  Hg0Gwq__Ds","  Hg2Gwq__Ds",   !115  
     &     "  MHgGwq__Ds","  Hg0SedYlPt","  Hg2SedYlPt","  MHgSedYlPt",   !119
     &     "  THgSolConc","   Hg0WyldDs","   Hg2WyldDs","  MeHgWyldDs",   !123
     &     "  Hg0_VOLATI","   Hg0_DRchg","   Hg2_DRchg","   MHg_DRchg"/)     !127

!!    numbers printed to VB interface HRU output file 
      icols = (/43,53,63,73,83,93,103,113,123,133,143,153,              
     &163,173,183,193,203,213,223,233,243,253,263,273,283,              
     &293,303,313,323,333,343,353,363,373,383,393,403,413,              
     &423,433,443,453,463,473,483,493,503,513,523,533,543,              
     &553,563,573,583,593,603,613,623,633,643,653,663,673,              
     &683,693,703,713,723,733,743,753,763,773,783,793,803,              
     &813,823/)

!!    column headers for subbasin output file
      hedb = (/"  PRECIPmm"," SNOMELTmm","     PETmm","      ETmm",  !4      
     &         "      SWmm","    PERCmm","    SURQmm","    GW_Qmm",  !8      
     &         "    WYLDmm","  SYLDt/ha"," ORGNkg/ha"," ORGPkg/ha",  !12     
     &         "NSURQkg/ha"," SOLPkg/ha"," SEDPkg/ha"," LAT Q(mm)",  !16     
     &         "LATNO3kg/h","GWNO3kg/ha","CHOLAmic/L","CBODU mg/L",  !20     
     &         "     DOXkG","     TILEQ","    TILEQN","   VAPTILE",  !24     
     &      "  Hg2ToDsDry","  Hg2ToPtDry","  Hg2LfDsDry","  Hg2LfPtDry",  !28     
     &      "  Hg2ToDsWet","  Hg2DrDsWet","  Hg2ThDsWet","  Hg0LfFalDs",  !32     
     &      "  Hg2LfFalDs","  Hg2LfFalPt","  Hg0LfStoDs","  Hg2LfStoDs",  !36     
     &      "  Hg2LfStoPt","  Hg0RsStoDs","  Hg2RsStoDs","  Hg2RsStoPt",  !40     
     &      "  Hg0SoStoDs","  Hg2SoStoDs","  MHgSoStoDs","  Hg0SoStoPt",  !44     
     &      "  Hg2SoStoPt","  MHgSoStoPt","  Hg0ShStoDs","  Hg2ShStoDs",  !48     
     &      "  MHgShStoDs","  Hg0SurfqDs","  Hg2SurfqDs","  MHgSurfqDs",  !52     
     &      "  Hg0LatlqDs","  Hg2LatlqDs","  MHgLatlqDs","  Hg0PercqDs",  !56     
     &      "  Hg2PercqDs","  MHgPercqDs","  Hg0Gwq__Ds","  Hg2Gwq__Ds",  !60     
     &      "  MHgGwq__Ds","  Hg0SedYlPt","  Hg2SedYlPt","  MHgSedYlPt",  !64 
     &      "   Hg0WyldDs","   Hg2WyldDs","  MeHgWyldDs","  THgSolConc",  !68
     &  "  Hg0_VOLATI","   Hg0_DRchg","   Hg2_DRchg","   MHg_DRchg"/)                                        !69   

!!    numbers printed to VB interface subbasin output file 
      icolb = (/35,45,55,65,75,85,95,105,115,125,135,145,               
     &155,165,175,185,195,205,215,225,235,245/)
!!  added headers TOTAL N/TOTALP/NO3 Concentration TO HEADING FOR OUTPUT.RCH GSM 10/26/2011
!!    column headers for reach output file
      hedr = (/"  FLOW_INcms"," FLOW_OUTcms","     EVAPcms",  !3          
     &         "    TLOSScms","  SED_INtons"," SED_OUTtons",  !6          
     &         "SEDCONCmg/kg","   ORGN_INkg","  ORGN_OUTkg",  !9          
     &         "   ORGP_INkg","  ORGP_OUTkg","    NO3_INkg",  !12          
     &         "   NO3_OUTkg","    NH4_INkg","   NH4_OUTkg",  !15          
     &         "    NO2_INkg","   NO2_OUTkg","   MINP_INkg",  !18          
     &         "  MINP_OUTkg","   CHLA_INkg","  CHLA_OUTkg",  !21          
     &         "   CBOD_INkg","  CBOD_OUTkg","  DISOX_INkg",  !24          
     &         " DISOX_OUTkg"," SOLPST_INmg","SOLPST_OUTmg",  !27          
     &         " SORPST_INmg","SORPST_OUTmg","  REACTPSTmg",  !30        
     &         "    VOLPSTmg","  SETTLPSTmg","RESUSP_PSTmg",  !33          
     &         "DIFFUSEPSTmg","REACBEDPSTmg","   BURYPSTmg",  !36          
     &         "   BED_PSTmg"," BACTP_OUTct","BACTLP_OUTct",  !39          
     &         "  CMETAL#1kg","  CMETAL#2kg","  CMETAL#3kg",  !42          
     &         "     TOT Nkg","     TOT Pkg"," NO3ConcMg/l",  !45          
     &         "    WTMPdegc",                                !46  
     &         "   Hg0DmgOut","   Hg2DmgOut","  MeHgDmgOut",  !49
     &         "   Hg0PmgOut","   Hg2PmgOut","  MeHgPmgOut",  !52
     &         "    Hg0DmgIn","    Hg2DmgIn","   MeHgDmgIn",  !55
     &         "    Hg0PmgIn","    Hg2PmgIn","   MeHgPmgIn",  !58
     &         "   Hg0DmgSto","   Hg2DmgSto","  MeHgDmgSto",  !61
     &         "   Hg0PmgSto","   Hg2PmgSto","  SedTHgCppm"/) !64
     
!!    numbers printed to VB interface reach output file 
      icolr = (/38,50,62,74,86,98,110,122,134,146,158,170,182,194,206,  
     &218,230,242,254,266,278,290,302,314,326,338,350,362,374,386,398,  
     &410,422,434,446,458,470,482,494,506,518,530,542,554,566,578,590,  
     &602,614,626,638,650,662,674,686,698,710,722,734,746,758,770/)

!!    column headers for reservoir output file
      hedrsv = (/"    VOLUMEm3","  FLOW_INcms"," FLOW_OUTcms",  !3         
     &           "    PRECIPm3","      EVAPm3","   SEEPAGEm3",  !6         
     &           "  SED_INtons"," SED_OUTtons"," SED_CONCppm",  !9         
     &           "   ORGN_INkg","  ORGN_OUTkg"," RES_ORGNppm",  !12        
     &           "   ORGP_INkg","  ORGP_OUTkg"," RES_ORGPppm",  !15        
     &           "    NO3_INkg","   NO3_OUTkg","  RES_NO3ppm",  !18        
     &           "    NO2_INkg","   NO2_OUTkg","  RES_NO2ppm",  !21        
     &           "    NH3_INkg","   NH3_OUTkg","  RES_NH3ppm",  !24        
     &           "   MINP_INkg","  MINP_OUTkg"," RES_MINPppm",  !27        
     &           "   CHLA_INkg","  CHLA_OUTkg","SECCHIDEPTHm",  !30        
     &           "   PEST_INmg","  REACTPSTmg","    VOLPSTmg",  !33        
     &           "  SETTLPSTmg","RESUSP_PSTmg","DIFFUSEPSTmg",  !36        
     &           "REACBEDPSTmg","   BURYPSTmg","  PEST_OUTmg",  !39        
     &           "PSTCNCWmg/m3","PSTCNCBmg/m3",                  !41
     &           "   Hg0D_INmg","   Hg2D_INmg","  MeHgD_INmg",  !44        
     &           "   Hg0P_INmg","   Hg2P_INmg","  MeHgP_INmg",  !47        
     &           "   Hg0D_OUmg","   Hg2D_OUmg","  MeHgD_OUmg",  !50        
     &           "   Hg0P_OUmg","   Hg2P_OUmg","  MeHgP_OUmg",  !53        
     &           "   THg_STOmg","   MHg_STOmg"," Hg2_NetDiff",  !56        
     &           " MHg_NetDiff","  Hg0P_DEPmg","  Hg2P_DEPmg",  !59 
     &           "  MHgP_DEPmg"/)  !60        

!!    numbers printed to VB interface reservoir output file 
      icolrsv = (/38,50,62,74,86,98,110,122,134,146,158,170,182,194,    
     &206,218,230,242,254,266,278,290,302,314,326,338,350,362,374,386,  
     &398,410,422,434,446,458,470,482,494,506,518/)

!!    column headers for HRU impoundment output file
      hedwtr = (/"  PNDPCPmm","  PND_INmm","PSED_It/ha","  PNDEVPmm",   
     &           "  PNDSEPmm"," PND_OUTmm","PSED_Ot/ha"," PNDVOLm^3",   
     &           "PNDORGNppm"," PNDNO3ppm","PNDORGPppm","PNDMINPppm",   
     &           "PNDCHLAppm","  PNDSECIm","  WETPCPmm","  WET_INmm",   
     &           "WSED_It/ha","  WETEVPmm","  WETSEPmm"," WET_OUTmm",   
     &           "WSED_Ot/ha"," WETVOLm^3","WETORGNppm"," WETNO3ppm",   
     &           "WETORGPppm","WETMINPppm","WETCHLAppm","  WETSECIm",   
     &           "  POTPCPmm","  POT_INmm","OSED_It/ha","  POTEVPmm",   
     &           "  POTSEPmm"," POT_OUTmm","OSED_Ot/ha"," POTVOLm^3",   
     &           "  POT_SAha","HRU_SURQmm","PLANT_ETmm"," SOIL_ETmm"/)

      return
      end                                           
let galilei = "A02DA03DA04SA05SA06SA10SA11SA12SA13SA14DB01DB02WB03DB04DB05SB06DB08DB09DB10SB11DB12DB13ZB14DC01SC02DC03DC04DC05DC06DC07DC08DC09DC10DC11DC12DC13DC14SD02DD03DD05DD08DD09DD10SD11DD12DD13DD14SE02SE03DE04DE05DE06DE08DE09DE10DE11DE12SE13DF01SF02DF03DF04DF05DF06DF07DF08DF09DF10SF11DG01DG02DG03DG04DG05DG06DG07SG08DG09DG10DG11DG12SG13DG14SH01SH02SH03SH04DH06DH07SH08DH09DH12DH13DH14SI01SI02DI03DI04DI05DI07DI08DI09SI10DI11DI13DI14SJ01SJ02DJ03DJ04DJ05DJ06DJ09DJ10DJ11DJ13DJ14SK01DK02SK03DK04DK05SK06DK08DK09SK10DK11SK12DK14DL01DL02SL03DL04SL05DL06AL08HL09SL10DL11SL12DL13DL14SM01DM02SM03DM04DM05SM06DM08DM09SM10DM11SM12DM13DM14DN01DN02DN03SN04DN05DN06DN08DN09DN10DN11DN12DN13DN14SO02DO05SO06DO07DO08DO09SO10DO11DO12DO13DO14SP01SP02DP03SP04SP05DP06DP07DP08DP09DP10DP11DP12SP13DP14DQ01SQ02DQ03DQ04SQ05DQ06SQ07DQ08DQ09DQ10DQ11SQ12DQ13DQ14SR01SR02DR03DR04SR06SR07SR08SR09DR12SR13DS02DS04DS05DS06DS07DS08DS09DS12DS13DT02DT05DT06DT07ST08ST11DT12DT13DT14SU01SU02DU03DU04DU05SU06DU07DU08DU09DU10DU11DU12SU13DU14DV01SV02XV03DV04DV05DV06DV08SV09DV10DV11DV12DV13YV14DW02DW03SW04SW05SW06SW09DW10SW11SW12SW13DW14D"

type map_type = 
    | RandomBig
    | RandomMedium
    | RandomSmall
    | String of string

type map_info = {
    name : string;
    value : map_type;
}

let built_ins = [
    { name = "Galilei"; value = String galilei };
    { name = "Random (Big)"; value = RandomBig };
    { name = "Random (Medium)"; value = RandomMedium };
    { name = "Random (Small)"; value = RandomSmall };
]

let build_map (value : map_type) : SectorMap.t =
    match value with
    | String str -> SectorMap.from_map_string str
    | RandomBig -> SectorMap.random_map Game.board_size_w Game.board_size_h
    | RandomMedium -> SectorMap.random_map ~sector_count:(23*14/2) Game.board_size_w Game.board_size_h
    | RandomSmall -> SectorMap.random_map ~sector_count:(23*14/4) Game.board_size_w Game.board_size_h

;;

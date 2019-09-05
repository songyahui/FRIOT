module Generator.Analysis where
import Parser.AST 
import Data.Maybe
import Data.List.Split


find_function_by_name :: String -> [Decl] -> Maybe Decl
find_function_by_name name [] = Nothing
find_function_by_name name (x:xs) = 
    case x of 
    App a (PList li) c -> 
        case (li!!0) of
        PVar n -> if n == name 
                  then Just x 
                  else find_function_by_name name xs
    otherwise -> find_function_by_name name xs


cat_libraries :: String -> [Expr] -> String
cat_libraries temp [] = temp
cat_libraries temp (x:xs) = 
    case x of 
        Str s ->  if temp == "" 
                  then cat_libraries ( temp++s) xs
                  else cat_libraries ( temp++ "," ++ s) xs

lookup_record_by_first:: [(Expr,Expr)] -> String -> Maybe String
lookup_record_by_first [] _first = Nothing
lookup_record_by_first (x:xs) _first = 
    case x of
        (Var a,Var b)  -> if a == _first then Just b
                          else lookup_record_by_first xs _first
        (Var a,Num b)  -> if a == _first then Just $ show b
                          else lookup_record_by_first xs _first
        (Var a,Str b)  -> if a == _first then Just b
                          else lookup_record_by_first xs _first
        (Var a,List b) -> if a == _first 
                          then Just (cat_libraries "" b)
                          else lookup_record_by_first xs _first
        otherwise -> lookup_record_by_first xs _first

-----------------------------------------------------
---TO analyse main function: to get the name of the 
---Model, update and view functions
-----------------------------------------------------
function_names :: [(Expr,Expr)] -> Maybe [String]
function_names fun_lists = 
    let exact fun_name f_l = 
            case f_l of
            [] -> Nothing
            (x:xs) -> case fst x of
                      Var temp -> if temp == fun_name 
                                  then case snd x of
                                       Var name -> Just name
                                  else exact fun_name xs
                      otherwise -> exact fun_name xs
             
        model = (exact "MODEL" fun_lists)
        view = (exact "VIEW" fun_lists)
        update = (exact "UPDATE" fun_lists)
    in  case model of 
        Nothing -> Nothing
        Just m -> case view of 
                  Nothing -> Nothing
                  Just v -> case update of 
                            Nothing -> Nothing
                            Just u -> Just ([m] ++ [v] ++ [u])
        
        
main_function :: [Decl] -> Maybe [String]
main_function [] = Nothing
main_function (x:xs) = 
    case x of 
    App a (PList [(PVar "main")]) c -> 
        case c of
         Record l -> function_names l 
    otherwise -> main_function xs
        --DeclareType a b -> main_function xs


-----------------------------------------------------
generate_lib :: String -> [String] -> Maybe String
generate_lib temp [] = Just temp 
generate_lib temp (x:xs) = 
    let gpio = "var Gpio = require('onoff').Gpio;\n"
        sleep = "var sleep = require('system-sleep');\n"
        dht = "var dht_sensor = require('node-dht-sensor');\n"
        snesor = "var RaspiSensors = require('raspi-sensors');\n"
    in
    case x of 
        "\"onoff\"" -> generate_lib (temp ++ gpio ) xs 
        "\"system-sleep\"" -> generate_lib (temp ++ sleep ) xs 
        "\"node-dht-sensor\"" -> generate_lib (temp ++ dht ) xs 
        "\"raspi-sensors\"" -> generate_lib (temp ++ snesor ) xs 
        otherwise -> Nothing

generate_libs :: String -> String
generate_libs str = 
    let libs = splitOn "," str
    in case generate_lib "" libs of 
        Nothing -> "undefined library"
        Just str -> str++"\n"


generate_header :: [(Expr,Expr)]  -> String
generate_header li = 
    case lookup_record_by_first li "_library" of
        Nothing -> "wrong in _librarys"
        Just aa -> generate_libs aa


generate_device :: [(Expr,Expr)]  -> String
generate_device li =  
    let pin_num = lookup_record_by_first li "_device_pin_number"
        device_name = lookup_record_by_first li "_device_name"
        device_state = lookup_record_by_first li "_device_state"
    in
    case pin_num of 
    Nothing -> "can not find device pin number"
    Just pin -> 
        if pin == "NULL" then "\n" ----no devices definded there
        else case device_name of 
                Nothing -> "can not find device name"
                Just name -> case device_state of 
                        Nothing -> "can not find device initial state"
                        Just state -> "var " ++ name ++ " = new Gpio( " ++ pin ++ ", 'out');\n" ++ name ++ ".writeSync("++ state ++ ");\n\n"

declare_sensor :: String-> String-> String-> String-> String
declare_sensor _name _type _desc _addr =
    "var " ++ _name ++ " = new RaspiSensors.Sensor({\n    " ++ "type    : \"" ++ _type ++ "\",\n    address : " ++ _addr ++ "\n}, "++_desc ++");\n\n"

fatch_sensor :: String-> String
fatch_sensor _name =
    _name ++ ".fetch(function(err, data) {\nif(err) {\nprocess.exit(1)\n}\n\n" 

        
generate_sensor_IC2 :: [String] ->[String] -> [Decl] ->String --[_name,_type,_desc,_addr]
generate_sensor_IC2 parm fun_names trees= --"IC2"
    let declare = declare_sensor (parm!!0) (parm!!1) (parm!!2) (parm!!3)
        loop_upper = "while (true) { \n" ++ fatch_sensor (parm!!0)
        loop_middle = analyse_View fun_names trees
        loop_lower = "});\nsleep(" ++ (parm!!4) ++ ");\n}"
    in declare ++ loop_upper ++ loop_middle ++ loop_lower

generate_sensor_GPIO :: [String] ->[String] ->[Decl] -> String--[_name,_type,_desc,_addr]
generate_sensor_GPIO parm fun_names trees= "gpio"

generate_sensor :: [(Expr,Expr)]  ->[String] ->[Decl] -> String
generate_sensor li fun_names trees = --"sensor\n"
    let sensor_name = lookup_record_by_first li "_sensor_name" 
        sensor_type = lookup_record_by_first li "_sensor_type" 
        sensor_plug = lookup_record_by_first li "_plug_type" 
        sensor_desc = lookup_record_by_first li "_sensor_description" 
        sensor_addr = lookup_record_by_first li "_sensor_address"  
        interval = lookup_record_by_first li "_interval"
    in case sensor_name of 
        Nothing -> "can not find sensor name"
        Just _name -> 
            if _name == "NULL" then "\n"
            else case sensor_type of 
                Nothing -> "can not find sensor type"
                Just _type -> case sensor_desc of
                    Nothing -> "can not find sensor description"
                    Just _desc -> case sensor_addr of 
                        Nothing -> "can not find sensor address"
                        Just _addr -> case interval of
                            Nothing -> "can not find interval"
                            Just _inter-> 
                                case sensor_plug of 
                                Nothing -> "can not find sensor plug type"
                                Just plug -> 
                                    case plug of 
                                        "IC2"  -> generate_sensor_IC2 [_name,_type,_desc,_addr,_inter] fun_names trees
                                        "GPIO" -> generate_sensor_GPIO [_name,_type,_desc,_addr,_inter] fun_names trees
                                        otherwise -> "sensor plug type not definded"
            

analyse_Model ::[String] -> [Decl] -> String
analyse_Model fun_names trees = 
    let model_declare = find_function_by_name (fun_names!!0) trees
    in case model_declare of 
        Just (App a b (Record li)) -> 
            let header = generate_header li 
                device = generate_device li
                sensor = generate_sensor li fun_names trees
            in header ++ device ++ sensor

        otherwise -> "can not find function" ++ (fun_names!!0)
    
-----------------------------------------------------

view_Msg_list :: [Expr] -> [String]
view_Msg_list li = 
    let helper x = 
            case x of 
            ViewBody c d -> case (c!!0) of 
                            Var str -> str
                            otherwise -> "wrong in view_Msg_list1\n"
            otherwise -> "wrong in view_Msg_list2\n"
    in map helper li

find_Msg_type :: [Decl] -> Maybe [String]
find_Msg_type [] = Nothing
find_Msg_type (x:xs) = 
    case x of 
        DeclareType (Var a) b -> 
            if a == "Msg" 
            then Just (map (\x1 -> case x1 of Var t -> t)b)
            else find_Msg_type xs
        otherwise -> find_Msg_type xs

generate_view_app :: String -> [(Expr,Expr)] -> [String] -> [Decl] -> String
generate_view_app temp [] fun_names trees = temp
generate_view_app temp (x:xs) fun_names trees = 
    case x of 
        (BinaryOp a b c , Var v) -> 
            case a of 
                Var aa -> case c of 
                    Num cc -> "if (" ++ aa ++ " " ++ b ++ " " ++ (show cc) ++" ){\n" ++ analyse_Update v fun_names trees ++ "}\n"
                    otherwise -> "not defined cc\n"

                otherwise -> "not defined aa\n"
        otherwise -> "not defined else if or if\n"



view_msg_app :: String ->[String] -> [Decl] -> String
view_msg_app msg_app fun_names trees = 
    let app_decl = find_function_by_name msg_app trees
    in case app_decl of 
        Just (App a bb (If li b )) -> 
            let if_elseif = generate_view_app "" li fun_names trees
                _else = case b of 
                    Var msg -> analyse_Update msg fun_names trees
                    otherwise -> "not defined else"
            in if_elseif ++ "else {\n" ++_else++ "}\n\n"
            


generate_view :: String -> [String] -> [String]-> [String] -> [Decl] -> String
generate_view temp [] msg_li fun_names trees= temp
generate_view temp (x:xs) msg_li  fun_names trees= 
    if (x `elem` msg_li )
    then generate_view (temp ++ (analyse_Update x fun_names trees)) xs msg_li fun_names trees
    else 
        let app_dec = find_function_by_name x trees
        in case app_dec of 
            Nothing -> generate_view (temp ++ x ++ ":not definded\n") xs msg_li fun_names trees
            Just app -> generate_view (temp ++ (view_msg_app x fun_names trees)) xs msg_li fun_names trees

    
    

analyse_View :: [String] -> [Decl] -> String
analyse_View fun_names trees =
    let model_declare = find_function_by_name (fun_names!!1) trees
    in case model_declare of 
        Just (App a b (ViewBody c d )) -> 
            let view_msg_list = view_Msg_list d
                type_msg_list = find_Msg_type trees
            in  case type_msg_list of 
                Nothing -> "type Msg not defined"
                Just msg_li -> generate_view "" view_msg_list msg_li fun_names trees
        otherwise -> "can not find function" ++ (fun_names!!1)


-----------------------------------------------------

get_device_name :: [Decl] -> [String] -> String
get_device_name trees fun_names = 
    let model_declare = find_function_by_name (fun_names!!0) trees
    in case model_declare of 
        Just (App a b (Record li)) -> 
            let device_name = lookup_record_by_first li "_device_name"
            in case device_name of  
                Nothing -> "device name not defined"
                Just str -> str


generate_update_code :: [(Expr,Expr)] -> String -> String
generate_update_code li name = 
    let d_state = lookup_record_by_first li "_device_state"
    in case d_state of 
        Nothing -> "update wrong in msg" 
        Just state -> name ++ ".writeSync("++ state ++ ");\n\n"


generate_update :: [(Pattern,Expr)] -> String -> String -> String
generate_update [] msg device_name = msg ++ "is not defined in update function\n"
generate_update (x:xs) msg device_name = 
    case x of 
        (PList l, Record r) -> 
            case (l!!0) of 
                PVar m -> 
                    if m == msg 
                    then generate_update_code r  device_name
                    else generate_update xs msg device_name
                otherwise -> generate_update xs msg device_name
        otherwise -> generate_update xs msg device_name



analyse_Update :: String -> [String] ->[Decl] -> String
analyse_Update msg fun_names trees = 
    let model_declare = find_function_by_name (fun_names!!2) trees
    in case model_declare of 
        Just (App a b (Case (Var c) li )) -> 
            let device = get_device_name trees fun_names
            in generate_update li msg device

        otherwise -> "can not find function" ++ (fun_names!!2)


-----------------------------------------------------
traversal_AST :: [Decl] -> String
traversal_AST trees = 
    case main_function trees of 
        Nothing ->  "Wrong defination of main function"
        Just mvu -> analyse_Model mvu trees 
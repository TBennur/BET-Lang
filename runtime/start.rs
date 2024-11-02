use std::env;
use std::collections::HashMap;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

// #[derive(Deserialize)]
// struct StructNameToStructInfoMap {
//     data: StdHashMap<String, (String, StdHashMap<String, String>)>,
// }
// use std::collections::HashMap as StdHashMap;



// fn deserialize_structs(serialized: String) -> HashMap<i32, (String, HashMap<i32, String>)> {
//     let mut res_map = std::collections::HashMap::new();
//     let subres_strs_vec: Vec<&str> = serialized.split(",").collect();
//     for subres_str in subres_strs_vec {
//         let struct_vec: Vec<&str> = subres_str.split(".").collect();
//         if struct_vec.len() < 4 {
//             // struct name + struct enum + >= 1 field name + >= 1 field offset
//             panic!("Invalid: illegal struct type serialization")
//         }
//         let chunked: Vec<&[&str]> = struct_vec.chunks(2).collect();
//         let (first, rest) = match chunked.split_first() {
//             Some(a) => a,
//             None => panic!("Unexpected: broke"),
//         };
//         let (struct_type_enum, struct_name) = match first[..] {
//             [struct_type_enum, struct_name] => (struct_type_enum, struct_name),
//             _ => panic!("Unexpected: broke"),
//         };
//         let struct_type_enum: i32 = match (struct_type_enum).parse::<i32>() {
//             Err(_) => panic!("Invalid Input"),
//             Ok(val) => val,
//         };

//         let mut struct_offset_field_to_name: HashMap<i32, String> = std::collections::HashMap::new();
//         for chunk in rest {
//             if let [offset, field_name] = chunk {
//                 let offset: i32 = match (*offset).parse::<i32>() {
//                     Err(_) => panic!("Invalid Input"),
//                     Ok(val) => val,
//                 };
//                 struct_offset_field_to_name.insert(offset, field_name.to_string());
//             } else {
//                 panic!("Unexpected: broke")
//             }
//         }

//         // map the struct_type_enum to (struct_name, map)
//         res_map.insert(
//             struct_type_enum,
//             (struct_name.to_string(), struct_offset_field_to_name),
//         );
//     }
//     res_map
// }


fn deserialize_structs(serialized: String, target_type_enum: i32) -> Option<(String, HashMap<i32, String>)> {
    let subres_strs_vec: Vec<&str> = serialized.split(",").collect();
    for subres_str in subres_strs_vec {
        let struct_vec: Vec<&str> = subres_str.split(".").collect();
        if struct_vec.len() < 4 {
            // struct name + struct enum + >= 1 field name + >= 1 field offset
            panic!("Invalid: illegal struct type serialization")
        }
        let chunked: Vec<&[&str]> = struct_vec.chunks(2).collect();
        let (first, rest) = match chunked.split_first() {
            Some(a) => a,
            None => panic!("Unexpected: broke"),
        };
        let (struct_type_enum, struct_name) = match first[..] {
            [struct_type_enum, struct_name] => (struct_type_enum, struct_name),
            _ => panic!("Unexpected: broke"),
        };
        let struct_type_enum: i32 = match (struct_type_enum).parse::<i32>() {
            Err(_) => panic!("Invalid Input"),
            Ok(val) => val,
        };

        if struct_type_enum != target_type_enum {
            continue;
        }

        let mut struct_offset_field_to_name: HashMap<i32, String> = std::collections::HashMap::new();
        for chunk in rest {
            if let [offset, field_name] = chunk {
                let offset: i32 = match (*offset).parse::<i32>() {
                    Err(_) => panic!("Invalid Input"),
                    Ok(val) => val,
                };
                struct_offset_field_to_name.insert(offset, field_name.to_string());
            } else {
                panic!("Unexpected: broke")
            }
        }
        return Some((struct_name.to_string(), struct_offset_field_to_name));
    }
    None
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(error_flag: i64) {
    if error_flag == 1 {
        eprintln!("Runtime error: overflow");
    } else if error_flag == 2 {
        eprintln!("Runtime error: out of space");
    } else if error_flag == 3 {
        eprintln!("Runtime error: null dereference");
    } else {
        eprintln!("Runtime error: unknown");
    }
    std::process::exit(1);
}

const IS_BOOL: u64 = 0;
const IS_INT: u64 = 1;

#[export_name = "\x01snek_print"]
/// Changed snek_print to reflect the semantic meaning of print, which evaluates to the printed value
pub extern "C" fn snek_print(value: i64, type_flag: u64, msg: i64) -> i64 {
    let c_str = unsafe { std::ffi::CStr::from_ptr(msg as *const i8) };
    let str_slice = c_str.to_str().expect("Invalid UTF-8");
    if type_flag == IS_BOOL {
        if value == 0 {
            println!("false");
        } else {
            println!("true");
        }
    } else if type_flag == IS_INT {
        println!("{}", value);
    } else {
        match deserialize_structs(str_slice.to_string(), type_flag as i32) {
            None => snek_error(-1),
            Some(map) => println!("{:?}", map),
        };

        println!("unknown flag {}, {}", value, type_flag);
    }
    value
}

fn parse_input(input: &str) -> u64 {
    let inp: i64 = match input.parse::<i64>() {
        Err(_) => panic!("Invalid Input"),
        Ok(val) => val,
    };
    inp as u64
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "0" };
    let input = parse_input(&input);

    let _i: u64 = unsafe { our_code_starts_here(input) };
}

use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

const SIZEOF_I_64: i64 = 8;

/// If the passed `target_type_enum` exists in the `serialized` string, returns Some(
///    - the name of the struct
///    - a vector contianing, for each field in the struct (in order of offset):
///         - offset, field name, field type name
/// )
fn deserialize_structs(
    serialized: String,
    target_type_enum: i32,
) -> Option<(String, Vec<(i32, String, String)>)> {
    let subres_strs_vec: Vec<&str> = serialized.split(",").collect();
    for subres_str in subres_strs_vec {
        let struct_vec: Vec<&str> = subres_str.split(".").collect();
        if struct_vec.len() < 6 {
            // struct + struct_name + struct_enum + >= 1 field name + >= 1 field offset + >= 1 type
            panic!(
                "Invalid: illegal struct type serialization: {:?}",
                struct_vec
            )
        }
        let chunked: Vec<&[&str]> = struct_vec.chunks(3).collect();
        let (first, rest) = match chunked.split_first() {
            Some(a) => a,
            None => panic!("Unexpected: broke"),
        };
        let (_struct_keyword, struct_type_enum, struct_name) = match first[..] {
            [_struct_keyword, struct_type_enum, struct_name] => {
                (_struct_keyword, struct_type_enum, struct_name)
            }
            _ => panic!("Unexpected: broke"),
        };
        let struct_type_enum: i32 = match (struct_type_enum).parse::<i32>() {
            Err(_) => panic!("Invalid Input"),
            Ok(val) => val,
        };

        if struct_type_enum != target_type_enum {
            continue;
        }

        let mut struct_offset_field_to_name: Vec<(i32, String, String)> = Vec::new();
        for chunk in rest {
            if let [offset, field_name, field_type_name] = chunk {
                let offset: i32 = match (*offset).parse::<i32>() {
                    Err(_) => panic!("Invalid Input"),
                    Ok(val) => val,
                };
                struct_offset_field_to_name.push((
                    offset,
                    field_name.to_string(),
                    field_type_name.to_string(),
                ));
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
    } else if error_flag == 4 {
        eprintln!("Runtime error: invalid array size");
    } else if error_flag == 5 {
        eprintln!("Runtime error: invalid array access");
    } else {
        eprintln!("Runtime error: unknown");
    }
    std::process::exit(1);
}

const IS_INT_ARR: u64 = 10_000_000;
const IS_BOOL: u64 = 0;
const IS_INT: u64 = 1;
const IS_UNIT: u64 = 2;

#[export_name = "\x01snek_print"]
/// Changed snek_print to reflect the semantic meaning of print, which evaluates to the printed value
pub extern "C" fn snek_print(value: i64, type_flag: u64, msg: i64) -> i64 {
    let c_str = unsafe { std::ffi::CStr::from_ptr(msg as *const i8) };
    let str_slice = c_str.to_str().expect("Invalid UTF-8");

    if type_flag == IS_INT_ARR {
        let arr_size = match i64_to_addr(value) {
            None => {
                snek_error(-1);
                0
            }
            Some(&arr_size) => arr_size,
        };
        let mut vals: Vec<i64> = Vec::new();
        for i in 0..arr_size {
            vals.push(*i64_to_addr(value + (SIZEOF_I_64 * (i + 1))).unwrap());
        }
        println!(
            "[{}]",
            vals.iter()
                .map(|num| num.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
    } else if type_flag == IS_BOOL {
        if value == 0 {
            println!("false");
        } else {
            println!("true");
        }
    } else if type_flag == IS_INT {
        println!("{}", value);
    } else if type_flag == IS_UNIT {
        println!("unit");
    } else {
        // pointer to a struct
        match deserialize_structs(str_slice.to_string(), type_flag as i32) {
            None => snek_error(-1), // invalid struct type
            Some((struct_name, field_map)) => {
                if value == 0 {
                    // null pointer
                    println!("null pointer to struct {}", struct_name)
                } else {
                    println!("struct {struct_name} ({value})");
                    for (offset, field_name, field_type) in field_map {
                        // access the address of the field, interpreted as a 64 bit int, if it exists
                        match i64_to_addr(value + (SIZEOF_I_64 * (offset as i64))) {
                            None => snek_error(-1),
                            Some(&field_val) => {
                                println!("\t{}: {} = {}", field_name, field_type, field_val)
                            }
                        }
                    }
                }
            }
        };
    }
    value
}

/// Convert an i64 to a pointer to an i64
///
/// Returns None if it's NULL
fn i64_to_addr(addr: i64) -> Option<&'static i64> {
    let ptr = addr as *const i64;
    unsafe { ptr.as_ref() }
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

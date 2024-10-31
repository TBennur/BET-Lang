use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error() {
    eprintln!("overflow");
    std::process::exit(1);
}

const IS_BOOL: u64 = 0;
const IS_INT: u64 = 1;

#[export_name = "\x01snek_print"]
/// Changed snek_print to reflect the semantic meaning of print, which evaluates to the printed value
pub extern "C" fn snek_print(value: i64, type_flag: u64) -> i64 {
    if type_flag == IS_BOOL {
        if value == 0 {
            println!("false");
        } else {
            println!("true");
        }
    } else if type_flag == IS_INT {
        println!("{}", value);
    } else {
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

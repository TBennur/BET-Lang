def container_test(n_lines: int):
    let_bindings = [f"container_{i} := new container" for i in range(n_lines)]
    let_bindings = ",\n".join(let_bindings)


    assignations = [f"container_{i}.val" for i in range(n_lines)]
    assignation_str = "69"
    for assignment in assignations[::-1]:
        assignation_str = "(" + assignment + ":=" + assignation_str + ")"
    return f"""
    struct container(val::int);
    let(
{let_bindings}
) {{{assignation_str}}}"""

if __name__ == "__main__":
    print(container_test(5000))
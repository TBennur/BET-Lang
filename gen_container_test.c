#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure definition
struct container {
    int val;
};

// Function to create and format the output string
char* container_test(int n_lines) {
    // Allocate memory for the output string
    // Calculate a reasonable buffer size based on the number of lines
    int buffer_size = n_lines * 100 + 1000; // Rough estimation
    char* result = (char*)malloc(buffer_size);
    char* current_pos = result;

    // Write the structure definition
    current_pos += sprintf(current_pos, "struct container(val::int);\nlet(\n");

    // Generate container declarations
    for (int i = 0; i < n_lines; i++) {
        current_pos += sprintf(current_pos, "    container_%d := new container", i);
        if (i < n_lines - 1) {
            current_pos += sprintf(current_pos, ",\n");
        }
    }

    // Start the assignment block
    current_pos += sprintf(current_pos, "\n) {");

    // Generate nested assignments
    // Start with the innermost assignment
    current_pos += sprintf(current_pos, "(");
    for (int i = n_lines - 1; i >= 0; i--) {
        if (i < n_lines - 1) {
            current_pos += sprintf(current_pos, "(");
        }
        current_pos += sprintf(current_pos, "container_%d.val:=", n_lines - 1 - i);
    }
    current_pos += sprintf(current_pos, "69");

    // Close all parentheses
    for (int i = 0; i < n_lines; i++) {
        current_pos += sprintf(current_pos, ")");
    }
    current_pos += sprintf(current_pos, "}");

    return result;
}

int main(int argc, char* argv[]) {
    const int DEFAULT = 5000;
    int num_assignments = DEFAULT;

    // Parse command line argument if provided
    if (argc > 1) {
        num_assignments = atoi(argv[1]);
    }

    // Generate and print the result
    char* result = container_test(num_assignments);
    printf("%s\n", result);

    // Free allocated memory
    free(result);

    return 0;
}

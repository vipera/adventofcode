#include <stdio.h>

#include "schematic.h"

int main(int argc, char* argv[]) {
  struct schematic sh = { .fields = NULL, .columns = 0, .rows = 0,
    .global_id = 1, .total_numbers = 0 };
  if (argc != 2) {
    printf("You should provide a file argument.\n");
    return 1;
  }
  create_schematic(&sh, argv[1]);
  print_schematic_info(&sh);
  free_schematic(&sh);
  return 0;
}

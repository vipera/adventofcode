#ifndef _SCHEMATIC_H
#define _SCHEMATIC_H

#include <stdint.h>

typedef uint32_t field_id;

typedef struct schematic {
  struct field_def *fields;
  int columns;
  int rows;
  field_id global_id;
  /* number of numbers found */
  int total_numbers;
} schematic;

typedef enum field_type {
  TYPE_GEAR,
  TYPE_PART,
  TYPE_NUMBER,
  TYPE_SPACE,
  TYPE_END,
} field_type;

typedef struct field_def {
  field_id id;
  field_type type;
  char character;
  int value;
} field_def;

typedef struct fields_list {
  int *values;
  int n;
} fields_list;

struct field_def *get_field(const struct schematic *sh, int row, int column);

struct fields_list create_adjacency_list(const struct schematic *sh, int gears);
void free_adjacency_list(struct fields_list *list);

void create_schematic(struct schematic *sh, char *filename);
void print_schematic_info(struct schematic *sh);
void free_schematic(struct schematic *sh);

#endif

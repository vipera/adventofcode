#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "schematic.h"
#include "utils.h"

#define LINEBUFSIZE 1024

struct field_def *get_field(const struct schematic *sh, int row, int column) {
  if (row < 0 || row >= sh->rows || column < 0 || column >= sh->columns) {
    return NULL;
  }
  return &sh->fields[row * sh->columns + column];
}

struct field_def *get_field_by_id(const struct schematic *sh, field_id id) {
  int row;
  int col;
  struct field_def *field;
  if (id == 0) {
    return NULL;
  }
  for (row = 0; row < sh->rows; row++) {
    for (col = 0; col < sh->columns; col++) {
      field = get_field(sh, row, col);
      if (field->id == id) {
        return field;
      }
    }
  }
  return NULL;
}

struct fields_list create_adjacency_list(const struct schematic *sh, int gears) {
  struct fields_list list = { .values = NULL, .n = 0 };
  int col;
  int row;
  struct field_def *field;
  struct field_def *near_field;
  int di;
  int dj;
  int present;
  field_id surroundings[8] = {0};
  int si;
  int surroundings_n = 0;
  int gear_product = 1;

  /* allocate list large enough if all numbers are adjacent */
  list.values = (int *)malloc(sizeof(int) * sh->total_numbers);
  memset(list.values, 0, sizeof(int) * sh->total_numbers);

  for (row = 0; row < sh->rows; row++) {
    for (col = 0; col < sh->columns; col++) {
      field = get_field(sh, row, col);

      if (field->type == TYPE_PART || field->type == TYPE_GEAR) {
        /* search surroundings */
        for (di = -1; di <= 1; di++) {
          for (dj = -1; dj <= 1; dj++) {
            near_field = get_field(sh, row + di, col + dj);
            if (near_field && near_field->type == TYPE_NUMBER) {
              present = 0;
              for (si = 0; si < surroundings_n; si++) {
                if (surroundings[si] == near_field->id) {
                  present = 1;
                  break;
                }
              }
              if (!present) {
                surroundings[surroundings_n++] = near_field->id;
              }
            }
          }
        }

        /* copy local surroundings to list of values */
        for (si = 0; si < surroundings_n; si++) {
          near_field = get_field_by_id(sh, surroundings[si]);
          if (!gears) {
            list.values[list.n++] = near_field->value;
          } else {
            gear_product *= near_field->value;
          }
        }

        if (gears && field->type == TYPE_GEAR && surroundings_n > 1) {
          list.values[list.n++] = gear_product;
        }
        surroundings_n = 0;
        gear_product = 1;
      }
    }
  }

  return list;
}

void print_adjacency_info(
  const struct schematic *sh,
  const struct fields_list *list,
  const struct fields_list *gear_list
) {
  int i;
  int sum = 0;

  printf("Adjacent numbers found: %d\n", list->n);
  for (i = 0; i < list->n; i++) {
    sum += list->values[i];
  }
  printf("Sum of numbers: %d\n", sum);

  printf("Gear ratios found: %d\n", gear_list->n);
  sum = 0;
  for (i = 0; i < gear_list->n; i++) {
    sum += gear_list->values[i];
  }
  printf("Sum of gear ratios: %d\n", sum);
}

void free_adjacency_list(struct fields_list *list) {
  if (list->values) {
    free(list->values);
  }
  list->values = NULL;
}

enum field_type parse_field_type(char input) {
  if (input == '\0') {
    return TYPE_END;
  } else if (input >= '0' && input <= '9') {
    return TYPE_NUMBER;
  } else if (input == '.') {
    return TYPE_SPACE;
  } else if (input == '*') {
    return TYPE_GEAR;
  } else {
    return TYPE_PART;
  }
}

void parse_line(struct schematic *sh, char *line, int lineno) {
  int i;
  int number;
  int prev_start_i;
  int field_i;
  enum field_type type;
  enum field_type prev_type;
  struct field_def *field;

  for (i = 0; i < sh->columns + 1; i++) {
    type = parse_field_type(line[i]);

    if (i == 0) {
      prev_start_i = 0;
      prev_type = type;
    }

    if (type != prev_type) {
      if (prev_type == TYPE_NUMBER) {
        number = str2int(&line[prev_start_i], i - prev_start_i);
        sh->total_numbers++;
      }
      for (field_i = prev_start_i; field_i < i; field_i++) {
        field = get_field(sh, lineno, field_i);
        field->type = prev_type;
        field->id = sh->global_id;
        field->character = line[field_i];
        if (prev_type == TYPE_NUMBER) {
          field->value = number;
        }
      }

      prev_start_i = i;
      prev_type = type;
      sh->global_id++;
    }
  }
}

long open_schematic(const char *filename, FILE **fp) {
  long fsize;
  if (!(*fp = fopen(filename, "r"))) {
    printf("File does not exist or is not readable.\n");
    perror("fopen");
    exit(2);
  }

  fseek(*fp, 0, SEEK_END);
  fsize = ftell(*fp);
  fseek(*fp, 0, SEEK_SET);

  return fsize;
}

void create_schematic(struct schematic *sh, char *filename) {
  FILE *fp;
  char line[LINEBUFSIZE];

  long fsize;
  int lineno;

  fsize = open_schematic(filename, &fp);

  for (lineno = 0; fgets(line, LINEBUFSIZE - 1, fp) != NULL; lineno++) {
    if (!sh->columns) {
      replace_newline(line);
      sh->columns = strlen(line);
    }
    if (!sh->rows) {
      /* columns + 1 to account for line endings */
      sh->rows = (fsize + 1) / (sh->columns + 1);
    }
    if (!sh->fields) {
      sh->fields = (struct field_def *)
        malloc(sizeof(struct field_def) * sh->rows * sh->columns);
    }
    parse_line(sh, line, lineno);
  }

  fclose(fp);
}

char get_field_type_repr(struct field_def *field) {
  switch (field->type) {
    case TYPE_GEAR:
      return '*';
    case TYPE_PART:
      return '+';
    case TYPE_NUMBER:
      return field->character;
    case TYPE_SPACE:
      return ' ';
    case TYPE_END:
      return ' ';
  }
  return '?';
}

void print_schematic_info_header(struct schematic *sh) {
  printf("Schematic details:\n");
  printf("Columns: %d\n", sh->columns);
  printf("Rows: %d\n", sh->rows);
  printf("Total numbers found: %d\n", sh->total_numbers);
  printf("------------------\n");
}

void print_schematic_info(struct schematic *sh) {
  int row;
  int col;
  struct field_def *field;
  fields_list adjacent;
  fields_list adjacent_gears;

  print_schematic_info_header(sh);
  for (row = 0; row < sh->rows; row++) {
    for (col = 0; col < sh->columns; col++) {
      field = get_field(sh, row, col);
      printf("[%c]", get_field_type_repr(field));
    }
    printf("\n");
  }
  printf("\n");
  printf("------------------\n");
  adjacent = create_adjacency_list(sh, 0);
  adjacent_gears = create_adjacency_list(sh, 1);
  print_adjacency_info(sh, &adjacent, &adjacent_gears);
  free_adjacency_list(&adjacent);
  free_adjacency_list(&adjacent_gears);
}

void free_schematic(struct schematic *sh) {
  if (sh->fields) {
    free(sh->fields);
  }
  sh->fields = NULL;
}

#include <string.h>

#include "utils.h"

void replace_newline(char *line) {
  char *nl;
  nl = strchr(line, '\n');
  if (nl) {
    *nl = '\0';
  }
}

int str2int(const char *str, int len) {
  int i;
  int ret = 0;
  for (i = 0; i < len; i++) {
    ret = ret * 10 + (str[i] - '0');
  }
  return ret;
}
